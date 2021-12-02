knitr::opts_chunk$set(echo = TRUE, 
                      eval = FALSE)
library(dplyr) # here comes "%>%" from ...
library(here) # to find pfad of files command here('covid19-forecast-hub-europe', arg1)
library(stringr) 
library(data.table)
library(scoringutils) #eval_forecasts(summarise_by = c("target_type", "model")
                    #comes from here and would return scores with 'target_type'
                      #and 'model' as two first columns.
library(purrr)
library(tidyr)
#This data includes all forecasts 
#as well as the true observations.
uk_data <- fread("data/full-data-uk-challenge.csv")
#head(uk_data)

#The file contains submitted forecasts by diff. Instits.
#as well as true observations
hub_data <- rbindlist(
  list(
    fread("data/full-data-european-forecast-hub-1.csv"), 
    fread("data/full-data-european-forecast-hub-2.csv")
  )
)
## loading truth data using the package: 'covidHubutils' 
devtools::install_github("reichlab/covidHubUtils")

#The symbol 'x::y' is for defining a function over x and y
# and appending '|> f()' is to beginn to define 'f'.   

truth <- covidHubUtils::load_truth(hub = "ECDC") |>
  filter(target_variable %in% c("inc case", "inc death")) |>
  mutate(target_variable = ifelse(target_variable == "inc case", 
                                  "Cases", "Deaths")) |>
  rename(target_type = target_variable, 
         true_value = value) |>
  select(-model)

#head(truth)

fwrite(truth, "data/weekly-truth-Europe.csv") #that is uploading
                                              #into R Environment


# Setting correct file-paths to all forecasts
folders <- here("data-processed", list.files("data-processed"))
folders <- folders[
  !(grepl("\\.R", folders) | grepl(".sh", folders) | grepl(".csv", folders))
]

file_paths <- purrr::map(folders, 
                         .f = function(folder) {
                           files <- list.files(folder)
                           out <- here::here(folder, files)
                           return(out)}) %>%
  unlist()
file_paths <- file_paths[grepl(".csv", file_paths)]
# Setting correct file-paths to all past forecasts 
# with helper-function 'get_model_name' 
# to get model name from a file path
get_model_name <- function(file_path) {
  split <- str_split(file_path, pattern = "/")[[1]]
  model <- split[length(split) - 1]
  return(model)
}

# loading forecasts
      ## her you see the same '::' and '%>%' princip... but
  
#######
######
#### object 'target' nicht gefunden.
####
#### to verifiy!
###
# 'prediction_data will return forecasts from true 'file_paths'
prediction_data <- map_dfr(file_paths, 
                           .f = function(file_path) {
                             data <- fread(file_path)
                             data[, `:=`(
                               target_end_date = as.Date(target_end_date),
                               quantile = as.numeric(quantile),
                               forecast_date = as.Date(forecast_date), 
                               model = get_model_name(file_path)
                             )]
                             return(data)
                           }) %>%
  filter(grepl("case", target) | grepl("death", target)) %>%
  mutate(target_type = ifelse(grepl("death", target), 
                              "Deaths", "Cases"), 
         horizon = as.numeric(substr(target, 1, 1))) %>%
  rename(prediction = value) %>%
  filter(type == "quantile", 
         grepl("inc", target)) %>%
  select(location, forecast_date, quantile, prediction, 
         model, target_end_date, target, target_type, horizon)
# merge forecast data and truth data and save
hub_data <- merge_pred_and_obs(prediction_data, truth, 
                               by = c("location", "target_end_date", 
                                      "target_type")) |>
  filter(target_end_date >= "2021-01-01") |>
  select(-location_name, -population, -target) 
# split forecast data into two to reduce file size
split <- floor(nrow(hub_data) / 2)

fwrite(hub_data[1:split, ], 
       file = "data/full-data-european-forecast-hub-1.csv")
fwrite(hub_data[(split + 1):nrow(hub_data), ], 
       file = "data/full-data-european-forecast-hub-2.csv")
#################################
### All that above was about Iteratively sample all possible combinations.
###############################
###To implement Enssemble-mean:
#First define mean-Ensemble
mean_ensemble <- hub_data %>%  ##dplyr now imports %>% from 
                                #magrittr and uses it by default
                                #to create the standard-output
                              #of 'hub_data' by mean-summarization as 'mean_ensemble'
                    #See ( https://cran.r-project.org/web/packages/magrittr/vignettes/magrittr.html )            
  group_by(location, target_type, target_end_date, true_value, horizon, quantile) %>%
  summarise(prediction = mean(prediction)) %>% ##Note that every additional
                                  ## pipe symbol '%>%' introduce
                              ##a new subset of definition for the function called.
  mutate(model = "mean-ensemble")
#hier you see the difference btw 'mean_ensemble' and 'hub_data'
tail(mean_ensemble)
tail(hub_data)

##The score_mean should return the summarized expected forecasts values 
##by mean method (scores_mean=eval_foracsts | mean_ensemble)
scores_mean <- mean_ensemble %>%
  eval_forecasts(summarise_by = c("target_type", "model"))
#to print all mean-scores:
scores_mean

##@ do the same for median-scores with summarize.'prediction = median(prediction)'
median_ensemble <- hub_data %>%
  group_by(location, target_type, target_end_date, horizon, true_value, quantile) %>%
  summarise(prediction = median(prediction)) %>%
  mutate(model = "median-ensemble")
scores_median <- median_ensemble %>%
  eval_forecasts(summarise_by = c("target_type", "model"))
#to print all median-scores:
scores_median

#############Evaluating performance depending 
####################on the number of member models#########
###########N
#first creating mean-ensemble *Functions*: This will be usefull for the 
# scores-ensembles at the end
create_mean_ensemble <- function(data, members) {
  data %>%
    filter(model %in% members) %>%
    group_by(location, target_end_date, horizon, true_value, target_type, quantile) %>%
    summarise(prediction = mean(prediction)) %>%
    mutate(model = "mean-ensemble")
}

score_forecasts <- function(data) {
  score <- data %>%
    eval_forecasts(summarise_by = c("target_type", "model"))
  return(score$interval_score)
}

models_germany <- hub_data %>%
  filter(location == "DE", 
         model != "") %>%
  pull(model) %>%
  unique()   ##unique(x) where x can be a vector, 
            #a data frame or a matrix. 
            #Bsp: An input vector having duplicate values
              #df<-c(1,2,3,2,4,5,1,6,8,9,8,6)
              #elimnates the duplicate values in the vector 
            #unique(df)
            #Output = 1 2 3 4 5 6 8 9
##To see List
models_germany
n <- 5
#list(1,2,3)
models <- list()
models[[1]] <- sample(models_germany, size = 5) 
models[[2]] <- sample(models_germany, size = 5) ##alternatively picking 
                                      #five entries in 'models_germany'
##To all possible combis?

scores <- list()

data_germany <- hub_data %>%
  filter(location == "DE")

for (i in 1:length(models)) {
  ensemble <- create_mean_ensemble(data_germany, members = models[[i]])
  scores[[i]] <- score_forecasts(ensemble)
}
#print list of scores
scores


