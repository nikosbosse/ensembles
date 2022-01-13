# Framework file separate from Readme
## Packages
library(dplyr)
library(here)
library(stringr)
library(data.table)
library(scoringutils)
library(purrr)
library(tidyr)
library(ggplot2)
library(readr)

## Data (list all available data)

#hub_data <- read_csv("data/hub_comp.csv")
hub_complete <- read_csv("data/hub_complete_loc.csv")
#hub_complete_loc <- read_csv("data/hub_complete_loc.csv")


## Creating Ensembles
create_mean_ensemble <- function(data, members) {
  data %>%
    filter(model %in% members) %>%
    group_by(location, target_end_date, horizon, true_value, target_type, quantile) %>%
    summarise(prediction = mean(prediction)) %>%
    mutate(model = "mean-ensemble") 
}

## Calculating Scores
score_forecasts <- function(data) {
  score <- data %>%
    eval_forecasts(summarise_by = c("target_type", "model"))
  
  out <- score |>
    select(target_type, interval_score) |>
    tidyr::pivot_wider(names_from = target_type, values_from = interval_score) # Please elaborate on this 
  
  return(out)
}



