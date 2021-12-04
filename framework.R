# Framework file separate from Readme

#### Mean ensemble (untrained)

# Quantiles of the predictive distribution are computed as an unweighted mean of the corresponding quantiles of all member models. E.g. the 80%-quantile of the ensemble is the mean of all 80% quantiles of all preditive distributions.
# 
# Simple example for a mean ensemble: 
  

mean_ensemble <- hub_data %>%
  group_by(location, target_type, target_end_date, true_value, horizon, quantile) %>%
  summarise(prediction = mean(prediction)) %>%
  mutate(model = "mean-ensemble")

scores_mean <- mean_ensemble %>%
  eval_forecasts(summarise_by = c("target_type", "model"))



#### Median ensemble (untrained)
# 
# Quantiles of the predictive distribution are computed as the mean of the corresponding quantiles of all member models. 
# 
# Simple example for a median ensemble: 

median_ensemble <- hub_data %>%
  group_by(location, target_type, target_end_date, horizon, true_value, quantile) %>%
  summarise(prediction = median(prediction)) %>%
  mutate(model = "median-ensemble")

scores_median <- median_ensemble %>%
  eval_forecasts(summarise_by = c("target_type", "model"))



#####

# helper function and the basis for our framework 

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
  
  out <- score |>
    select(target_type, interval_score) |>
    tidyr::pivot_wider(names_from = target_type, values_from = interval_score) 
  
  return(out)
}

# example with data from Germany
# filtering not really needed for a complete data set, but left hear 
# for non-complete data sets
models_germany <- hub_complete %>%
  filter(location == "DE", 
         model != "") %>%
  pull(model) %>%
  unique()

# do for n = 1, ..., N  
n <- 5

# get all possisble combinations for n
models <- combn(models_germany, m = n) |>
  as.data.frame() |>
  as.list()

scores <- list()

data_germany <- hub_data %>%
  filter(location == "DE")

for (i in 1:length(models)) {
  ensemble <- create_mean_ensemble(data_germany, members = models[[i]])
  scores[[i]] <- score_forecasts(ensemble)
}

scores |>
  bind_rows() |>
  summarise(Cases = mean(Cases), 
            Deaths = mean(Deaths))

