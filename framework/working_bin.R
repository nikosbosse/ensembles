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

# Test PIT
median_tv <- as.vector(median_ensemble$true_value)
median_pred <- as.matrix(median_ensemble$prediction)
pit(median_tv, median_pred)



######## example with data from Germany
# filtering not really needed for a complete data set, but left here 
# for non-complete data sets
models_germany <- hub_data %>%
  filter(location == "DE", 
         model != "") %>%
  pull(model) %>%
  unique()

# do for n = 1, ..., N  
n <- 2

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

#scores_orginal <- scores

scores |>
  bind_rows() |>
  summarise(Cases = mean(Cases), 
            Deaths = mean(Deaths))


mean_ensemble <- data_germany %>%
  group_by(location, target_type, target_end_date, horizon, true_value, quantile) %>%
  summarise(prediction = mean(prediction)) %>%
  mutate(model = "median-ensemble")
