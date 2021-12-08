# Code nikos fragen

##

plot_locs_per_model <- function(data) {
  data |>
    group_by(model, forecast_date) |>  # Group the data by
    mutate(n = length(unique(location))) |>  # corrects n to the actual number of countries. 
    # how does actually mutate counts the number of countries considered by each model just by using this 
    # command? If used alone, it returns just 32 i.e the number of locs. 
    ggplot(aes(y = reorder(model, n), x = as.Date(forecast_date), fill = n)) + 
    geom_tile() + 
    facet_wrap(~ target_type) + 
    labs(y = "Location", x = "Forecast date")
}

## 
mean_ensemble <- hub_data %>%
  group_by(location, target_type, target_end_date, true_value, horizon, quantile) %>%
  summarise(prediction = mean(prediction)) %>%  #Q2 Nikos: How does exactly this summarize function here works?
  mutate(model = "mean-ensemble")


##

## Calculating Scores
score_forecasts <- function(data) {
  score <- data %>%
    eval_forecasts(summarise_by = c("target_type", "model"))
  
  out <- score |>
    select(target_type, interval_score) |>
    tidyr::pivot_wider(names_from = target_type, values_from = interval_score) # Q3. Please elaborate on this 
  return(out)
}



