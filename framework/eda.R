# View Data
options(scipen=999)
View(hub_data)
str(hub_data)
summary(hub_data)

## Location
table(hub_data$location)
str(levels(as.factor(hub_data$location)))
str(levels(as.factor(hub_data$target_type)))

table(hub_data$location)
str(levels(as.factor(hub_data$location))) # 32 locations.  List in Github
plot(table(hub_data$location))


## Target Type
table(hub_data$target_type) # Absolute frequencies of cases and deaths. Both in the hundredthousends. 
str(levels(as.factor(hub_data$target_type))) # 2 target types: cases and deaths 

# n
str(levels(as.factor(hub_data$n))) # n = 4. what does this mean?

# Models
table(hub_data$model) # Absolute frequencies of model logs 
str(levels(as.factor(hub_data$model))) # Number of models: 33
levels(as.factor(hub_data$model)) # List of models (Names)
models <- data.frame(table(hub_data$model))
models <- arrange(models, desc(freq))  # Arrange models in descending order. 
colnames(models) <- c("models", "freq")

ggplot(models, aes(models, freq))+  # plot model frequency
  geom_point()




