##### Challenge - predict if a pokemon is legendary or not
# dataset from https://www.kaggle.com/rounakbanik/pokemon/downloads/pokemon.zip/1
#------------- Load packages ------------------

# Data wrangling
library(tidyverse)

# Machine Learning
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)

#------------- Inspect and correct data ------------------
pokedex <-  read_csv("./data/pokemon.csv")


# Examine the dataset
head(pokedex)
str(pokedex)

# change  variables into factors
factors <- c("type1", "type2", "generation", "is_legendary")

pokedex[factors] <- lapply(pokedex[factors], factor)

#-------------  EDA of legendary vs. non-legendary pokemons ------------------

# Check grouping of legendary pokemon
legendary_pokemon <- pokedex %>% 
  count(is_legendary) %>% 
  mutate(prop = n / nrow(pokedex))

legendary_pokemon

# Ploting weight vs. height

plot_weight <- pokedex %>% 
  ggplot(aes(x = weight_kg)) +
  geom_histogram()

plot_height_weight <- pokedex %>% 
  ggplot(aes(x = height_m, y = weight_kg)) +
  geom_point(aes(color = is_legendary), size = 1) +
  geom_text(aes(label = ifelse((height_m > 7.5 | weight_kg > 600), as.character(name), '')), 
            vjust = 0, hjust = 0) +
  geom_smooth(method = "lm", se = FALSE, col = "black", linetype = "dashed") +
  expand_limits(x = 16) +
  labs(title = "Pokemon by height and weight",
       x = "Height (m)",
       y = "Weight (kg)") +
  guides(color = guide_legend(title = "Pokémon status")) +
  scale_color_manual(labels = c("Non-Legendary", "Legendary"),
                     values = c("#F8766D", "#00BFC4"))

plot_height_weight

# Plot by type
# Prepare the data
legend_by_type <- pokedex %>% 
  group_by(type1) %>% 
  mutate(is_legendary = as.numeric(is_legendary) - 1) %>% 
  summarise(prop_legendary = mean(is_legendary)) %>% 
  ungroup() %>% 
  mutate(type = fct_reorder(type1, prop_legendary))

# Prepare the plot
legend_by_type_plot <- legend_by_type %>% 
  ggplot(aes(x = type, y = prop_legendary, fill = prop_legendary)) + 
  geom_col() +
  labs(title = "Legendary Pokemon by type") +
  coord_flip() +
  guides(fill = FALSE)

# Print the plot
legend_by_type_plot

# Compare with some boxplots between variables

#Prepare the data
legend_by_stats <- pokedex  %>% 
  select(is_legendary, attack, sp_attack, defense, sp_defense, hp, speed)  %>% 
  gather(key = fght_stats , value = value, -is_legendary) 

# Prepare the plot
legend_by_stats_plot <- legend_by_stats %>% 
  ggplot(aes(x = is_legendary, y = value, fill = is_legendary)) +
  geom_boxplot(varwidth = TRUE) +
  facet_wrap(~fght_stats) +
  labs(title = "Pokemon fight statistics",
       x = "Legendary status") +
  guides(fill = "legend")

# Print the plot
legend_by_stats_plot

#-------------  Machine Learning ------------------

# split data
# Set seed for reproducibility
set.seed(1234)

# Save number of rows in dataset
n <- nrow(pokedex)

# Generate 60% sample of rows
sample_rows <- sample(n, 0.6*n)

# Create training set
pokedex_train <- pokedex  %>% 
  filter(row_number() %in% sample_rows)

# Create test set
pokedex_test <- pokedex  %>% 
  filter(!row_number() %in% sample_rows)


# Fit decision tree
model_tree <- rpart(is_legendary ~ attack + defense + height_m + 
                      hp + sp_attack + sp_defense + speed + type1 + weight_kg,
                    data = pokedex_train,
                    method = "class",
                    na.action = na.omit)

# Plot decision tree
rpart.plot(model_tree)

# Load package and set seed
set.seed(1234)

# Fit random forest
model_forest <- randomForest(is_legendary ~ attack + defense + height_m + 
                               hp + sp_attack + sp_defense + speed + type1 + weight_kg,
                             data = pokedex_train,
                             importance = TRUE,
                             na.action = na.omit)

# Print model output
model_forest

# Print ROC of models

# Create prediction and performance objects for the decision tree
probs_tree <- predict(model_tree, pokedex_test, type = "prob")
pred_tree <- prediction(probs_tree[,2], pokedex_test$is_legendary)
perf_tree <- performance(pred_tree, "tpr", "fpr")

# Create prediction and performance objects for the random forest
probs_forest <- predict(model_forest, pokedex_test, type = "prob")
pred_forest <- prediction(probs_forest[,2], pokedex_test$is_legendary)
perf_forest <- performance(pred_forest, "tpr", "fpr")

# Plot the ROC curves: first for the decision tree, then for the random forest
plot(perf_tree , col = "red", main = "ROC curves")
plot(perf_forest, add = TRUE, col = "blue")
legend(x = "bottomright",  legend = c("Decision Tree", "Random Forest"), fill = c("red", "blue"))

# Plot Importance
# Print variable importance measures
importance_forest <- importance(model_forest)
importance_forest

# Create a dotchart of variable importance
varImpPlot_forest <- varImpPlot(model_forest)
varImpPlot_forest
