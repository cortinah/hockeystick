# Load necessary libraries
library(tidymodels)

# Load the data
data(iris)

set.seed(42)

# Split the data (no stratification, to match the Python version)
iris_split <- initial_split(iris, prop = 0.9)
train_data <- training(iris_split)
test_data  <- testing(iris_split)

# Define logistic regression model
log_reg_spec <- multinom_reg() |>
  set_engine("nnet") |>   # multinomial logistic regression
  set_mode("classification")

# Create workflow (formula = Species ~ . )
iris_workflow <- workflow() |>
  add_model(log_reg_spec) |>
  add_formula(Species ~ .)

# Fit the model on training data
iris_fit <- fit(iris_workflow, data = train_data)

# Predict on test data
predictions <- predict(iris_fit, new_data = test_data) |>
  bind_cols(test_data)

# Evaluate accuracy
metrics <- predictions |>
  metrics(truth = Species, estimate = .pred_class)

# Classification report equivalent: confusion matrix
conf_mat <- predictions |>
  conf_mat(truth = Species, estimate = .pred_class)

# Print results
print(metrics)
print(conf_mat)
