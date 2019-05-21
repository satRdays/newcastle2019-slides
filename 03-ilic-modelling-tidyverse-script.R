library(tidyverse)
library(tidymodels)

# Load
autompg <- read_csv("auto-mpg.csv") %>%
  select(-car_name)

#--------------------------------- Splitting ----------------------------------#
# Package: rsample

set.seed(42)
auto_split <- initial_split(autompg, prop = 0.8, strata = "mpg")
auto_train <- training(auto_split)
auto_test <- testing(auto_split)

#--------------------------------- Preprocessing ------------------------------#
# Package recipes 

# 
# The recipes package rigidly follows a cooking analogy for applying preprocessing 
# steps, which is totally analogous to how we fit a model. The three stages are:
# 
# 1.	recipe: Define a specification for preprocessing steps.  none of the 
# actual data.
# 
# 2.	prep:  For a recipe with at least one preprocessing operation, estimate the 
# required parameters from a training set that can be later applied to other data sets.
# 
# 3.	bake: Apply the recipe 
rec_auto <- recipe(mpg ~ ., data = auto_train) %>%
  step_other(make, threshold = 0.05) %>%  # move infrequently occurring values into the "other" category.
  step_dummy(origin, make) %>%            # deal with categories
  step_knnimpute(all_predictors()) %>%    # impute missing data using nearest neighbors
  prep(auto_train)                        # prepare the recipe



# bake 
#Apply the recipe on the training set
auto_train <- bake(rec_auto, new_data = auto_train) 

#Apply the recipe on the test set
auto_test <- bake(rec_auto, new_data = auto_test)

# ------------------Create a model , choose the enfine and fit------------------#
# Package parsnip

model <- boost_tree() %>%
  set_engine("xgboost") %>%
  fit(mpg ~ ., data = auto_train)

# the algorithm and the computational machine are detached
# the data do not enter the play untill the fit step
#model <- linear_reg(penalty = 0.01) %>%
#  set_engine("glmnet") %>%
#  fit(mpg ~ ., data = auto_train)

# -------------------------------------- Predict-------------------------------#
# parsnip  model ALWAYS returns the results as a data frame 


predictions <- bind_cols(
  select(auto_test, mpg),
  predict(model, auto_test)
)

# -------------------Score Get the metrics ------------------------------------#
# And here comes yardstick
# When "truth" is a factor: accuracy and the Kappa
# When "truth" is numeric: rmse, rsq and mae.


metrics(predictions, truth = mpg, estimate = .pred)
