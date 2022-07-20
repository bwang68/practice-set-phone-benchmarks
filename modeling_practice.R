### supervised modeling
# visualizing data

iris %>% 
  ggplot(aes(Sepal.Length, Sepal.Width)) +
  geom_point()

cor(iris$Sepal.Length, iris$Sepal.Width) # gives correlation
cor(iris$Petal.Length, iris$Petal.Width) # this cor() gives a strong positive

#ggplot to visualize the pos cor
iris %>% 
  ggplot(aes(Petal.Length, Petal.Width)) +
  geom_point()

iris %>% 
  ggplot(aes(Petal.Length, Sepal.Length)) +
  geom_point()

iris %>% 
  ggplot(aes(Petal.Length, Sepal.Width)) +
  geom_point()

cor(iris$Petal.Length, iris$Sepal.Length)
cor(iris$Petal.Length, iris$Sepal.Width)
cor(iris$Petal.Length, iris$Petal.Width)

# for regression we chooose one variable we want to predict
# then we figure out which variables we want to use 
# to predict that one variable
# the vars we want to use to predict that one said var, petal length in this case,
# usually has high correlation values

# so we will use Petal.Width, Sepal.Length, and maybe Sepal.Width
# these are highest to lowest correlation

# split into training, test, and validation sets
# so but first example of using the rep() func
greetings <- c(rep("hello", 5), rep("goodbye", 3)) %>% 
  sample(size = 8, replace = FALSE) # replace = false means that as soon as one value is used, it cant be reused
greetings


# now with irisset
iris_data_len <- nrow(iris)
iris$label <- c(rep("training", ceiling(0.6*iris_data_len)), 
                rep("test", ceiling(0.2*iris_data_len)),
                rep("validation", ceiling(0.2*iris_data_len))) %>% 
  sample(150, replace = FALSE) # sample to randomize and get exact # of labels

# a thing to note here is that percentages can lead to half a observation, which we dont want, we can round up so it gives us whole numbers as the number in each group: training, testing, validation

# it is also ok to have a few extra, which is why we round up, but noT ok to have less, cuz then u might end up with rows that dont have a label

# so now that we have assigned all the rows to a group in our process, we can move onto the next step


### choosing a model!
## when using a model, we "train" it using the training set, and 
## "test" it using the testing set
iris_train <- iris %>% 
  filter(label == "training")
iris_test <- iris %>% 
  filter(label == "test")
iris_valid <- iris %>% 
  filter(label == "validation")
#start w linear model
iris_lm <- lm(Petal.Length ~ Petal.Width + Sepal.Length, data = iris_train)
summary(iris_lm) 

# select out x vals from test set for model to predict (petal width and sepal length)

iris_lm_predictions <- iris_test %>% 
  select(Petal.Width, Sepal.Length) %>% 
  predict(object = iris_lm) #object is our linear model

iris_train$lm_pred <- iris_lm_predictions

head(iris_train) # looking at the pred vs petal length its kinda a little off somehow but we can test what happens if we look at only one variable or more variables

iris_lm <- lm(Petal.Length ~ Petal.Width + Sepal.Length + Sepal.Width, data = iris_train)
iris_lm

# select out x vals from test set for model to predict (petal width and sepal length)

iris_lm_predictions <- iris_test %>% 
  select(Petal.Width, Sepal.Length, Sepal.Width) %>% 
  predict(object = iris_lm) #object is our linear model

iris_test$lm_pred <- iris_lm_predictions

head(iris_test)


## logistic model

# two options we have is to predict two species
# predict by creating a new cateogry long or short pedal


iris_train_glm <- iris_train %>% 
  mutate(petal_length_cat = as.factor(ifelse(Petal.Length < 3.758, "short", "long"))) # we got 3.758 by just using the mean petal length of iris
# we also use as factor because binomial catogries have to be factors

iris_glm <- glm(petal_length_cat ~ Petal.Width + Sepal.Length + Sepal.Width,
                data = iris_train_glm, family = binomial(link = "logit"))

iris_glm_preds <- iris_test %>% 
  select(Petal.Width, Sepal.Length, Sepal.Width) %>% 
  predict(object = iris_glm)

summary(iris_glm)

iris_test$glm_pred <- iris_glm_preds
iris_test <- iris_test %>% 
  mutate(petal_length_cat = as.factor(ifelse(Petal.Length < 3.758, "short", "long")))

# in glm_pred col from iris_test, the negative and positive correpond to large or short, the magnitude of the values in that col
# mean how sure the model is of that prediction, the bigger magnitude, the more sure it is


# filter down to 2 categories
iris_train_2species <- filter(iris_train, Species %in% c("setosa", "virginica"))

# create model

iris_glm <- glm(Species ~ Sepal.Width + Sepal.Length + Petal.Width + Petal.Length, data = iris_train_2species, family = binomial(link = "logit"))

summary(iris_glm)

# mcreate test set with only 2 species, cuz we cant append when iris_test has 3 species, so make new set
iris_test_2species <- iris_test %>% 
  filter(Species %in% c("setosa", "virginica"))

# make predictions based on model
iris_2species_preds <- iris_test_2species %>% 
  select(-Species) %>% 
  predict(object = iris_glm)

# append predictions to test set
iris_test_2species$glm_2spec_pred <- iris_2species_preds

# GRADIENT BOOOSTIN MACHINE
# technically its called generalized boosted regression modeling
# boosted means its making its own models and its own corrections

library(gbm)

#in this case, the order of ur indepnt vars dont matter as much bc it makes its own trees


iris_gbm <- gbm(Petal.Length ~ Petal.Width + Sepal.Length + Sepal.Width + Species, 
                data = iris, n.cores = 4, 
                n.trees = 500) # we using all all iris data bc the way we split our data is, there isnt enough data for the model so we need more data

#select out only the x values we used from test and predict
iris_gbm_preds <- iris_test %>% 
  select(Petal.Width, Sepal.Length, Sepal.Width, Species) %>% 
  predict(object = iris_gbm)

summary(iris_gbm) # this will give cool info saying which 
                  # variables wil infleunce the model the most
                  # which can also u can use to help ur other
                  # models

# save predictions back into test set

iris_test$gbm_pred <- iris_gbm_preds

#eval perf of models
View(iris_test)
library(Metrics)

# calculate RMSE between predictiosn and true values
rmse(iris_test$Petal.Length, iris_test$lm_pred) 

rmse(iris_test$Petal.Length, iris_test$gbm_pred) # wins, smaller error
# truth is that, sometimes simpler is better 


# calculate MAE between predictions and true values
mae(iris_test$Petal.Length, iris_test$lm_pred) 
mae(iris_test$Petal.Length, iris_test$gbm_pred) # wins, smaller error

## accuracy and f1 of models

# accuracy
# looking at long or short pedal
iris_test <- iris_test %>% 
  mutate(glm_petal_cat = ifelse(glm_pred < 0, "long", "short"))

true_vals <- sum(iris_test$glm_petal_cat == iris_test$petal_length_cat)
total_vals <- nrow(iris_test)

accuracy <- true_vals/total_vals # 1! great acc
accuracy

# f1 score tells us about false positive & false negative rates
f1(iris_test$glm_petal_cat, iris_test$petal_length_cat)
# high f1 = good model

