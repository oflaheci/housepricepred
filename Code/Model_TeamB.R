##############################
# Final model script: Team B #
##############################

### Note: fill in the code below until the line.
### Make sure your model works as expected by running summary().

# Load any packages here
# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# load necessary packages
lapply(c("ggplot2", "stargazer", "tidyverse", "stringr", "broom"),  pkgTest)

# Load training data
train <- readRDS("data/train.rds")

# Data transformation
train <- train %>% 
  # Add here any code necessary to transform variables
  
  # Model
  mod <- lm(# your model formula here
    , 
    data = dat,
    na.action = na.omit)

summary(mod)

########## do not fill in below the line ###########

# Load test data
test <- readRDS("data/test.rds")

# Transform test data
test <- test %>% 
  # I will copy/paste here the code you use above
  
  # Run model on test data
  test$prediction <- predict(mod, newdata = test)

# Calculate RMSE
test$residuals <- test$AdjSalePrice - test$prediction
(rmse <- sqrt(mean(test$residuals^2)))

# Calculate R^2
mean_y <- mean(test$AdjSalePrice)
tss <- sum((test$AdjSalePrice - mean_y)^2)
rss <- sum((test$AdjSalePrice - test$prediction)^2)
(r_sq <- 1 - (rss/tss))