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
train <- readRDS("../Data/train.rds")

# Data transformation

#exclude outliers
outliers <- c(15204, 8893, 12591, 4465)

train <-train[-match(outliers,train$ID),]

#add zipgroup
train %>%
  group_by(ZipCode) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ggplot(aes(as.factor(reorder(ZipCode, n)), n)) +
  geom_col() +
  coord_flip() +
  xlab("Zip Code")

zip_group <- train %>%
  group_by(ZipCode) %>%
  summarise(med_price = median(AdjSalePrice),
            count = n()) %>%
  arrange(med_price) %>%
  mutate(cumul_count = cumsum(count),
         ZipGroup = ntile(cumul_count, 5))

train <- train %>%
  left_join(select(zip_group, ZipCode, ZipGroup), by = "ZipCode")


# Model
mod <- lm(AdjSalePrice ~ 
              SqFtTotLiving + I(SqFtTotLiving^2) + 
              BldgGrade + 
              ZipGroup + I(ZipGroup), 
            data = train, 
    na.action = na.omit)

stargazer(mod, type = "text")
  
par(mfrow = c(2, 2)) # we change the graphic device to show 4 plots at once
plot(mod) # we supply our lm object to plot()
  
  
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