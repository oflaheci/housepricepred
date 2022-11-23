####################################
# Zipcode: clustering on residuals #
####################################

# load packages
library(tidyverse)
library(broom)
library(stargazer)
library(car)
# set working directory to current parent folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load data
dat <- readRDS("../Data/train.rds")

#----------------------
#from tutorial

mod1 <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + Bedrooms + BldgGrade + PropertyType, data = dat)
dat <- cbind(dat, residuals = resid(mod1))

zip_group_res <- dat %>%
  group_by(ZipCode) %>%
  summarise(med_price = median(residuals),
            count = n()) %>%
  arrange(med_price) %>%
  mutate(cumul_count = cumsum(count),
         ZipGroup_r = ntile(cumul_count, 5))
dat <- dat %>%
  left_join(select(zip_group_res, ZipCode, ZipGroup_r), by = "ZipCode")

mod2 <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + Bedrooms + BldgGrade + PropertyType + as.factor(ZipGroup_r), data = dat)

stargazer(mod1, mod2, type = "text")


#library?
plot_ly(data = dat, z = ~AdjSalePrice, x = ~SqFtTotLiving, y = ~ZipGroup_r, 
        color = ~as.factor(ZipGroup_r))


mod3 <- lm(AdjSalePrice ~ SqFtTotLiving, data = dat) # bivariate regression model

#base r - model line
scatter.smooth(dat$SqFtTotLiving, resid(mod3), # plot a smooth line on the scatter plot
               lpars = list(col = "blue", lwd = 3, lty = 3), 
               main = "Residual Plot (Sale Price ~ Size)",
               xlab = "Total Living Area (sq.ft.)",
               ylab = "Residuals")
abline(h = 0, col = "red") # plot a horizontal line through zero

# added variable plots
par(mfrow = c(1,1))
avPlot(mod2, variable = "SqFtTotLiving")


avPlot(mod2, variable = "BldgGrade")
# partial predictor plot
terms <- predict(mod2, type = "terms") # extract the individual regression terms from our model for each observation

partial_resid <- resid(mod2) + terms # add the individual regression terms to the residual for each observation

df <- data.frame(SqFtTotLiving = dat[, "SqFtTotLiving"], # create a new data.frame of these vals
                 Terms = terms[, "SqFtTotLiving"],
                 PartialResid = partial_resid[, "SqFtTotLiving"])

# plot line of sqftliving to compare to lm model
# sqftliving is not straight line
ggplot(df, aes(SqFtTotLiving, PartialResid)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  geom_line(aes(SqFtTotLiving, Terms), colour = "red")




mod4 <- lm(AdjSalePrice ~ SqFtTotLiving + 
             I(SqFtTotLiving^2) + SqFtLot + 
             Bathrooms + Bedrooms + BldgGrade + 
             PropertyType + as.factor(ZipGroup_r), 
           data = dat)

stargazer(mod1, mod2, mod4, type = "text")

# plot polynomial
terms_poly <- predict(mod4, type = "terms") # extract the individual regression terms from our model for each observation

partial_resid_poly <- resid(mod4) + terms_poly # add the individual regression terms to the residual for each observation

df_poly <- data.frame(SqFtTotLiving = dat[, "SqFtTotLiving"], # create a new data.frame of these vals
                      Terms = terms_poly[, "I(SqFtTotLiving^2)"],
                      PartialResid = partial_resid_poly[, "I(SqFtTotLiving^2)"])

ggplot(df_poly, aes(SqFtTotLiving, PartialResid)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  geom_line(aes(SqFtTotLiving, Terms), colour = "red")


#using model plots to id outliers
par(mfrow = c(2, 2)) # we change the graphic device to show 4 plots at once
plot(mod4) # we supply our lm object to plot()

#Another way to detect outliers is by examining standardised residuals (the residual divided by the standard error of the residuals). We can do this in R as follows:
sresid <- rstandard(mod4) # the rstandard() function extracts standardised residuals
index <- order(sresid) # make an index of standardised residuals
dat[index[1:5], c("AdjSalePrice", "SqFtTotLiving", "SqFtLot", "Bathrooms", "Bedrooms", "BldgGrade")]
#better for finding outliers on RHS
#-------------------


# run a model
mod1 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade, data = dat)

# we can access the residuals of the model using the resid() function
resid(mod1)

# our initial code for clustering on AdjSalePrice
zip_group_pr <- dat %>%
  group_by(ZipCode) %>%
  summarise(med_price = median(AdjSalePrice), # we need to change this
            count = n()) %>%
  arrange(med_price) %>%
  mutate(cumul_count = cumsum(count),
         ZipGroup = ntile(cumul_count, 5))

dat <- dat %>%
  left_join(select(zip_group_pr, ZipCode, ZipGroup), by = "ZipCode")

# bind residuals to data
dat <- cbind(dat, residuals = resid(mod1))

# code for clustering on residuals
zip_group_res <- dat %>%
  group_by(ZipCode) %>%
  summarise(med_price = median(residuals), # we need to change this
            count = n()) %>%
  arrange(med_price) %>%
  mutate(cumul_count = cumsum(count),
         ZipGroup_r = ntile(cumul_count, 5))

dat <- dat %>%
  left_join(select(zip_group_res, ZipCode, ZipGroup_r), by = "ZipCode")

# compare the two approaches...
# as a constant
mod2 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + ZipGroup, data = dat)
mod3 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + ZipGroup_r, data = dat)
stargazer(mod1, mod2, mod3, type = "text")

## note: when adding zipcode as a constant, we need to be careful interpreting
## our constant, as there is no house in our data without zero zipcode effect.
## (The same would go for building grade).

# as categories
mod4 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + as.factor(ZipGroup), data = dat)
mod5 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + as.factor(ZipGroup_r), data = dat)
stargazer(mod1, mod4, mod5, type = "text")

## note: when modelled as a category, the constant now includes the effect of 
## the reference category.

# Which zipcodes are in each category?
# dplyr approach: group_by two variables
dat %>%
  group_by(ZipGroup, ZipCode) %>%
  summarise(n = n())

# make a list using a for loop :(
ZipCode <- list(NULL)
for (i in seq_along(unique(dat$ZipGroup))) {
  ZipCode[[i]] <- unique(dat$ZipCode[dat$ZipGroup == i])
}
