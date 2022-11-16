
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# load necessary packages
lapply(c("ggplot2", "stargazer", "tidyverse", "stringr", "broom"),  pkgTest)

# function to save output to a file that you can read in later to your docs
output_stargazer <- function(outputFile, appendVal=FALSE, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=appendVal)
}

# set working directory to current parent folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# categories to intervals
dat <- readRDS("data/train.rds")
hist(dat$BldgGrade)

sort(unique(dat$BldgGrade))

with(dat, boxplot(AdjSalePrice ~ BldgGrade))
#1 is a cabin, 2 is substandard, 5 is fair, 10 is very good, 12 is luxury and 
#13 is a mansion.

# -----------------------------------------------------
# ordered factor

mod3 <- lm(dat$AdjSalePrice ~ as.ordered(dat$BldgGrade))
stargazer(mod3, type = "text")
# L - linear; Q - quadratic ; C cubic

# -------------------------------------------------
#many categories
dat %>%
  group_by(ZipCode) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ggplot(aes(as.factor(reorder(ZipCode, n)), n)) +
  geom_col() +
  coord_flip() +
  xlab("Zip Code")

zip_group <- dat %>%
  group_by(ZipCode) %>%
  summarise(med_price = median(AdjSalePrice),
            count = n()) %>%
  arrange(med_price) %>%
  mutate(cumul_count = cumsum(count),
         ZipGroup = ntile(cumul_count, 5))

dat <- dat %>%
  left_join(select(zip_group, ZipCode, ZipGroup), by = "ZipCode")

mod4 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + ZipGroup, data = dat)

stargazer(mod4, type = "text")

modx <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade, data = dat)
View(modx$residuals)
dat$residuals = modx$residuals

zip_group <- dat %>%
  group_by(ZipCode) %>%
  summarise(med_resid = median(residuals),
            count = n()) %>%
  arrange(med_resid) %>%
  mutate(cumul_count = cumsum(count),
         ZipGroup = ntile(cumul_count, 5))


dat <- dat %>%
  left_join(select(zip_group, ZipCode, ZipGroup), by = "ZipCode")

mod4 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + ZipGroup, data = dat)

mod5 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + as.factor(ZipGroup), data = dat)

stargazer(mod4, mod5,  out = "../Results/zipCodeModel.tex",  type = "latex")
stargazer("../Results/zipCodeModel.tex", mod4, type = "text")

dat %>% ggplot( aes(x=dat$SqFtTotLiving , y=AdjSalePrice,  group = BldgGrade)) +
  geom_point(alpha = 0.5, aes(colour = as.factor(BldgGrade))) +
  geom_line(data = dat_add, aes(y = .fitted, as.factor(BldgGrade))) # we change our data to the fitted values of the additive model

ggsave("../Results/sqftLiving.png")

output_stargazer("../Results/week3.tex", appendVal = FALSE, mod4,
                 title="Adjusted Sale Price, Living Space Building Grade and ZipCode",
                 label="tab:SqFtTotLiving_bldGrade",  digits = 6
)

stargazer::stargazer(modx, mod4, type = "latex", title = "Price and living space, building grade and ZipCode")

with(dat, pairs(~ AdjSalePrice + SqFtTotLiving + BldgGrade + ZipGroup))


ggplot(dat, aes(SqFtTotLiving + BldgGrade, y= AdjSalePrice, group = ZipGroup)) +
  geom_point(alpha = 0.5, size = 0.2,aes(colour = ZipGroup)) +
  geom_smooth(method = "lm", aes(colour = ZipGroup),
        ) +
  theme(legend.position = "bottom")
ggsave("Results/ZipGroup.png")

for (i in seq(7,22)) {
  boxplot(dat[i], xlab=colnames(dat[i]) )
}
