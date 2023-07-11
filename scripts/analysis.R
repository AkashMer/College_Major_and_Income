# Downloaidng the data
devtools::install_github("jhudsl/collegeIncome")
library(collegeIncome)
data(college)

# Downloading the matahari package
devtools::install_github("jhudsl/matahari")
library(matahari)

# Start the documentation of analysis
dance_start(value = FALSE, contents = FALSE)

# Explore the data set
str(college)

# Q. Are college major category and income significantly associated
# Plan:
# 1. Look at the distribution of the income variable which has been
#   represented as the median(50th), p25th and p75th quantiles of samples
#   taken from full time employed individuals who studied a particular major
# 2. median column will be used as a representation of income related to that major
#   since median values are less sensitive to number of samples, we should be fine
#   with different no. of samples from each major
# 3. The major_category will need to be converted to a factor variable
# 4. Create a boxplot of median income for each major category
# 5. Fit a simple linear model using lm() for median income of full time individuals
#   against each major category so each category will have their own dummy variables
# 6. I feel the median income (our outcome) should be adjusted for percent of
#   individuals with jobs not requiring a college degree to give a fair assessment
# 7. So a second model with this percentage included will be compared to the simpler
#   first model
# Extracting only the relevant variables from the data set for our comparison
library(dplyr)
medianIncomebyMajor <- college %>%
    # Finding the percentage with jobs not requiring a college degree out of
    # the full time employed year round individuals since our median income is only
    # for full time year round employed individuals
    mutate(perc_non_college_jobs_full_time_employed_yearround =
               perc_non_college_jobs/perc_employed_fulltime_yearround) %>%
    select(median, major_category, perc_non_college_jobs_full_time_employed_yearround)

head(medianIncomebyMajor)
# Checking if all the rows are complete in our data set
mean(complete.cases(medianIncomebyMajor))

# Looking at incomplete rows
medianIncomebyMajor[which(!complete.cases(medianIncomebyMajor)),]

# Looking at this row in the original data set
college[which(!complete.cases(medianIncomebyMajor)),]

# No information on perc_college_jobs, perc_non_college_jobs and perc_low_wage_jobs
# is there for this row
# This row will thus be imputed only if there is a significant change in relationship
# of major category and income when adjusted for jobs not requiring college degree
naIndex <- which(is.na(medianIncomebyMajor$perc_non_college_jobs_full_time_employed_yearround))

# Looking at the unique major categories and converting it into a factor
unique(medianIncomebyMajor$major_category)
# 16 unique major categories, so our model fit should have 15 dummy variables
# To determine the levels of the factor of major category, we will arrange them
# in decreasing order of the mean median income from each
# This is done in order to better visualize any trend relationship
# when going from higher income to lower income
arrangedbyMeanMedianIncome <- medianIncomebyMajor %>%
    summarise(meanMedianIncome = mean(median), .by = major_category) %>%
    arrange(desc(meanMedianIncome))
medianIncomebyMajor$major_category <- factor(
    medianIncomebyMajor$major_category,
    levels = arrangedbyMeanMedianIncome$major_category)

# Converting the median income column in 1000 times the current unit
# for better visualization in the plot
medianIncomebyMajor$median <- medianIncomebyMajor$median/1000

# Distribution of our outcome variable
library(ggplot2)
ggplot(medianIncomebyMajor) +
    geom_histogram(aes(median), fill = "salmon", color = "black", alpha = 0.5)

# The distribution seems approximately normal except for an outlier on the higher side
# We will find out which major category this belongs to in the box plot
ggplot(medianIncomebyMajor) +
    geom_boxplot(aes(major_category, median))

# Points to note,
# 1. The income does not change a lot among the major categories
# 2. Outlier belongs to Business major
# 3. Interdisciplinary majors seem to not have any info on sub-majors, thus contain
#   only 1 value, if this was included in the model, this category will have a high
#   variance of the coefficient
outlierIndex <- which.max(college$median)
interdisciplinaryIndex <- which(college$major_category == "Interdisciplinary")

# Fitting a simple linear model to check association of median income
# in full time employed year round individuals
fit <- lm(median ~ major_category, medianIncomebyMajor)

# Summary of this fit
summary(fit)

# H0 - No difference in mean median income for each major category (All are equal)
# H1 - Difference in mean median income for each major category (All are not equal)
# Any pvalues below 0.05 except the intercept since this value is not estimating
# the difference in means, but the mean median income for Agriculture & Natural Resources
# major will be considered significant
# pvalues will be adjusted since multiple comparisons were done
# Adjustment Procedure: FDR less than 0.05 with benjamini-hochberg method
# This is done to avoid any problems with FPs since we are conducting 15 comparisons
sum(p.adjust(summary(fit)$coef[-1,4], method = "BH") < 0.05)

# Fitting a simple linear model to check association of median income
# adjusted for income from jobs not requiring a college degree
fit2 <- lm(median ~ major_category + perc_non_college_jobs_full_time_employed_yearround,
           medianIncomebyMajor)

# Summary of this fit
summary(fit2)

# H0 and H1 are similar
# Any pvalues below 0.05 except the intercept since this value is not estimating
# the difference in means, but the mean median income for Agriculture & Natural Resources
# major
# Again, same adjustment procedure will be used here
sum(p.adjust(summary(fit2)$coef[-1,4], method = "BH") < 0.05)

# Checking the models regarding the mean of residuals being zero
mean(fit$residuals)
mean(fit2$residuals)

# To perform ANOVA on our models, we would have to impute the NaN containing row
# from the first model
# Imputation will be done with the mean value for that major category
meanPercentCompMath <- mean(medianIncomebyMajor$perc_non_college_jobs_full_time_employed_yearround[
    which(medianIncomebyMajor$major_category == "Computers & Mathematics")
], na.rm = TRUE)
medianIncomebyMajor$perc_non_college_jobs_full_time_employed_yearround[naIndex] <-
    round(meanPercentCompMath, 4)
# Confirming removal of NaN
which(is.na(medianIncomebyMajor$perc_non_college_jobs_full_time_employed_yearround))

# Updating the 2nd fit to including this imputed value
fit2imputed <- lm(median ~ major_category + perc_non_college_jobs_full_time_employed_yearround,
                  medianIncomebyMajor)

# Checking mean of residuals
mean(fit2imputed$residuals)

# Comparing the 2 fits using anova
anova(fit, fit2imputed)

# Adjusting for income from jobs not requiring college degrees did not produce
# a significantly different model from the simpler one
# The VIF of these 2 variables does show mild association of the two predictor variables
library(car)
vif(fit2)

# Thus we will be going with the simpler 1 predictor model
summary(fit)

# Major categories which have significanlty different mean median Income
which(p.adjust(summary(fit)$coef[-1,4], method = "BH") < 0.05)

# Looking at all the fitted model plots to find understand the influence of the outlier
par(mfrow = c(3,3))
plot(fit)

# The outlier does have higher leverage but does not influence the model a lot
# Thus keeping our simpler model is a good option
# Major categories which have significanlty different mean median Income
which(p.adjust(summary(fit)$coef[-1,4], method = "BH") < 0.05)

# Do these remain the same once the outlier for the business is removed
fitoutlier <- lm(median ~ major_category, medianIncomebyMajor[-outlierIndex,])
which(p.adjust(summary(fitoutlier)$coef[-1,4], method = "BH") < 0.05)

# no, but Does removing the outlier make sense
college[63,]
# I dont think so

dance_save("./college_major_analysis.rds")
dance_stop()

## -------------------------------------------------------------------------

# Personal exploration beyond the question

levels <- levels(medianIncomebyMajor$major_category)

positives <- sapply(levels, function(level, data) {
    data$major_category <-
        relevel(data$major_category, level)
    fit <- lm(median ~ major_category, data)
    adjustedpVals <- p.adjust(summary(fit)$coef[-1,4], method = "BH")
    sum(adjustedpVals < 0.05)
}, medianIncomebyMajor)
positives

ind <- which(medianIncomebyMajor$major_category %in%
                 c("Business", "Computers & Mathematics", "Humanities & Liberal Arts"))

fit4 <- lm(median ~ major_category, medianIncomebyMajor[ind, ])
summary(fit4)
fit4star <- lm(median ~ major_category + perc_non_college_jobs_full_time_employed_yearround,
               medianIncomebyMajor[ind, ])
summary(fit4star)

fit5 <- lm(median/1000 ~ factor(major_category) + total, college)
summary(fit5)
which(p.adjust(summary(fit5)$coef[-1,4], method = "BH") < 0.05)
plot(fit5, which = 1)
p.adjust(summary(fit)$coef[-1,4], method = "BH")[c(11,13,14)]

hatvalues(fit)[order(hatvalues(fit), decreasing = TRUE)]
hatdat <- data.frame(num = 1:nrow(medianIncomebyMajor), hatvalues = hatvalues(fit))
ggplot(hatdat, aes(num, hatvalues)) + geom_point()

cookdat <- data.frame(num = 1:nrow(medianIncomebyMajor), cookdist = cooks.distance(fit))
ggplot(cookdat, aes(num, cookdist)) + geom_point()

dfbetasdat <- data.frame(num = 1:nrow(medianIncomebyMajor), dfbetas = dfbetas(fit)[,11])
ggplot(dfbetasdat, aes(num, dfbetas)) + geom_point()
dfbetasdat <- data.frame(num = 1:nrow(medianIncomebyMajor), dfbetas = dfbetas(fit)[,13])
ggplot(dfbetasdat, aes(num, dfbetas)) + geom_point()
dfbetasdat <- data.frame(num = 1:nrow(medianIncomebyMajor), dfbetas = dfbetas(fit)[,14])
ggplot(dfbetasdat, aes(num, dfbetas)) + geom_point()

for(i in 1:16) {
    dfbetasdat <- data.frame(num = 1:nrow(medianIncomebyMajor), dfbetas = dfbetas(fit)[,i])
    with(dfbetasdat, plot(num, dfbetas))
}
# It does influence the model, but it cannot be ignored as well