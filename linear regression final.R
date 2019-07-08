##
##  Introduction
## 
##
##   Learning objectives:
##     Learn the R formula interface
##     Specify factor contrasts to test specific hypotheses
##     Perform model comparisons
##     Run and interpret variety of regression models in R
##
## Set working directory
## 
##   It is often helpful to start your R session by setting your working
##   directory so you don't have to type the full path names to your data
##   and other files

# set the working directory

## Load library
library(ggplot2)
library(GGally)
library(dplyr)
library(tidyr)
library(scales)
library(RColorBrewer)
library(ggthemes)


##
## Load the states data
## read the states data
##

states.data <- readRDS("states.rds") 
#str(states.data)
#attributes(states.data)
View(states.data)

## get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])   
states.info


## look at last few labels
tail(states.info, 8)


## 
## Linear regression
## Examine the data before fitting models  
## Start by examining the data to check for problems.


## summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)


## Plot the data before fitting models
##   Plot the data to look for multivariate outliers, non-linear
##   relationships etc.

## scatter plot of expense vs csat
plot(sts.ex.sat)


## determine if there is correlation between expense and csat
cor(sts.ex.sat)


##
## Linear regression example
## 
##   Linear regression models can be fit with the `lm()' function
##   For example, we can use `lm' to predict SAT scores based on
##     per-pupal expenditures:


# Fit our regression model
sat.mod <- lm(csat ~ expense, # regression formula
              data=states.data) # data set


# Summarize and print the results
summary(sat.mod) # show regression coefficients table

## Plot residuals to check if its normally distributed
hist(sat.mod$residuals)


##
## Why is the association between expense and SAT scores /negative/?
## 
## Answeer:
## The association between expense and SAT scores is negative because 
## there is a negative correlatin between the two variables. 
## This implies that high expenditure, will lead to lower csta score and high csat score 
## is obtained when the expenditures are lower. 


##   Many people find it surprising that the per-capita expenditure on
##   students is negatively related to SAT scores. The beauty of multiple
##   regression is that we can try to pull these apart. What would the
##   association between expense and SAT scores be if there were no
##   difference among the states in the percentage of students taking the
##   SAT?

#### Answer
plot(states.data$csat, states.data$percent)
cor(states.data$csat, states.data$percent)

## There is strong negative correlation between csat and percentage of students taking the SAT exam

sat.mod2 = lm(csat ~ expense + percent, data = states.data)

## Summarize and print the results
summary(sat.mod2) # show regression coefficients table


## The coefficient for expense in logistic sat.mod2 is positive, it has switched from negative to positive 
## also it is not as significant as in sat.mod1. 
## The coefficient for percent in sat.mod2 is negative and highly significant. 
## The R^2 = 0.7857

## Plot residuals to check if its normally distributed
hist(sat.mod2$residuals)


## Creating another model with income  
sat.mod3 = lm(csat ~ expense+percent+income, data = states.data)
summary(sat.mod3) 

## In sat.mod3, the coefficient of percent is negative and highly significant with R^2=0.7984 
## is higher than that of sat.mod2

anova(sat.mod,sat.mod2,sat.mod3)


## The result from the lm() using expense and state percentage show that percentage
## have a negative coeffcient. An indication that states having different precentage 
## have a negative impact on cstat. Furthermore, cstat and percentages have very strong 
## negative correlation
## comparing the three model using anova() shows that a model containing both variables are 
## necessary to study the csat. The result from anova() also indicate that model 2 is highly
## significant compare to model 2

confint(sat.mod)
confint(sat.mod2)
confint(sat.mod3)


##
## The lm class and methods
## 
##   OK, we fit our model. Now what?
##   Examine the model object:


class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:20]
attributes(sat.mod)

##   Use function methods to get more information about the fit

confint(sat.mod)
hist(residuals(sat.mod))
anova(sat.mod)

confint(sat.mod2)
hist(residuals(sat.mod))
anova(sat.mod)


confint(sat.mod3)
hist(residuals(sat.mod))
anova(sat.mod)



## 
## Linear Regression Assumptions
## 
##   Ordinary least squares regression relies on several assumptions,
##     including that the residuals are normally distributed and
##     homoscedastic, the errors are independent and the relationships are
##     linear.
##
##   Investigate these assumptions visually by plotting your model:

plot(sat.mod)  
abline(sat.mod)

##
## Comparing models
## 
##   Do congressional voting patterns predict SAT scores over and above expense? 
##   Fit two models and compare them:
##


# fit another model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
sat.mod <- update(sat.mod, data=na.omit(states.data))
#sat.mod

coef(summary(sat.mod))
coef(summary(sat.voting.mod))


# compare using the anova() function
anova(sat.mod, sat.voting.mod)

## Answer
## A look at the result of the anova() shows that the inclusion of these variables are not necessary,
## since their coefficients are not that significant and so the model sat.voting.mod will not do better
## than sat.mod at predicting csat


#################################################################################################
## Exercise: least squares regression
## 
##   Use the /states.rds/ data set. 
##   Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
##   2. Print and interpret the model `summary'
##   3. `plot' the model to look for deviations from modeling assumptions


energy_metro <- subset(states.data, select = c("energy", "metro"))
summary(energy_metro)

################################
## Answers, "percent"
## Determine if there is a linear relationship between the variables, energy and metero
## 1.
plot(energy_metro)   
cor(states.data$energy,states.data$metro, use="complete.obs")   

## 2.
energy.mod <- lm(energy ~ metro, # regression formula
                 data=na.omit(states.data)) # data set states.data

# Summarize and print the results
summary(energy.mod) # show regression coefficients table


## Interprete the coefficients
## The metro has a negative coefficient, indicating a negative impact on energy use.
## Although this model is not highly significant. R^2 = 0.1154
## Therefore the model does not explain the dependent variable

## 3.
confint(energy.mod)
hist(energy.mod$residual)


##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?



#states.data2 <- data.frame(states.data$energy,states.data$metro,states.data$green)
states.data2 <- subset(states.data, select = c("energy", "metro","green"))
#View(states.data2)

pp <-ggpairs(states.data2)
pp


cor(states.data2, use="complete.obs") 


energy.mod2 <- lm(energy ~ metro+green, # regression formula      
                  data=na.omit(states.data)) #  states.data

# Summarize and print the results
summary(energy.mod2) # show regression coefficients table



states.data3 <- subset(states.data, select = c("energy", "metro","green","toxic"))
#View(states.data3)
pp <-ggpairs(states.data3)
pp



cor(states.data3, use="complete.obs") 


energy.mod3 <- lm(energy ~ metro+green+toxic, # regression formula      
                  data=na.omit(states.data)) #  states.data

# Summarize and print the results
summary(energy.mod3) # show regression coefficients table


# compare using the anova() function
anova(energy.mod,energy.mod2,energy.mod3)      


#################################################################################################
## Interactions and factors
## Modeling interactions
##
##   Interactions allow us assess the extent to which the association
##   between one predictor and the outcome depends on a second predictor.
##   For example: Does the association between expense and SAT scores
##   depend on the median income in the state?
## 

##  Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data) 

## Show the results
coef(summary(sat.expense.by.percent)) # show regression coefficients table

##  
## Regression with categorical predictors
##   Let's try to predict SAT scores from region, a categorical variable.
##   Note that you must make sure R does not think your categorical
##   variable is numeric.
##


## make sure R knows region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)

## Add region to the model
sat.region <- lm(csat ~ region,
                 data=states.data) 

## Show the results
summary(sat.region)
coef(summary(sat.region)) # show regression coefficients table
anova(sat.region) # show ANOVA table


##   Again, *make sure to tell R which variables are categorical by
##   converting them to factors!*
##  Setting factor reference groups and contrasts
##   In the previous example we use the default contrasts for region. The
##   default in R is treatment contrasts, with the first level as the
##   reference. We can change the reference group or use another coding
##   scheme using the `C' function.
## 

## print default contrasts
contrasts(states.data$region)  

## change the reference group, base=4
coef(summary(lm(csat ~ C(region, base=4),
                data=states.data)))

## change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),
                data=states.data)))

## change the coding scheme
coef(summary(lm(csat ~ C(region, base=4, contr.treatment ),
                data=states.data)))

##   See also `?contrasts', `?contr.treatment', and `?relevel'.

?contr.helmert()

#################################################################################################
## Exercise: interactions and factors
## 
##   Use the states data set.
##
##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.
##
##   2. Try adding region to the model. Are there significant differences
##      across the four regions?



################################
## Answers
##  1.  add another interaction to energy.mod1, energy.mod2, and energy.mod3 and then test this inclusion


# Ploting to see relationships

states.data4 <- subset(states.data, select = c("energy", "metro","green","toxic","region"))
#View(states.data4)
pp <-ggpairs(states.data4)
pp



## 2. 

# Summarize and print the results

str(states.data$region)  
levels(states.data$region)  

states.data$region <- factor(states.data$region)

energy.mod4 <- lm(energy ~ metro*region, # regression formula     ##    
                  data=na.omit(states.data)) #  states.data

summary(energy.mod4) # show regression coefficients table

energy.mod5 <- lm(energy ~ metro*region+green, # regression formula      
                  data=na.omit(states.data)) # states.data    

summary(energy.mod5) 


energy.mod6 <- lm(energy ~ metro*region+green+toxic, # regression formula      
                  data=na.omit(states.data)) # states.data 

summary(energy.mod6) 


hist(residuals(energy.mod4))
hist(residuals(energy.mod5))
hist(residuals(energy.mod6))


## Interprete these results including the anova() result.
anova(energy.mod4,energy.mod5,energy.mod6)


## The R-squared for the models are given as follows: R^2  for energy.mod4 = 0.2904, 
## the  R^2 for energy.mod5 = 0.6659, and R^2 for energy.mod6 = 0.7906. 
## This indicate that energy.mod6 is a better model compare to the other two models.
## Furthermore, a look at the result of the anova emphasizes this. Alhthough the althhough the p-value
## for energy.mod5 is small than that of energy.mod6, the sum of sq error of energy.mod 6 is the smallest
## of the models. Thus, energy.mod6 should be the selected model.
##
## Yes, there are significant differences across the region (as observed from model4 and 5), 
## however, when we include toxic to the model (model 6), the region are not significantly different.
## 







