#################################################################################
#################################################################################
# Marketing Analytics
# Tutorial 2: Consumer and Customer Analytics
#################################################################################
#################################################################################

#################################################################################
# Set up
#################################################################################

# load required packages
# install.packages("tidyverse")
library(tidyverse) # for "nicer" plots & case_when
# install.packages("mlogit")
library(mlogit) # for multinomial logit models
# install.packages("lmtest")
library(lmtest) # for likelihood ratio test
# install.packages("expm")
library(expm)  # for matrix exponentiation
# install.packages("superheat")
library(superheat) # for transition visualization in heatmap
# install.packages("clickstream")
library(clickstream) # for Markov Chain

# set working directory to the desired folder
getwd()
# setwd("INSERT FILE PATH HERE")

#################################################################################
# Part A) Binary Choice: Logistic Regression
#################################################################################

#################################################################################
# Exercise 1

# We use the data set mtcars from R that contains design and performance aspects 
# of automobiles in the early 1970s. 
data(mtcars)
# Take a look at the included variables and the number of observations.
summary(mtcars)
str(mtcars)

####
# a)
#	Use linear regression to estimate the engine type of a car using only the 
# weight as regressor. Visualize the resulting model in an appropriate graph.
####

# Estimate the described relationship (engine shape as DV, weight as IV).
car_lin <- lm(vs ~ wt, data = mtcars)
summary(car_lin)
# Heavier cars are less likely to have a straight engine (a V-shape instead),
# since the coefficient is negative (-0.29) and statistically significant.
# The R² value suggests this variable explains roughly 30% of the variation in
# engine type.

plot(mtcars$wt, mtcars$vs, xlab = "Weight", 
     ylab = "V-shape (0) or straight (1) engine")
# Include the linear regression in the scatterplot.
abline(car_lin)
# The slope conveys the negative relationship estimated by the linear regression.

summary(car_lin$fitted.values)
# The fitted values are the estimated y values for each observed x (weight). For
# this linear model, they range from - 0.19 to 0.92. Since our dependent variable
# is meant to convey whether the engine is V-shaped or straight, we need to 
# "translate" these fitted values.

# Often, a threshold of 0.5 is set for y. Using the general linear regression
# function y = b0 + b1*x, we can rearrange to x = (y - b0)/b1 to find the 
# corresponding x.
x_threshold <- (0.5 - coef(car_lin)[1]) / coef(car_lin)[2] # 3
abline(v = x_threshold, lty = 2)
# For cars with a weight less than 3000 lbs, we predict a straight engine, a 
# V-shape for heavier cars.

# "Nicer" graph using ggplot shows which cars would be predicted wrongly.
ggplot(data = mtcars, aes(x = wt, y = vs)) +
  geom_rect(aes(xmin = 1, xmax = x_threshold, ymin = -0.25, ymax = 1), fill = "darkseagreen4", alpha = 0.05) +
  annotate(geom = "text", x = 2, y = 0.5, label = "Straight") +
  geom_rect(aes(xmin = x_threshold, xmax = 6, ymin = -0.25, ymax = 1), fill = "darkseagreen", alpha = 0.05) +
  annotate(geom = "text", x = 4, y = 0.5, label = "V-Shape") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_vline(xintercept = x_threshold, linetype = "dashed") +
  annotate(geom = "point", x = c(2.14, 2.62, 2.77, 2.875), y = 0, color = "darkseagreen", 
           size = 3) +
  annotate(geom = "point", x = c(3.15, 3.19, 3.215, 3.44, 3.46), y = 1, color = "darkseagreen4", 
           size = 3) +
  labs(x = "Weight", y = "V-shape (0) or straight (1) engine")
# Using this approach, we would incorrectly predict a straight engine shape for
# the light green observations (cars) and a wrong V-shape for the cars in the
# dark green points. 

# More specifically, we use the predicted y for each car in the data set
mtcars$prob_lin <- predict(car_lin, mtcars)
# to estimate the engine type by adhering to the 0.5 threshold.
mtcars$pred_lin <- ifelse(mtcars$prob_lin >= 0.5, 1, 0)
table(mtcars$pred_lin, mtcars$vs)
# Overall, the linear regression would mispredict the engine type for 4 + 6
# = 10 or over 30% of cars in the data set. 

####
# b)	
# Estimate the same relationship using logistic regression. Again, visualize 
# the model.
####

# Estimate the described relationship (engine shape as DV, weight as IV).
car_log <- glm(vs ~ wt, data = mtcars, family = binomial)
summary(car_log)
# The general result that heavier cars are more likely to have a V-shape engine
# remains. The coefficient is negative (-1.91) and statistically significant.

plot(mtcars$wt, mtcars$vs, xlab = "Weight", 
     ylab = "V-shape (0) or straight (1) engine")
# Include the predictions of the logistic regression in the scatterplot.
mtcars$prob_log <- predict(car_log, mtcars, type = "response")
points(prob_log ~ wt, mtcars, col = "darkseagreen", pch = 16)

summary(car_log$fitted.values)
# A logistic regression has probabilities as outputs, so that y is constrained 
# between 0 and 1, as seen in the range of the fitted values.
# Again, we can manually set a threshold for y. Considering the graph and 
# potential mispredictions, let's use 0.7 and obtain the corresponding x value.
x_threshold <- (log(0.7/0.3) - coef(car_log)[1]) / coef(car_log)[2] # 2.55
abline(v = x_threshold, lty = 2)
# For cars with a weight less than 2550 lbs, we predict a straight engine, a 
# V-shape for heavier cars.

# "Nicer" graph using ggplot shows which cars would be predicted wrongly.
ggplot(data = mtcars, aes(x = wt, y = vs)) +
  geom_rect(aes(xmin = 1, xmax = x_threshold, ymin = 0, ymax = 1), fill = "darkseagreen4", alpha = 0.05) +
  annotate(geom = "text", x = 2, y = 0.5, label = "Straight") +
  geom_rect(aes(xmin = x_threshold, xmax = 6, ymin = 0, ymax = 1), fill = "darkseagreen", alpha = 0.05) +
  annotate(geom = "text", x = 4, y = 0.5, label = "V-Shape") +
  geom_point() +
  geom_smooth(method = "glm", se = FALSE, method.args = list(family = binomial),
              color = "black") +
  geom_vline(xintercept = x_threshold, linetype = "dashed") +
  annotate(geom = "point", x = 2.14, y = 0, color = "darkseagreen", 
           size = 3) +
  annotate(geom = "point", x = c(2.78, 3.15, 3.19, 3.215, 3.44, 3.46), y = 1, color = "darkseagreen4", 
           size = 3)
# Using this approach, we would incorrectly predict engine shape for less cars.
mtcars$pred_log <- ifelse(mtcars$prob_log >= 0.7, 1, 0)
table(mtcars$pred_log, mtcars$vs)
# Overall, the logistic regression would mispredict the engine type for 1 + 7
# = 8 or 25% of cars in the data set. 

####
# c)
# Manually calculate the association between engine type and car weight using the 
# odds ratio.
####

# The odds of a binomial variable = p/(1 - p). This is also referred to as the 
# ratio of the probability of the desired outcome (p) to the probability of the
# alternative outcome (1 - p). 
# So, while the probability of rolling a 4 with a regular dice = 1/6. The odds
# of getting a 4 = 1/5, or p/(1-p).

# In R, we use plogis(x) to obtain the logistic distribution function and 
# calculate the odds ratio manually. This allows us to interpret the estimated
# coefficient in more detail than before.
plogis(coef(car_log)[2]) / (1 - plogis(coef(car_log)[2]))
# From the lecture, you know this is equivalent to:
exp(coef(car_log)[2])
# The effect of the car's weight equals an estimated odds ratio of 0.15. This
# suggests that the odds of having a straight engine (vs = 1) decreases by 85% 
# for each one-unit increase (1000 lbs) in car weight.


#################################################################################
# Exercise 2

# We will use a data set  that contains information on customer transactions, as 
# well as their satisfaction, on an e-commerce website specializing in clothing 
# and accessories.

# First, download the sales data.
salesdata <- read.csv("https://goo.gl/4Akgkt") 
# Take a look at the included variables and the number of observations.
summary(salesdata)
str(salesdata)

####
# a) 
# Using logistic regression, estimate the relationship between customers 
# receiving the coupon and their purchasing behavior of the promoted product. 
####

# Estimate the described relationship (purchase as DV, coupon as IV).
purchase_m1 <- glm(purchase ~ coupon, data = salesdata, family = binomial)
summary(purchase_m1)  
# Customers that receive the coupon are more likely to buy the promoted product, 
# since the coupon coefficient is positive (1.72) and statistically significant.

####
# b)
# Manually calculate the association between purchase and the coupon factor 
# using the odds ratio.
####

# We can either use plogis( ) and calculate the ratio manually
plogis(1.7234) / (1-plogis(1.7234))
# or use exp( ) to get the exponential.
exp(coef(purchase_m1)[2])  
# The result shows that the effect of the coupon equals an estimated odds ratio 
# of 5.603, meaning that the odds of purchasing the product is 460% higher for
# customers who received a coupon.

####
# c)
# How does the model from a) change if region, overall satisfaction, and total 
# spending are also added as predictors? 
####

# Estimate the described relationship (purchase as DV, coupon, lifetime spending,
# region and overall satisfaction as IV).
purchase_m2 <- glm(purchase ~ coupon + spendToDate + region + satOverall, 
                   data = salesdata, family = binomial)
summary(purchase_m2)

# The coefficient for coupon is still positive and significant.
# The coefficient for spending is not significant, so it seems that previous 
# purchases do not influence the purchase of the promoted product.
table(salesdata$region)
# The Mideastern region is taken as the baseline in this model. Region does not
# seem to have a significant effect, as all coefficients are not significant.
# Overall satisfaction has a highly significant effect. Higher satisfaction is
# related to a higher purchase likelihood (positive coefficient).

exp(coef(purchase_m2)[c(2, 10)])
# The odds of purchasing the product are 161 % higher for customers that
# received a coupon. For a one-unit increase in overall satisfaction, the odds
# of purchasing the product increases by 87%.

####
# d)
# Is there an interaction between the coupon variable and overall satisfaction 
# in their relationship with the purchase of the promoted product? 
####

# Estimate the described relationship (purchase as DV, coupon and overall
# satisfaction as IV) and include an interaction term in addition to the main 
# effects.
purchase_m3 <- glm(purchase ~ coupon + satOverall + coupon:satOverall,         
                   data = salesdata, family = binomial)
summary(purchase_m3)
# The interaction term itself can be added to the formula using :, or include the 
# main effects and the interaction effect directly using *. Both approaches lead 
# to the same model.
# glm(purchase ~ coupon * satOverall, data = salesdata, family = binomial)
# The interaction term is not significant. 


#################################################################################
# Exercise 3

# We will use a data set gathered by a multi-national bank to analyse customer 
# churn.  

# First, download the bank data.
load("bankdata.RData")
# Take a look at the included variables and the number of observations.
summary(bankdata)
str(bankdata)

####
# a) 
# Using logistic regression, estimate the relationship between customers 
# churning and their residence and account balance. 
####

# Estimate the described relationship (churn as DV, country and balance as IV).
bank_log <- glm(churn ~ balance + country,         
               data = bankdata, family = binomial)
summary(bank_log) 
# All coefficients (except that for Spain) are significant.

####
# b) 
# Interpret the model coefficients using the odds ratio.
####

# Calculate the odds ratio for the 2 significant coefficients.
exp(coef(bank_log)[2:3])  
# The result shows that even though the coefficient for account balance is 
# statistically significant (p-value near 0), the effect size might be negligible. 
# For each additional dollar in the account, the odds of a customer churning 
# increases by 0.0003%. While this seems tiny, it also means that for each 100
# dollar increase in the account, the odds of churning increases by 0.03% and for
# each 10.000 dollar increase, the odds of churning increases by 3%. In order to
# not lose too many high-value customers, the bank should consider why this is 
# the case.
# Furthermore, customers living in Germany have 216% higher odds of churning
# compared to the baseline (residence in France).


#################################################################################
# Exercise 4

# We will use a data set that contains information on passengers of an airline 
# and their satisfaction with the services provided. 

# First, download the airline data.
load("airlinedata.RData")
# Take a look at the included variables and the number of observations.
summary(airlinedata)
str(airlinedata)

####
# a) 
# Using logistic regression, estimate the relationship between passenger 
# satisfaction and their gender, travel occasion and delay at arrival. 
####

# Estimate the described relationship (satisfaction as DV, gender, travel type 
# and delay at arrival as IV).
airline_m1 <- glm(Satisfaction ~ Gender + Type.of.Travel + Arrival.Delay.in.Minutes, 
                   data = airlinedata, family = binomial)
summary(airline_m1) 
# All coefficients are significant.

####
# b) 
# Interpret the model coefficients using the odds ratio.
####

# When interpreting the effects of factor variables, its important to be aware of
# the variable order.
str(airlinedata)
# Satisfaction: 0 dissatisfied vs 1 satisfied
# Gender: 0 female vs 1 male
# Type.of.Travel: 0 business vs 1 personal

# Calculate the odds ratio for all coefficients.
exp(coef(airline_m1)[2:4]) 
# All estimated odds ratio are < 1 suggesting that all three variables have a 
# negative effect on passenger satisfaction.
# More specifically, the odds of being satisfied are 58% lower for men and 39%
# lower for personal travel. For each minute of delay upon arrival, the odds of a
# passenger being satisfied with the trip decreases by 0.5%. This also means, a 
# 15-minute delay reduces the odds of being satisfied by 7.5% and a one-hour
# delay reduces the odds by 30%.

####
# c)	
# Is there an interaction between passenger gender and travel occasion?
####

# Estimate the described relationship (satisfaction as DV, gender and travel type 
# as IV) and include an interaction term in addition to the maineffects.
airline_m2 <- glm(Satisfaction ~ Gender + Type.of.Travel + Gender:Type.of.Travel, 
                  data = airlinedata, family = binomial)
summary(airline_m2) 
# The interaction term is significant.

# Calculate the odds ratio for all coefficients.
exp(coef(airline_m2)[2:4]) 
# Now, both odds ratios for the gender and travel occasion are > 1. This is very 
# different to the results in b). The reason lies in the interaction.
# Exponentiating the interaction coefficient does not lead to an odds ratio, but
# rather a ratio of odds ratios. 
# The correct interpretation is as follows: 
# For female passengers (Gender = 0), the odds ratio for travel type is 3.67,
# indicating that the odds of being satisfied are 267% higher for personal travel.
# Instead, for male passengers, the odds ratio for travel type is found by 
# multiplying 3.67 and the interaction ratio.
exp(coef(airline_m2)[3]) * exp(coef(airline_m2)[4])
# For men, the odds of being satisfied are 92% lower for personal travel. 


#################################################################################
# Part B) Product Choice: Multinomial Logit Model
#################################################################################

#################################################################################
# Exercise 5

# We use a data set  that contains the heating system choices for 900 single-
# family Californian houses. The builders of the houses can choose between 
# five different systems: gas central (gc), gas room (gr), electric central (ec), 
# electric room (er), and heat pump (hp). 

# First, download the choice data.
load("heatingdata.RData") 
# Take a look at the included variables and the number of observations.
summary(heatingdata)
head(heatingdata)
# For instance, the first household selected the gas central option.

####
# a)
# Estimate a multinomial logit model with installation and operating costs, but 
# do not include an intercept. 
####

# Estimate the described relationship (depvar as DV, installation and operating
# costs as IV) using a multinomial logit model. Suppress the intercept either 
# specifying + 0 in the formula directly
heating_costmodel <- mlogit(depvar ~ 0 + ic + oc, heatingdata)
# or by adding | 0. Both approaches lead to the same estimated model.
# mlogit(depvar ~ ic + oc | 0, heating)
summary(heating_costmodel)
# Both estimated coefficients are negative. This suggests that as the cost of a 
# heating system rises (and the costs of the alternatives remain the same), the 
# probability of that system being chosen decreases. Both coefficients are 
# statistically significant.

####
# b) 
# How close do the model's estimated probabilities match the shares of houses 
# that actually chose each heating system?
####

# The observed frequencies are given at the top of the model summary output. 
#       ec       er       gc       gr       hp 
# 0.071111 0.093333 0.636667 0.143333 0.055556 

# The function fitted returns the fitted values from the estimated model.
# Specifying the argument outcome as FALSE returns the probability for each 
# alternative and for each house. Using apply will take the mean for each column.
apply(fitted(heating_costmodel, outcome = FALSE), MARGIN = 2, FUN = mean)
# Observed frequencies and predicted shares do not match well, especially for 
# the two gas alternatives.

####
# c) 
# Calculate the willingness to pay (WTP) for a $1 reduction in annual operating 
# costs.
####

# The ratio of operating cost coefficient to installation cost coefficient gives
# the WTP for a $1 reduction in operating costs (paid for through higher 
# installation costs).
coef(heating_costmodel)["oc"]/coef(heating_costmodel)["ic"]
# Individuals are willing to pay $0.73 more in installation costs to have $1 lower
# annual operating costs.

####
# d) 
# What is the discount rate r implied by this WTP? Assume a sufficiently long 
# life T of the operating system so that the current value of operating costs cv
# approaches oc/r for an increasing lifetime.
####

# As T increases, cv approaches oc/r. Then, a $1 reduction in annual operating
# costs reduces cv by 1/r, representing the WTP for such a future discount. 
# Rearranging this gives the discount rate implied by the model (r = 1/WTP)
1 / (coef(heating_costmodel)[2]/coef(heating_costmodel)[1])
# A discount rate > 1 is not reasonable.


#################################################################################
# Exercise 6

# Continuing  with the heating data set, estimate a multinomial logit model with 
# installation and operating costs and include alternative-specific constants. 
# Take the heat pump as the base alternative. 

# With J alternatives, a model can only estimate J-1 alternative-specific 
# constants. The resulting coefficients are interpreted relative to the base J.

# Estimate the described relationship (depvar as DV, installation and operating
# costs as IV) using a multinomial logit model. Include alternative-specific
# constants, taking hp as the base.
heating_altconstant <- mlogit(depvar ~ ic + oc, heatingdata, reflevel = "hp")
summary(heating_altconstant)
# All estimates except for the constant for gas room are statistically 
# significant at the 5% level. Both costs coefficients are negative as before.

#### 
# a)
# How close do this model's estimated probabilities now match the shares of 
# houses that actually chose each heating system? 
####

# Again, the observed frequencies are given at the top of model summary output.
#       hp       ec       er       gc       gr
# 0.055556 0.071111 0.093333 0.636667 0.143333 
apply(fitted(heating_altconstant, outcome = FALSE), MARGIN = 2, FUN = mean)
# The probabilities are a perfect match. Alternative-specific constants ensure
# average probabilities equal the observed shares in logit models.

####
# b) 
# Again, compute the corresponding WTP and discount rate.
####

coef(heating_altconstant)["oc"]/coef(heating_altconstant)["ic"]
# Individuals are willing to pay $4.56 higher installation costs to have $1 lower
# annual operating costs.
1 / (coef(heating_altconstant)["oc"]/coef(heating_altconstant)["ic"])
# The corresponding discount rate is 0.22. 


#################################################################################
# Exercise 7

# Now, relate the magnitude of upfront installation costs to household income and
# add this to the model instead of installation cost by itself. Include operating 
# costs as before.
####

# Estimate the described relationship (depvar as DV, installation costs divided
# by income and operating costs as IV) using a multinomial logit model. Since the
# symbol : is used for interactions in R formulas, use the wrapper I( ) to 
# specify the desired division.
heating_relcost <- mlogit(depvar ~ oc + I(ic / income), heatingdata)
summary(heating_relcost)
# Both cost coefficients remain negative, but the estimated coefficient for the 
# installation cost term is not statistically significant.


#################################################################################
# Exercise 8

# Next, use alternative-specific income effects instead, again with the heat pump
# as the base alternative. Test whether including income effects leads to a 
# better model than one using alternative-specific constants.

# Estimate the described relationship (depvar as DV, installation and operating
# costs as IV) using a multinomial logit model. Include alternative-specific
# income effects, taking hp as the base. The operator | is used to signify the 
# variable on which the formula is conditioned.
heating_alteffect <- mlogit(depvar ~ oc + ic | income, heatingdata, reflevel = "hp")
summary(heating_alteffect)
# Income does not seem to have a strong effect, since all corresponding 
# coefficients are not statistically significant.

# Use a likelihood ratio test to compare the smaller model with alternative-
# specific constants to this model with alternative-specific income effects.
lrtest(heating_altconstant, heating_alteffect)
# The p-value is too high, the null hypothesis that the smaller model provides as
# good a fit for the data as the larger one can't be rejected. It seems that 
# including the income effects does not lead to a better model.


#################################################################################
# Exercise 9

# Finally, we use a multinomial logit model for prediction. The Californian
# government is considering to offer a 15% rebate on the installation cost of 
# heat pumps. They want to predict the effect of this proposal on the choice of 
# heating systems. Use the estimated coefficients from Exercise 6 to calculate 
# the new probabilities and predicted shares in the case of the cheaper 
# installation costs for a heat pump. 

# Create a copy of the data set.
heating_new <- heatingdata
# Adjust the installation costs of heat pumps to reflect the proposed rebate.
heating_new[idx(heating_new, 2) == "hp", "ic"] <- 
  0.85 * heating_new[idx(heating_new, 2) == "hp", "ic"]

# The observed frequencies in the original data:
apply(fitted(heating_altconstant, outcome = FALSE), 2, mean)
# Use predict() to estimate the new shares of the heating systems after the 
# proposed price change for heat pumps.
apply(predict(heating_altconstant, newdata = heating_new), 2, mean)
# It seems that the proposed rebate would lead to a few more households selecting
# the heat pump alternative.


#################################################################################
# Part C: Markov Chain Models
#################################################################################

#################################################################################
# Exercise 10

# Imagine a popular restaurant is now offering a weekly lunch, where customers 
# can choose between 3 dish options: Pasta, Rice, and Salad. The individual 
# dishes vary weekly, but the base component of each of the 3 alternatives 
# remains the same. Due to the weekly menu, assume that customers will only get 
# lunch at the restaurant once a week. 

#### 
# a)
# In the first week, 70% of customers ate pasta for lunch, 20% rice, and 10% 
# salad. Create a vector in R with the starting probabilities of the 3 menu 
# options. 
####

# Assign a meaningful name to the specified vector of starting probabilities and
# name the individual elements according to the corresponding lunch dish.
p_start <- c(0.7, 0.2, 0.1)
names(p_start) <- c("Pasta", "Rice", "Salad")
p_start

####
# b)
# Since the restaurant just started offering the lunch menu, there is no 
# historical data from which to derive the likelihood of customers switching 
# between the 3 lunch options in the following weeks. Instead, the restaurant
# used its regular dinner orders to estimate the purchasing behavior of their 
# lunch customers. Store the transition probabilities as a matrix in R.
####

# The matrix is filled with the specified values element-wise by row. There are 3 
# rows in total.
p_trans <- matrix(c(0.1, 0.6, 0.3, 0.5, 0.4, 0.1, 0.2, 0.8, 0), nrow = 3, 
                  byrow = TRUE)
# Assign meaningful names to the rows and columns.
rownames(p_trans) <- c("Pasta (t)", "Rice (t)", "Salad (t)")
colnames(p_trans) <- c("Pasta (t+1)", "Rice (t+1)", "Salad (t+1)")
p_trans
# A customer who purchased the Rice dish in the first week has a 40% probability
# of eating the same type of lunch in the next week, for example.

####
# c)
# Use the information you have to compute the estimated popularity of each of the 
# 3 lunch alternatives in the second week.
####

# Simply using the normal product operator would calculate element-wise. All 
# values in the first row of the matrix are multiplied by the first vector 
# element, all values in the second row by the second vector element, and all 
# values in the final row of the matrix are multiplied by the third vector 
# element.
p_start * p_trans

# Use %*% to calculate the matrix product of the starting probabilities and the
# transition probabilities.
p_start %*% p_trans
# The prediction for the second week is that 19% of customers will order the 
# Pasta, 58% the Rice dish, and 23% Salad.
# Manually calculating the predicted popularity of Pasta in the second week 
# confirms this.
0.7*0.1 + 0.2*0.5 + 0.1*0.2

####
# d)
# Do the same popularity prediction for the third week in which the restaurant 
# offers the lunch menu.
####

# Calculate the state probabilities in the third week by multiplying the state
# probabilities that result from c) with the transition probabilities.
p_start %*% p_trans %*% p_trans
# The prediction for the second week is that 35.5% of customers will order the 
# Pasta, 53% the Rice dish, and 11.5% Salad.
# Manually calculating the predicted popularity of Pasta in the third week 
# confirms this.
0.19*0.1 + 0.58*0.5 + 0.23*0.2

####
# e)
# Find the long-term steady state shares of the 3 lunch options.
####

# Exponentiate the transition matrix and multiply with the state probabilities to
# get the steady long-term prediction.
p_start %*% (p_trans %^% 100)
# The long-term prediction is that 32.5% of customers will order the Pasta, 52.5% 
# the Rice dish, and 15% Salad. This steady state is achieved in the long run, 
# regardless of the initial distribution across the states (driven by transition
# probabilities).
c(1, 0, 0) %*% (p_trans %^% 100)
c(1/3, 1/3, 1/3) %*% (p_trans %^% 100)


#################################################################################
# Exercise 11

# A local gym offers memberships that allow users to choose whether they want to 
# subscribe each month. This means that a gym user could be active for 2 months, 
# then inactive for 3 before coming back to the gym after.

####
# a)	
# Given the following example of a gym member, calculate the transition 
# probabilities and store in a matrix in R. 
####

# The transcription sequence can be rewritten to convey the transition pairs:
# 1 -> 2  yes -> yes
# 2 -> 3  yes -> yes
# 3 -> 4  yes -> no
# 4 -> 5  no  -> no
# 5 -> 6  no  -> no
# 6 -> 7  no  -> yes
# 7 -> 8  yes -> no
# 8 -> 9  no  -> yes
# 9 -> 10 yes -> yes
# When the user was in an active state, he remained active in 3/5 of cases and 
# turned inactive in the remaining 2/5. Instead, when the user was inactive, he
# remained so in 2/4 of cases and became active in 2/4.
gym_trans <- matrix(c(3/5, 2/5, 2/4, 2/4), nrow = 2, 
                  byrow = TRUE)
rownames(gym_trans) <- c("Active (t)", "Inactive (t)")
colnames(gym_trans) <- c("Active (t+1)", "Inactive  (t+1)")
gym_trans

####
# b)	
# What is the likelihood that this gym member is active next month? What is the 
# probability in 5 months?
####

# Next month:
gym_trans[1,]

# In five months:
(gym_trans %^% 5)[1,]

####
# c)	
# The gym has gathered data (gym_data.RData) on 100 of its members over the past 
# year. More specifically, the gym knows in which of the 12 months each of these
# members were active. Estimate the Markov Chain and visualize the transition 
# probabilities.
####

load("gymdata.RData")

# Fit a Markov Chain to the data so that the next state only depends on the 
# current stage.
gym_mc <- fitMarkovChain(gymdata, order = 1)
# Print the observed transition matrix.
gym_mc_trans <- t(gym_mc@transitions[[1]])
# We transpose the transition matrix to read column (t-1) to row (t), as in 
# Exercise 10.
gym_mc_trans
# Members that were inactive for a month are also more likely (54%) to stay that
# way in the next month. Members that were active last month are also more likely
# (64%) to stay active. The estimated probabilities were relatively consistent
# with the results in a) suggesting that this member was a good representation 
# of the average gym user.

plot(gym_mc)
# These transition probabilities can also be illustrated in a graph.

####
# d)	
# What is the likelihood that a gym member is active next month? What is the 
# probability in 5 months? Compare to the results in b).
####

# Next month:
gym_mc_trans[2,]
# The likelihood of the user being active again in the next month is 64%, 
# slightly higher than our initial prediction.

# In five months:
(gym_mc_trans %^% 5)[2,]
# The likelihoods have converged closer together.


#################################################################################
# Exercise 12

# We will use a public data set  of a web server log that contains requests made 
# to a web server for the US Environmental Protection Agency (EPA). 

# First, download the server log data. 
load("epaweb_data.RData") 
# Take a look at the included variables and the number of observations.
summary(epaweb_data)

####
# a)
# When are users most active? Choose an appropriate visualization method.
#### 

# One way to visualize the distribution of requests over time is to plot a 
# histogram of the datetime variable.
hist(epaweb_data$datetime, breaks = 25,
     main = "User Requests Over Time", xlab = "Date & Time")
# It seems most requests occur in the evening (of Wednesday, August 29, 1995) and
# by far the fewest requests are registered in the early morning hours.

####
# b)
# How many unique users are registered in the data? Do the users vary in their
# activity?
####

# Get the length of the vector of unique host IDs to see that the web server log
# counts 2333 active users.
length(unique(epaweb_data$host))

# Plot the sorted frequency table of users.
host_tab <- sort(table(epaweb_data$host), decreasing = TRUE)
plot(host_tab, main = "Activity of Unique Users",
     ylab = "Number of Requests by User") 
# The graph indicates an anti-logarithmic pattern, suggesting that only a few 
# users account for the majority of activity.

####
# c)
# How many unique sessions are there? 
####

# The unique number of sessions is given either by the sum of all new sessions,
sum(epaweb_data$newsession)
# or the final running total (maximum value).
max(epaweb_data$sessionnum)
# There are 3314 unique sessions.

#### 
# d)	
# Estimate the Markov Chain model based on the data set epamarkov_data.RData. 
# This data set only considers the requests of the 20 most popular HTML pages.
####

load("epamarkov_data.RData") 

# Fit a Markov Chain to the data so that the next state only depends on the 
# current stage.
epa_mc <- fitMarkovChain(epamarkov_data, order = 1)
# Print the observed transition matrix.
epa_mc_trans <- t(epa_mc@transitions[[1]])
# We transpose the transition matrix to read column (t-1) to row (t), as in 
# Exercise 10. 
epa_mc_trans[1:5, 1:5]
# For example, users go from the Info page to the News page with 10%. 8% of 
# users on the News page transition to the Info page.

####
# e)
# Visualize the transition matrix in a heat map using the package superheat.
####

# Since the layout of the heat map is determined partially at random, set the 
# random number seed to ensure consistency.
set.seed(59911)
# Plot the heat map, removing the first row (contains transitions from END).
superheat(epa_mc_trans[-1, ],                
          bottom.label.size = 0.4,
          bottom.label.text.size = 3.5,
          bottom.label.text.angle = 270,
          left.label.size = 0.3,
          left.label.text.size = 4,
          heat.col.scheme = "red", 
          n.clusters.rows = 5, n.clusters.cols = 5,
          left.label = "variable", bottom.label = "variable", 
          title = "Transition Matrix in Sequences of Top 20 HTML Pages")
# Most obvious pattern suggests most users visit the efhome page after the major
# docs and Software page. 

# Alternatively use a regular graph layout and restrict to those transitions that
# occur with at least 25% probability.
set.seed(70510)
plot(epa_mc, minProbability = 0.25)
# This graph shows the association between pages and common transition paths.
# It shows the previous pattern (major docs & software -> home page), as well as
# the transition from "what's new" to "what's hot"

####
# f)
# Use the transition likelihoods to predict the next page request in session 110. 
# Similarly, predict the next two likely pages for session 160.
####

# Session 110 gives the event sequence of host1439
epamarkov_data[110]
# Store the sequence in a new clickstream object, keeping all individual requests
# of the session except for the last (the END).
epa_pred <- new("Pattern", sequence = head(unlist(epamarkov_data[110]), -1))
# Use the fitted Markov Chain to predict the next likely page.
predict(epa_mc, epa_pred, dist = 1)
# The next page is most likely the Rules page (29%).

# Do the same for session 160, but predict the next 2 pages.
epamarkov_data[160]
epa_pred <- new("Pattern", sequence = head(unlist(epamarkov_data[160]), -1))
predict(epa_mc, epa_pred, dist = 2)
# With 10% probability the sequence continues with the Rules page before reaching
# the end state.