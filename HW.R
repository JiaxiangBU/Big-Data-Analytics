v <- c(1, 1:3, c(5, 8), 13) #1 dimension
v

m <- matrix(1:10, 2) #2 dimensions
m

a <- array(1:8, c(2,2,2)) #n dimensions

a
l <- list(
  c(1, 2, 3),
  month.abb,
  matrix(c(3, -8, 1, -3), nrow = 2),
  list(1, "a"))
l

df <- data.frame(
  x = letters[1:5],
  y = rnorm(5),
  z = runif(5) > 0.5)
df

c(apple = 1, banana = 2, "kiwi fruit" = 3, 4)

twod.array <- array(
  1:6,
  dim = c(3, 2),
  dimnames = list(
    c("one", "two", "three"),
    c("un", "deux")
  ))
twod.array


l

n <- "numbers"
names(l) <- c(n, "months", "matrix", "list")
l

v[1:4]

v[c(-5,-6,-7)]


l

l[c(TRUE, FALSE, TRUE, FALSE)]


# df[1, ] row
# df[1] colomn
df[,1]

df[, 2:3, 1:2]
df[,]
df[[1]][2:3]  
# The first colomn 2-3 letters

df[df$x == "a" ]
df[df$x == "a" ,]

str(l)

str(heights)

gender_char <- c(1,2,1,2,1,2,3,4,2,3)
(gender_fac <- factor(gender_char))


str(l)

str(df)

heights <- data.frame(
  height_cm = c(153, 181, 150, 172, 165, 149, 174, 169, 198, 163),
  gender = c(
    "female", "male", "female", "male", "male",
    "female", "female", "male", "male", "female"
  ))



heights <- data.frame(
  height_cm = c(153, 181, 150, 172, 165, 149, 174, 169, 198, 163),
  gender = c(
    "female", "male", "female", "male", "male",
    "female", "female", "male", "male", "female"
  ))
levels(heights$gender)


class(heights$gender)

class(heights$height_cm)

nlevels(heights$gender)

gender_char <- c(
  "female", "male", "female", "male", "male",
  "female", "female", "male", "male", "female"
)
gender_char 

(gender_fac <- factor(gender_char))

a <- c(2,3,4)
levles(a)


factor(gender_fac, levels = c("male", "female"))


getting_to_work <- data.frame(
  mode = c(
    "bike", "car", "bus", "car", "walk",
    "bike", "car", "bike", "car", "car"
  ),
  time_mins = c(25, 13, NA, 22, 65, 28, 15, 24, NA, 14))
# getting_to_work <- subset(getting_to_work, is.na(time_mins))

getting_to_work <- subset(getting_to_work, !is.na(time_mins))

# getting_to_work <- subset(getting_to_work, is.na(time_mins))

unique(getting_to_work$mode)

getting_to_work$mode <- droplevels(getting_to_work$mode)
getting_to_work <- droplevels(getting_to_work)
levels(getting_to_work$mode)

##############################################################
# problem 1
##############################################################
library(ISLR)
data(package = "ISLR")
Auto <- data.frame(Auto)
lm_hp <- lm(weight~horsepower, data = Auto)
lm_hp

lm_hp.summary <- summary(lm_hp)
names(lm_hp.summary)
lm_hp.summary$coefficients


weight_hat <- predict(lm_hp)
head(weight_hat, n=5)
head(lm_hp$fitted.values, n=5)

newdata <- data.frame(horsepower=-1000) 
predict(lm_hp, newdata)

plot(Auto$horsepower, Auto$weight, data = Auto, col = "black")
predict_weight <- predict(lm_hp, data = Auto)
lines(Auto$horsepower, predict_weight, col = "red")

##############################################################
# problem 2
##############################################################
round(mean(lm_hp$residuals),4)
# > round(mean(lm_hp$residuals),4)
# [1] 0

# Because we assume predict_weight is unbaised predictor of weight, which is one of the 
# assumptions of OLS regression.


##############################################################
# problem 3
##############################################################
glm_hp <- glm(weight~horsepower, data = Auto, family=poisson)
round(glm_hp$coefficients,4)
round(lm_hp$coefficients,4)

summary(glm_hp)
summary(lm_hp)

# > glm_hp <- glm(weight~horsepower, data = Auto, family=poisson)
# > round(glm_hp$coefficients,4)
# (Intercept)  horsepower 
# 7.3767      0.0057 
# > round(lm_hp$coefficients,4)
# (Intercept)  horsepower 
# 984.5003     19.0782 
# > summary(glm_hp)
# 
# Call:
#   glm(formula = weight ~ horsepower, family = poisson, data = Auto)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -38.817   -6.450   -1.521    6.610   20.285  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) 7.377e+00  2.624e-03  2810.8   <2e-16 ***
#   horsepower  5.706e-03  2.155e-05   264.8   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 92543  on 391  degrees of freedom
# Residual deviance: 27148  on 390  degrees of freedom
# AIC: 30992
# 
# Number of Fisher Scoring iterations: 4
# 
# > summary(lm_hp)
# 
# Call:
#   lm(formula = weight ~ horsepower, data = Auto)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2191.1  -297.7   -80.1   330.8  1150.8 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 984.5003    62.5143   15.75   <2e-16 ***
#   horsepower   19.0782     0.5616   33.97   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 427.4 on 390 degrees of freedom
# Multiple R-squared:  0.7474,	Adjusted R-squared:  0.7468 
# F-statistic:  1154 on 1 and 390 DF,  p-value: < 2.2e-16

# The coefficients and intercept are signiciantlly diffrent.
