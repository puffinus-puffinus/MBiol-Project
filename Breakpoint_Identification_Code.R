#This code:
#(a) fits a linear mixed-effects model for changes in each measured visual field parameter across ontogeny
#(b) identifies the optimal breakpoint for each model
#(c) performs diagnostic checks by examining residual plots and plotting fitted against observed values


data <- read.csv("Visual_Field_Parameters_and_Morphological_Traits.csv")
data$Bird_ID <- factor(data$Bird_ID)  


#1 - VERTICAL EXTENT OF BINOCULARITY

#Fit model
library(arm)
library(lme4)
library(lmerTest)
library(performance)
library(see)
breaks <- seq(14, 35, by = 0.1)

get_aic_a <- function(a) {
  data$a_age1 <- pmin(data$Days_Old, a)
  data$a_age2 <- pmax(data$Days_Old - a, 0)
  
  model <- lmer(
    Vertical_Extent ~ a_age1 + a_age2 + (1 | Bird_ID),
    data = data,
    REML = FALSE
  )
  
  AIC(model)
  
}

#Identify breakpoint
aic_values_a <- sapply(breaks, get_aic_a)

best_break_a <- breaks[which.min(aic_values_a)]
best_break_a # = 24.5 

data$a_age1 <- pmin(data$Days_Old, best_break_a)
data$a_age2 <- pmax(data$Days_Old - best_break_a, 0)

a_model <- lmer(
  Vertical_Extent ~ a_age1 + a_age2 + (1 | Bird_ID),
  data = data,
  REML = TRUE
)

summary(a_model)


#Check assumptions
arm::display(a_model)
check_heteroscedasticity(a_model)
check_predictions(a_model)
plot(check_normality(a_model, effects = "fixed"))
plot(check_normality(a_model, effects = "random"))



#2 - BINOCULAR FIELD WIDTH IN THE HORIZONTAL PLANE

#Fit model
library(lme4)
library(lmerTest)
breaks <- seq(14, 35, by = 0.1)

get_aic_b <- function(c) {
  data$b_age1 <- pmin(data$Days_Old, b)
  data$b_age2 <- pmax(data$Days_Old - b, 0)
  
  model <- lmer(
    Horizontal_Binocular_Width ~ b_age1 + b_age2 + (1 | Bird_ID),
    data = data,
    REML = FALSE
  )
  
  AIC(model)
  
}

#Identify breakpoint
aic_values_b <- sapply(breaks, get_aic_b)

best_break_b <- breaks[which.min(aic_values_b)]
best_break_b # = 29.5 

data$b_age1 <- pmin(data$Days_Old, best_break_b)
data$b_age2 <- pmax(data$Days_Old - best_break_b, 0)

c_model <- lmer(
  Horizontal_Binocular_Width ~ b_age1 + b_age2 + (1 | Bird_ID),
  data = data,
  REML = TRUE
)

summary(b_model)


#Check assumptions
arm::display(b_model)
check_heteroscedasticity(b_model)
check_predictions(b_model)
plot(check_normality(b_model, effects = "fixed"))
plot(check_normality(b_model, effects = "random"))



#3 - BINOCULAR FIELD AREA

#Fit model
library(lme4)
library(lmerTest)
breaks <- seq(14, 35, by = 0.1)

get_aic_c <- function(d) {
  data$c_age1 <- pmin(data$Days_Old, c)
  data$c_age2 <- pmax(data$Days_Old - c, 0)
  
  model <- lmer(
    Area_Percentage ~ c_age1 + c_age2 + (1 | Bird_ID),
    data = data,
    REML = FALSE
  )
  
  AIC(model)
  
}

#Identify breakpoint
aic_values_c <- sapply(breaks, get_aic_c)

best_break_c <- breaks[which.min(aic_values_c)]
best_break_c # = 29.5 

data$c_age1 <- pmin(data$Days_Old, best_break_c)
data$c_age2 <- pmax(data$Days_Old - best_break_c, 0)

c_model <- lmer(
  Area_Percentage ~ c_age1 + c_age2 + (1 | Bird_ID),
  data = data,
  REML = TRUE
)

summary(c_model)


#Check assumptions
arm::display(c_model)
check_heteroscedasticity(c_model)
check_predictions(c_model)
plot(check_normality(c_model, effects = "fixed"))
plot(check_normality(c_model, effects = "random"))
