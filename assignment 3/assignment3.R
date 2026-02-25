# -----------------
### Assignment 3
## Carlo Knappschneider
# -----------------
# Library
library(dplyr)
library(broom)
library(ggplot2)
library(modelsummary)
library(sandwich)
library(marginaleffects)
library(tidyverse)


### 1. Part 1: In-Class (ANES Voter Turnout)
# -----------
## 1.1 Setup and data preparation
raw = read.csv("C:/Users/carlo/OneDrive/Dokumente/AQMSS2/anes_timeseries_2020.csv")

# a

print("NOTE: mutate vs transmute")
print("NOTE: case_when and ifelse")


class(NA_character_)
class(NA_real_)
class(NA)

df = raw %>%
  transmute(
    voted = ifelse(V202109x < 0, NA, V202109x),
    age = ifelse(V201507x < 0, NA, V201507x),
    female = case_when(
      V201600 == 2 ~ 1,
      V201600 == 1 ~ 0,
      TRUE ~ NA_real_),
    education = case_when(
      V201511x == 1 ~ 10, V201511x == 2 ~ 12, V201511x == 3 ~ 14, 
      V201511x == 4 ~ 16, V201511x == 5 ~ 20, TRUE ~ NA_real_),
    income = ifelse(V201617x < 0, NA, V201617x),
    party_id = ifelse(V201231x < 0, NA, V201231x)
    )
    
# b)
df = na.omit(df)
nrow(df)


# c)
mean(df$voted)
summary(df)

# ---------
## 1.2 Exploratory visualization

# a) 
turnout_by_edu = df %>%
  group_by(education) %>%
  summarise(turnout = mean(voted))

ggplot(turnout_by_edu, aes(x = factor(education), y = turnout)) +
  geom_col() +
  labs(x = "Years of education", y = "Turnout rate")

# b)
# Turnout increases with education

# -----------
## 1.3 Linear probability model
# a)
lpm = lm(voted ~ age + education + income + female, data = df)

# b)
tidy(lpm)

# c)
#It describes the change in probability of voting for every extra year of education.


# d)
preds_lpm = predict(lpm)
sum(preds_lpm < 0)
sum(preds_lpm > 1)
range(preds_lpm)

# ----------
## 1.4 Logistic regression
# a)
logit = glm(voted ~ age + education + income + female,
            family = binomial, data = df)


# b)
tidy(logit)

# c)
exp(coef(logit))


# d)
preds_logit = predict(logit, type = "response")
range(preds_logit)

# -----------
## 1.5 Comparing LPM and logit
# a)
avg_slopes(logit)

#b) 
#The AMEs from the logit model are similar to the LPM coefficients.

# c)
modelsummary(list("LPM" = lpm, "Logit" = logit),
             vcov = list("robust", NULL), output = "markdown")

# ------------
## 1.6 Predicted probabilities
# a)
p1 = plot_predictions(logit, condition = "education")
p1
ggsave("pred_prob_education.png", p1, width = 6, height = 4)

# b)
p2 = plot_predictions(logit, condition = c("age", "female"))
p2

ggsave("pred_prob_age_gender.png", p2, width = 6, height = 4)


# c) 
#Both education and age show a clear positive effect.
#both men and women follow similar age-turnout patterns.

# ------------
##1.7 Presenting results
# a)
p3 = modelplot(list("LPM" = lpm, "Logit" = logit),
               vcov = list("robust", NULL))
p3

# b)
ggsave("coefplot_lpm_logit.png", p3, width = 6, height = 4)



### ------------------

### 2. Part 2: Take-Home Exercises (STAR — High School Graduation)

# -----------------
## 2.1 Data preparation

# a)
star = read.csv("C:/Users/carlo/OneDrive/Dokumente/AQMSS2/star.csv")


star <- star %>%
  mutate(
    classtype_f = case_when(
      classtype == 1 ~ "Small",
      classtype == 2 ~ "Regular",
      classtype == 3 ~ "Regular+Aide",
      TRUE ~ NA_character_
    ),
    classtype_f = factor(classtype_f,
                         levels = c("Small", "Regular", "Regular+Aide"))
  )
class(star$classtype_f)

star <- star %>%
  mutate(
    race_f = case_when(
      race == 1 ~ "White",
      race == 2 ~ "Black",
      race == 3 ~ "Asian",
      race == 4 ~ "Hispanic",
      race == 5 ~ "Native American",
      race == 6 ~ "Other",
      TRUE ~ NA_character_
    ),
    race_f = factor(race_f,
                    levels = c("White", "Black", "Asian", "Hispanic", "Native American", "Other"))
  )


class(star$race_f)

# b)
star <- star %>%
  mutate(
    small = case_when(
      classtype_f == "Small" ~ 1,
      is.na(classtype_f) ~ NA_real_,   
      TRUE ~ 0
    )
  )

# c)
#star = na.omit(star)
#nrow(star)
star1 = star %>%
  drop_na(hsgrad)


# d)
mean(star1$hsgrad)

star1 %>%
  group_by(classtype_f) %>%
  summarise(
    grad_rate = mean(hsgrad),
    n = n()
  )


#The classes with regular size have the lowest graduation rate with 82,5%.
#The regular + aide classes have the highest one with 83,9%.

# --------------------
## 2.2 LPM and logit

# a)
lpm1 = lm(hsgrad ~ small, data = star1)
summary(lpm1)
coef(lpm1)

# b)
logit1 = glm(hsgrad ~ small, family = binomial, data = star1)
summary(logit1)


# c)
#Students in small classes have a 0,4% h higherhance of graduating high school,
#compared to students in non-small classes.

# d)
avg_slopes(logit1)
#The AME is 0.00375. This means that students in a small class have a 0,4% higher probability 
#of graduating from high school on average. The AME is almost identical to the LPM coefficient.

# ----------------
## 2.3 Adding controls
# a)
lpm2 = lm(hsgrad ~ small + race + yearssmall, data = star1)
logit2 = glm(hsgrad ~ small + race + yearssmall,
             family = binomial, data = star1)
coef(lpm2)
coef(logit2)

# b)
#The coef of lpm2 changes substantially, from about 0.0038 to about −0.073 in the controlled model.
#This suggests that the first comparison was affected by differences in race and years spent in small classes. 
#However, because years in small classes is partly determined by the initial treatment, controlling for it may introduce post-treatment bias.


# c)
avg_slopes(logit2)

# ------------------
## 2.4 Predicted probabilities
#a)

preds <- predictions(
  logit2,
  newdata = datagrid(
    race = c(1, 2),
    classtype = c(1, 1),
    yearssmall = c(3, 0)
  )
)

preds


plot_pred <- plot_predictions(
  logit2,
  condition = c("yearssmall", "small")
)

plot_pred

ggsave("predicted_probabilities.png", plot_pred, width = 7, height = 5)

# -------------------------
## 2.5 Interactions
# a)

logit3 = glm(hsgrad ~ small * race + yearssmall,
             family = binomial, data = star1)

# b)
avg_slopes(logit3, variables = "small", by = "race")

# c)

#The marginal effects of small classes are broadly similar across racial groups. 
#While the estimated effect is negative and statistically significant for White students and marginally significant for Black students, 
#the estimates for other groups are imprecise and not statistically significant.

# -----------------
## 2.6 Presenting results and discussion
# a)

models <- list(
  "LPM (bivariate)"   = lpm1,
  "LPM (controlled)"  = lpm2,
  "Logit (bivariate)" = logit1,
  "Logit (controlled)"= logit2
)


vcovs <- list(
  vcovHC(lpm1, type = "HC1"),
  vcovHC(lpm2, type = "HC1"),
  NULL,
  NULL
)

modelsummary(
  models,
  vcov = vcovs,
  statistic = "({std.error})",
  stars = TRUE,
  output = "markdown",
  exponentiate = c(FALSE, FALSE, TRUE, TRUE),
  coef_map = c(
    "(Intercept)" = "Intercept",
    "small" = "Small class (1=yes)",
    "race" = "Race (numeric code)",
    "yearssmall" = "Years in small class"
  ),
  gof_map = c("nobs", "r.squared", "aic", "bic", "logLik")
)

# b)
p <- modelplot(
  models,
  vcov = vcovs,
  coef_omit = "(Intercept)"
)

p

ggplot2::ggsave("coefplot_models.png", p, width = 8, height = 5)

# c)
#The STAR data suggest that small class sizes have little to no positive effect on high school graduation. 
#In the simple bivariate models, the estimated effect is very close to zero. After adding controls, the estimated effect becomes negative.
#The LPM and logit models show similar results. In both, the estimated effects are small and often statistically insignificant. 
#The logit marginal effects are nearly identical to the LPM coefficients, indicating that the linear probability model provide a good approximation.
#This experimental evidence is more credible than an observational study because class size was randomly assigned at the start of the STAR experiment.
#This ensures that, on average, students in small and regular classes are comparable in characteristics, reducing selection bias
