library(dplyr)
library(broom)
library(ggplot2)
library(modelsummary)
library(sandwich)

### 1. QoG Dataset

## 1.1 Setup and data preparation

# a)
qog = read.csv("https://www.qogdata.pol.gu.se/data/qog_std_cs_jan26.csv")
df = qog %>%
  select(country = cname, epi = epi_epi, women_parl = wdi_wip,
         gov_eff = wbgi_gee, green_seats = cpds_lg)
# b)
nrow(df)

# c)
summary(df)

## 1.2 Exploratory visualization

# a) & b) 
ggplot(df, aes(x = women_parl, y = epi)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Women in Parliament (%)", y = "EPI Score")

# c)
# The graph shows a positive relationship, meaning that on average countries with more women
# in parliament have higher EPI Scores

## 3. Bivariate regression

# a)
m1 = lm(epi ~ women_parl, data = df)

# b)
tidy(m1)

# c)
p25 = quantile(df$women_parl, 0.25, na.rm = TRUE)
p75 = quantile(df$women_parl, 0.75, na.rm = TRUE)

coef(m1)["women_parl"] * (p75 - p25)



## 1.4 Multiple regression

# a)
m2 = lm(epi ~ women_parl + gov_eff, data = df)
tidy(m2)

#b) 
#women_parl`s coefficient decreases in the multiple regression. 
#This is probably due to a correlation between gov_effect and women_parl.

## 1.5 Demonstrating OVB

# a)
beta1_biva = tidy(m1) %>% filter(term == "women_parl") %>% pull(estimate)
beta1_mult = tidy(m2) %>% filter(term == "women_parl") %>% pull(estimate)
beta2_mult = tidy(m2) %>% filter(term == "gov_eff") %>% pull(estimate)

# b)
aux = lm(gov_eff ~ women_parl, data = df)
delta = tidy(aux) %>% filter(term == "women_parl") %>% pull(estimate)

# c)
round(beta1_mult + beta2_mult * delta, 4)

round(beta1_biva, 4)

# d)
# gov_eff is positively correlated with women_parl

## 1.6 Robust standard errors

# a)
modelsummary(m2, output = "markdown")

# b)
modelsummary(m2, vcov = "robust", output = "markdown")

# c)
# The SEs do not differ substantially and the conclusion would not change anyway

## 1.7 Presenting results

# a)
modelsummary(list("Bivariate" = m1, "Multiple" = m2),
             vcov = "robust", output = "markdown")

# b)
modelplot(list("Bivariate" = m1, "Multiple" = m2),
          vcov = "robust")






### 2. STAR Dataset

## 2.1 Data preparation

# a)
star = read.csv("C:/Users/carlo/OneDrive/Dokumente/AQMSS2/star.csv")

# b)
star <- star %>%
  mutate(
    classtype = case_when(
      classtype == 1 ~ "Small",
      classtype == 2 ~ "Regular",
      classtype == 3 ~ "Regular+Aide",
      TRUE ~ NA_character_
    ),
    classtype = factor(classtype,
                       levels = c("Small", "Regular", "Regular+Aide"))
  )

class(star$classtype)

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

# c)
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


# d)
star <- star %>%
  mutate(
    small = case_when(
      classtype_f == "Small" ~ 1,
      is.na(classtype_f) ~ NA_real_,   
      TRUE ~ 0
    )
  )


# e)
sum(!is.na(star$g4reading))

sum(is.na(star$g4reading))

sum(!is.na(star$g4math))

sum(is.na(star$g4math))

## 2.2 Comparing groups

# a)
means_reading <- star %>%
  group_by(classtype) %>%
  summarise(
    mean_g4reading = mean(g4reading, na.rm = TRUE),
  )

means_reading

# small scores the highest

# b)
biv_reg <- lm(g4reading ~ small, data = star)
summary(biv_reg)
#Small classes score on average 3.1 points higher in fourth-grade reading than regular classes.
#But the effect is not significant.

# c)
star_two <- star %>%
  filter(classtype_f %in% c("Small", "Regular+Aide"))

m_simple <- lm(g4reading ~ classtype_f, data = star_two)
summary(m_simple)

with(star_two,
     mean(g4reading[classtype_f == "Small"], na.rm = TRUE) -
       mean(g4reading[classtype_f == "Regular+Aide"], na.rm = TRUE))

# d)
star %>%
  group_by(classtype_f) %>%
  summarise(mean_g4math = mean(g4math, na.rm = TRUE))

m_math <- lm(g4math ~ classtype_f, data = star)
summary(m_math)

# The pattern is similar class type does not affect much either reading or math scores.

##2.3

# a)
m_controls <- lm(g4reading ~ small + race_f + yearssmall, data = star)
summary(m_controls)

# b)
m_biv <- lm(g4reading ~ small, data = star)
summary(m_biv)

coef(m_biv)["small"]
coef(m_controls)["small"]

# Yes, the effect becomes negative and changes by about 7 points.

# c)
# It captures the marginal effect of spending one additional year in a small class on reading scores.

##2.4

# a)
m_interact <- lm(g4reading ~ small * race_f + yearssmall, data = star)
summary(m_interact)

# b) 
tidy(m_interact)

# c)
coef(m_interact)["small"]

coef(m_interact)["small"] +
  coef(m_interact)["small:race_fBlack"]

# d)
# The interaction term is meaningful. The effect of small classes differs strongly by race.
# White students experience a negative effect of about 5.3 points, Black students experience a positive effect of about 1.6 points.

##2.5

# a)
models <- list(
  "Bivariate"    = m_biv,
  "With controls"= m_controls,
  "Interaction"  = m_interact
)

vcovs <- list(
  vcovHC(m_biv,      type = "HC1"),
  vcovHC(m_controls, type = "HC1"),
  vcovHC(m_interact, type = "HC1")
)

table <- modelsummary(
  models,
  vcov = vcovs,
  stars = TRUE,
  statistic = "({std.error})",
  output = "gt"
)

table

# b)
plot <- modelplot(
  models,
  vcov = vcovs,
  coef_omit = "Intercept"
)

plot

# c)
ggplot2::ggsave("reading_models_coefplot.png", plot = p, width = 10, height = 6, dpi = 300)


##2.6

# The STAR data suggest that small class sizes do not have a large overall effect on student achievement, but the effect may differ across racial groups. 
# For White students, the estimated effect is negative, while for Black students it is positive. However, some of these differences are not very precise.
# This evidence is more credible than a normal observational study because STAR was a randomized experiment. Students were randomly assigned to small or regular classes, 
# which reduces selection bias and makes the treatment more independent from background characteristics.
# There are some limitations. The overall explanatory power of the models is low, and some subgroup effects are imprecise. 
# Therefore the size and consistency of the effects should be interpreted with caution.