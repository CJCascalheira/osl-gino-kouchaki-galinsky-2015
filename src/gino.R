####### SETUP WORKSPACE #######

# Load dependencies
library(tidyverse)
library(psych)
library(knitr)
library(kableExtra)
library(broom)
library(car)
library(gmodels)

# Set working directory
setwd("~/GitHub/osl-gino-kouchaki-galinsky-2015/src")

# Import data
gino <- read_csv("../data/Gino Kouchaki Galinsky 2015 Experiment 3.csv")

# Set default ggplot theme
theme_set(theme_minimal())

###### CLEAN #######

# Filter for participants who passed manipulation check
gino_clean <- gino %>%
  filter(FAILED_MC == 0) %>%
  select(-c(instr, filter, CONDITION, FAILED_MC, MCheck, age, male),
         -ends_with("Dummy")) %>%
  rename(condition = condition_string)

# Factorize condition variable
gino_clean <- within(gino_clean, {
  condition <- factor(condition)
})

####### UNDERSTAND #######

# Compute means of dependent measures
(gino_means <- gino_clean %>%
  mutate(
    feelings_of_impurity = select(gino_clean, starts_with("impurity")) %>% rowMeans(),
    feelings_of_discomfort = select(gino_clean, starts_with("dissonance")) %>% rowMeans(),
    negative_affect = select(gino_clean, starts_with("neg")) %>% rowMeans(),
    positive_affect = select(gino_clean, starts_with("pos")) %>% rowMeans(),
    embarrassment = select(gino_clean, embarrassed, ashamed) %>% rowMeans(),
    self_alienation = select(gino_clean, starts_with("alien")) %>% rowMeans()
  ) %>%
  select(condition, feelings_of_impurity, feelings_of_discomfort, negative_affect,
         positive_affect, embarrassment, self_alienation, decided_to_help))

### Cronbach's Alpha ###

# Cronbach's alpha for feelings of impurity
gino_clean %>%
  select(starts_with("impurity")) %>%
  psych::alpha()

# Cronbach's alpha for feelings of discomfort
gino_clean %>%
  select(starts_with("dissonance")) %>%
  psych::alpha()

# Cronbach's alpha for negative affect
gino_clean %>%
  select(starts_with("neg")) %>%
  psych::alpha()

# Cronbach's alpha for positive affect
gino_clean %>%
  select(starts_with("pos")) %>%
  psych::alpha()

# Cronbach's alpha for embarrassment
gino_clean %>%
  select(embarrassed, ashamed) %>%
  psych::alpha()

# Cronbach's alpha for self-alienation
gino_clean %>%
  select(starts_with("alien")) %>%
  psych::alpha()

### Descriptive Statistics ###

# Calculate descriptives
(gino_summary <- gino_means %>%
  select(-decided_to_help) %>%
  gather(key = item, value = score, -condition) %>%
  group_by(condition, item) %>%
  summarize(
    mean = mean(score),
    sd = sd(score),
    n = n(),
    t_star = qt(p = 0.975, df = n - 1),
    upper = mean + (t_star * (sd/sqrt(n))), 
    lower = mean - (t_star * (sd/sqrt(n)))
  ))

# Factorize item and add labels
(gino_summary <- within(gino_summary, {
  item <- factor(item, 
                 levels = c("self_alienation", "feelings_of_impurity", "feelings_of_discomfort",
                            "negative_affect", "positive_affect", "embarrassment"),
                 labels = c("Self-alienation", "Feelings of impurity", "Discomfort",
                            "Negative affect", "Positive affect", "Embarrassment"))
})) 

# Drop confidence interval information
(gino_summary_short <- gino_summary %>%
    select(condition, item, mean, sd))

# Rename columns
(gino_summary_short <- gino_summary_short %>%
    rename(
      Variable = item,
      M = mean,
      SD = sd
    ) %>%
    arrange(Variable))

# Separate by condition
(gino_table <- gino_summary_short %>%
  filter(condition == "inauthenticity") %>%
  ungroup() %>%
  select(Variable, M, SD))

failure <- gino_summary_short %>%
  filter(condition == "failure") %>%
  ungroup() %>%
  select(M, SD)

condition <- gino_summary_short %>%
  filter(condition == "neutral") %>%
  ungroup() %>%
  select(M, SD)

# Prepare table format
(clean_table <- bind_cols(gino_table, failure, condition))

# Construct table, save as .png
kable(clean_table, "latex", booktabs = TRUE, digits = 2, align = "lcccccc",
      col.names = c("Variable", "M", "SD", "M", "SD", "M", "SD")) %>%
  add_header_above(header = c(" ", "Inauthenticity" = 2, "Failure" = 2, "Control" = 2)) %>%
  kable_as_image(filename = "../data/results/apa_table")

# Construct table, save as .jpeg (for white background)
kable(clean_table, "latex", booktabs = TRUE, digits = 2, align = "lcccccc",
      col.names = c("Variable", "M", "SD", "M", "SD", "M", "SD")) %>%
  add_header_above(header = c(" ", "Inauthenticity" = 2, "Failure" = 2, "Control" = 2)) %>%
  kable_as_image(filename = "../data/results/apa_table", file_format = "jpeg")

### One-way ANOVA ###

# Self-alienation
alienation_aov <- aov(self_alienation ~ condition, data = gino_means)

# Partial eta-squared
(alienation_tidied <- tidy(alienation_aov))
alienation_tidied$sumsq[1] / (alienation_tidied$sumsq[1] + alienation_tidied$sumsq[2])

# Pairwise comparisons
pairwise.t.test(gino_means$self_alienation, gino_means$condition,
                p.adjust.method = "bonferroni")

## Outliers?
ggplot(gino_means, aes(x = condition, y = self_alienation)) +
  geom_boxplot()

## Normality?
alienation_residuals <- residuals(alienation_aov)
shapiro.test(alienation_residuals)
plot(alienation_aov, 2)

## Homoscedasticity?
leveneTest(self_alienation ~ condition, data = gino_means)
plot(alienation_aov, 1)

###############################################################################

# Feelings of Impurity
impurity_aov <- aov(feelings_of_impurity ~ condition, data = gino_means)

# Partial eta-squared
(impurity_tidied <- tidy(impurity_aov))
impurity_tidied$sumsq[1] / (impurity_tidied$sumsq[1] + impurity_tidied$sumsq[2])

# Pairwise comparisons
pairwise.t.test(gino_means$feelings_of_impurity, gino_means$condition,
                p.adjust.method = "bonferroni")

## Outliers?
ggplot(gino_means, aes(x = condition, y = feelings_of_impurity)) +
  geom_boxplot()

## Normality?
impurity_residuals <- residuals(impurity_aov)
shapiro.test(impurity_residuals)
plot(impurity_aov, 2)

## Homoscedasticity?
leveneTest(feelings_of_impurity ~ condition, data = gino_means)
plot(impurity_aov, 1)

###############################################################################

# Discomfort
discomfort_aov <- aov(feelings_of_discomfort ~ condition, data = gino_means)

# Partial eta-squared
(discomfort_tidied <- tidy(discomfort_aov))
discomfort_tidied$sumsq[1] / (discomfort_tidied$sumsq[1] + discomfort_tidied$sumsq[2])

# Pairwise comparisons
pairwise.t.test(gino_means$feelings_of_discomfort, gino_means$condition,
                p.adjust.method = "bonferroni")

## Outliers?
ggplot(gino_means, aes(x = condition, y = feelings_of_discomfort)) +
  geom_boxplot()

## Normality?
discomfort_residuals <- residuals(discomfort_aov)
shapiro.test(discomfort_residuals)
plot(discomfort_aov, 2)

## Homoscedasticity?
leveneTest(feelings_of_discomfort ~ condition, data = gino_means)
plot(discomfort_aov, 1)

###############################################################################

# Negative Affect
negative_aov <- aov(negative_affect ~ condition, data = gino_means)

# Partial eta-squared
(negative_tidied <- tidy(negative_aov))
negative_tidied$sumsq[1] / (negative_tidied$sumsq[1] + negative_tidied$sumsq[2])

# Pairwise comparisons
pairwise.t.test(gino_means$negative_affect, gino_means$condition,
                p.adjust.method = "bonferroni")

## Outliers?
ggplot(gino_means, aes(x = condition, y = negative_affect)) +
  geom_boxplot()

## Normality?
negative_residuals <- residuals(negative_aov)
shapiro.test(negative_residuals)
plot(negative_aov, 2)

## Homoscedasticity?
leveneTest(negative_affect ~ condition, data = gino_means)
plot(negative_aov, 1)

###############################################################################

# Positive Affect
positive_aov <- aov(positive_affect ~ condition, data = gino_means)

# Partial eta-squared
(positive_tidied <- tidy(positive_aov))
positive_tidied$sumsq[1] / (positive_tidied$sumsq[1] + positive_tidied$sumsq[2])

# Pairwise comparisons
pairwise.t.test(gino_means$positive_affect, gino_means$condition,
                p.adjust.method = "bonferroni")

## Outliers?
ggplot(gino_means, aes(x = condition, y = positive_affect)) +
  geom_boxplot()

## Normality?
positive_residuals <- residuals(positive_aov)
shapiro.test(positive_residuals)
plot(positive_aov, 2)

## Homoscedasticity?
leveneTest(positive_affect ~ condition, data = gino_means)
plot(positive_aov, 1)

###############################################################################

# Embarrassment
embarrassment_aov <- aov(embarrassment ~ condition, data = gino_means)

# Partial eta-squared
(embarrassment_tidied <- tidy(embarrassment_aov))
embarrassment_tidied$sumsq[1] / (embarrassment_tidied$sumsq[1] + embarrassment_tidied$sumsq[2])

# Pairwise comparisons
pairwise.t.test(gino_means$embarrassment, gino_means$condition, 
                p.adjust.method = "bonferroni")

## Outliers?
ggplot(gino_means, aes(x = condition, y = embarrassment)) +
  geom_boxplot()

## Normality?
embarrassment_residuals <- residuals(embarrassment_aov)
shapiro.test(embarrassment_residuals)
plot(embarrassment_aov, 2)

## Homoscedasticity?
leveneTest(embarrassment ~ condition, data = gino_means)
plot(embarrassment_aov, 1)

###############################################################################

### Chi-square Test of Independence ###

# Select helping variable
(gino_help <- gino_clean %>%
  select(condition, decided_to_help))

# Conduct chi-square test on contingency table
chisq.test(table(gino_help))

# Conduct chi-square test on two vectors
chisq.test(gino_means$condition, gino_means$decided_to_help)

# Function to calculate Cramer's V
cv.test = function(x,y) {
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
              (length(x) * (min(length(unique(x)),length(unique(y))) - 1)))
  print.noquote("Cramér V / Phi:")
  return(as.numeric(CV))
}

# Cramer's V for all conditions
cv.test(gino_means$condition, gino_means$decided_to_help)

# Percentage of participants helping by condition 
gino_help %>%
  group_by(condition) %>%
  summarize(
    percentage = mean(decided_to_help)
  )

# Chi-square of inauthenticity vs. failure
gino_help_fail <- gino_help %>%
  filter(condition != "neutral")

# Drop unused factor level
gino_help_fail$condition <- droplevels(gino_help_fail$condition)

chisq.test(table(gino_help_fail))

# Crosstabs method
CrossTable(gino_help_fail$condition, gino_help_fail$decided_to_help, 
           format = "SPSS", chisq = TRUE)

# Chi-square of inauthenticity vs. neutral
gino_help_neutral <- gino_help %>%
  filter(condition != "failure")

# Drop unused factor level
gino_help_neutral$condition <- droplevels(gino_help_neutral$condition)

chisq.test(table(gino_help_neutral))

# Crosstabs method
CrossTable(gino_help_neutral$condition, gino_help_neutral$decided_to_help, 
           format = "SPSS", chisq = TRUE)
