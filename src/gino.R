####### SETUP WORKSPACE #######

# Load dependencies
library(tidyverse)
library(psych)
library(knitr)
library(kableExtra)

# Set working directory
setwd("~/GitHub/osl-gino-kouchaki-galinsky-2015/src")

# Import data
gino <- read_csv("../data/Gino Kouchaki Galinsky 2015 Experiment 3.csv")

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

# Construct table
kable(clean_table, "latex", booktabs = TRUE, digits = 2, align = "lcccccc",
      col.names = c("Variable", "M", "SD", "M", "SD", "M", "SD")) %>%
  add_header_above(header = c(" ", "Inauthenticity" = 2, "Failure" = 2, "Control" = 2)) %>%
  kable_as_image(filename = "../data/results/apa_table")
