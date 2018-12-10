####### SETUP WORKSPACE #######

# Load dependencies
library(tidyverse)

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

# Cronbach's alpha