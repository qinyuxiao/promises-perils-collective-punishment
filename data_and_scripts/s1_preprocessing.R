library(tidyverse)
library(janitor)

# Read raw data (before anonymization)
dat_raw <- read_csv("raw_data/CP_STUDY1_RAW_DO_NOT_EDIT.csv")
dat_raw <- dat_raw[-c(1, 2), ]

# Remove those who did not consent
dat_raw <- dat_raw %>% filter(
  consent_agree == paste0("I have read and understood the above. ",
                          "I agree to participate in this study.")
)
nrow(dat_raw) # n = 1594 (all who consented to participate)

# Recode match reports
dat_raw <- dat_raw %>% mutate(
  match = case_when(
    match == "Yes, I obtained a match." ~ 1,
    match == "No, I did not obtain a match." ~ 0
  ))

# Only those who completed (n = 1559) the study will receive a bonus
dat_raw <- dat_raw %>% mutate(completed = as.numeric(!is.na(attentive)))
dat_bonus <- dat_raw %>% filter(completed == 1)

# Determine whether to apply collective punishment (yes)
dat_bonus %>%
  tabyl(condition, match) %>%
  adorn_totals(where = "col") %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")

# Normative expectation: "Socially Inappropriate" was the mode across conditions
dat_bonus %>%
  tabyl(condition, ne) %>%
  adorn_totals(where = "col") %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")

# Randomly pick a conditional normative expectation question (10% to 20%)
set.seed(2024)
sample(c(1, 2, 3, 4, 5), size = 1)

# Again it was "Socially Inappropriate" for all conditions
dat_bonus %>%
  tabyl(condition, cne_2) %>%
  adorn_totals(where = "col") %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")

# Determine which bonus to pay out
set.seed(2024)
dat_bonus$which_bonus <- sample(
  c("match", "ne", "cne"), size = nrow(dat_bonus), replace = TRUE
)

# Determine bonus
dat_bonus$bonus <- NA
dat_bonus <- dat_bonus %>% mutate(
  bonus = case_when(
    which_bonus == "match" & condition == "baseline" & match == 1 ~ 1.2,
    which_bonus == "match" & condition == "baseline" & match == 0 ~ 0.2,
    which_bonus == "match" & condition == "cp" & match == 1 ~ 1.2,
    which_bonus == "match" & condition == "cp" & match == 0 ~ 0,
    which_bonus == "match" & condition == "norm" & match == 1 ~ 1.4,
    which_bonus == "match" & condition == "norm" & match == 0 ~ 0.2,
    which_bonus == "ne" & ne == "Socially Inappropriate" ~ 0.4,
    which_bonus == "cne" & cne_2 == "Socially Inappropriate" ~ 0.4,
    TRUE ~ 0
  )
)

# Output
bonus_pay <- dat_bonus %>% select(pid, which_bonus, bonus)
approve <- dat_bonus %>% select(pid)

# Anonymization
dat_raw_anonymous <- left_join(dat_raw, bonus_pay, by = c("pid"))
dat_raw_anonymous <- dat_raw_anonymous %>% select(
  -c("IPAddress", "LocationLatitude", "LocationLongitude", "pid", "PROLIFIC_PID")
)

# Save
write_csv(approve, file = "raw_data/CP_STUDY1_TO_APPROVE.csv")
write_csv(
  bonus_pay %>% select(pid, bonus) %>% filter(bonus != 0),
  file = "raw_data/CP_STUDY1_BONUS.csv"
)
write_csv(dat_raw_anonymous, file = "processed_data/s1_data_anonymized_raw.csv")
