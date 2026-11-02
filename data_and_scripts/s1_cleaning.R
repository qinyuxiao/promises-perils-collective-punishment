library(tidyverse)

dat_raw <- read_csv("processed_data/s1_data_anonymized_raw.csv")

##### Recode responses #####
dat_raw <- dat_raw %>% mutate(
  across(c(pn, cpn_1, cpn_2, cpn_3, cpn_4, cpn_5), ~ case_when(
    . == "Very Inappropriate" ~ 1,
    . == "Inappropriate" ~ 2,
    . == "Somewhat Inappropriate" ~ 3,
    . == "Somewhat Appropriate" ~ 4,
    . == "Appropriate" ~ 5,
    . == "Very Appropriate" ~ 6,
    TRUE ~ NA
  )),
  across(c(ne, cne_1, cne_2, cne_3, cne_4, cne_5), ~ case_when(
    . == "Very Socially Inappropriate" ~ 1,
    . == "Socially Inappropriate" ~ 2,
    . == "Somewhat Socially Inappropriate" ~ 3,
    . == "Somewhat Socially Appropriate" ~ 4,
    . == "Socially Appropriate" ~ 5,
    . == "Very Socially Appropriate" ~ 6,
    TRUE ~ NA,
  )),
  across(d_1:d_17, ~ case_when(
    . == "Strongly Disagree" ~ 1,
    . == "Disagree" ~ 2,
    . == "Neutral" ~ 3,
    . == "Agree" ~ 4,
    . == "Strongly Agree" ~ 5,
    TRUE ~ NA,
  )),
  attentive = case_when(
    attentive == "5 - Very" ~ 5,
    attentive == "4" ~ 4,
    attentive == "3" ~ 3,
    attentive == "2" ~ 2,
    attentive == "1 - Not at all" ~ 1,
    TRUE ~ NA
  ),
  d_attention = case_when(
    d_17 == 4 ~ "Pass",
    TRUE ~ "Fail"
  )
)

# Reverse code, compute mean
dat_raw <- dat_raw %>%
  mutate(
    d_1 = 6 - d_1,
    d_3 = 6 - d_3,
    d_5 = 6 - d_5,
    d_7 = 6 - d_7,
    d_9 = 6 - d_9,
    d_13 = 6 - d_13,
    d_14 = 6 - d_14,
    d_16 = 6 - d_16
  ) %>%
  rowwise() %>%
  mutate(d_score = mean(c_across(d_1:d_16)))

# Add subject ID
dat_raw$id <- c(1:nrow(dat_raw))

##### Select variables into cleaned datasets #####
dat_clean <- dat_raw %>% select(
  id,
  completion_time = `Duration (in seconds)`,
  record_date = RecordedDate,
  completed,
  consent = consent_agree,
  condition,
  compre_1, compre_2, compre_3F, compre_3T, compre_4,
  match,
  ee, pn, ne,
  cee_1, cee_2, cee_3, cee_4, cee_5,
  cpn_1, cpn_2, cpn_3, cpn_4, cpn_5,
  cne_1, cne_2, cne_3, cne_4, cne_5,
  d_1:d_16, d_score, d_attention,
  age, gender, place_of_birth, place_of_residence = place_of_resi,
  attentive, error, which_bonus, bonus
)

##### Output clean datasets #####
write_csv(dat_clean, file = "processed_data/s1_data_cleaned.csv")
