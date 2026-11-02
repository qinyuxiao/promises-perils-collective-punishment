library(tidyverse)
library(janitor)


# Set the working directory to the one of this script
misty::setsource()


# Read raw data (before anonymization)
dat_raw <- read_csv("raw_data/CP_STUDY2_RAW_DO_NOT_EDIT.csv")
dat_raw <- dat_raw[-c(1, 2),]  # Remove first two rows (Qualtrics metadata)


# Remove those who did not consent
dat_consented <- dat_raw |> filter(
  consent == "I have read and understood the above. I agree to participate."
)
nrow(dat_consented) # n = 3,396 (all who consented to participate)


# Do all responses have an associated Prolific ID?
sum(is.na(dat_consented$PROLIFIC_PID)) # There is one NA
dat_consented[is.na(dat_consented$PROLIFIC_PID), "attention"]


# Assign a participant ID to each unique Prolific ID (NA remains NA)
dat_consented <- dat_consented |>
  mutate(id = as.integer(factor(PROLIFIC_PID)))


# Recode match reports
dat_consented <- dat_consented |>
  mutate(
    match = case_when(
      match == "Yes, I obtained a match." ~ 1,
      match == "No, I did not obtain a match." ~ 0,
      TRUE ~ NA
    )
  )


# Only those who completed the survey will receive flat payment and bonus
# Use the last compulsory question `attention` as indicator of completion
dat_consented <- dat_consented |> mutate(completed = as.numeric(!is.na(attention)))
dat_completed <- dat_consented |> filter(completed == 1)
nrow(dat_completed)


# Did any participant have more than one completion?
any(duplicated(dat_completed$id))
anyDuplicated(dat_completed$id)


# Determine whether to apply collective punishment in the CP condition (yes)
dat_completed |>
  tabyl(condition, match) |>
  adorn_totals(where = "col") |>
  adorn_percentages(denominator = "row") |>
  adorn_pct_formatting() |>
  adorn_ns(position = "front")

# Output:

# condition           0           1          Total
#  baseline 466 (42.3%) 635 (57.7%) 1,101 (100.0%)
#        cp 583 (53.1%) 515 (46.9%) 1,098 (100.0%)
#      norm 607 (55.0%) 496 (45.0%) 1,103 (100.0%)


# Normative expectation: "Socially inappropriate" was the mode across conditions
dat_completed |>
  tabyl(ne, condition) |>
  adorn_totals(where = "row") |>
  adorn_percentages(denominator = "col") |>
  adorn_pct_formatting() |>
  adorn_ns(position = "front")

# Output:

#                              ne       baseline             cp           norm
#            Socially appropriate    79   (7.2%)    63   (5.7%)    48   (4.4%)
#          Socially inappropriate   406  (36.9%)   415  (37.8%)   455  (41.3%)
#   Somewhat socially appropriate    86   (7.8%)    60   (5.5%)    70   (6.3%)
# Somewhat socially inappropriate   313  (28.4%)   200  (18.2%)   260  (23.6%)
#       Very socially appropriate    29   (2.6%)    26   (2.4%)    14   (1.3%)
#     Very socially inappropriate   188  (17.1%)   334  (30.4%)   256  (23.2%)
#                           Total 1,101 (100.0%) 1,098 (100.0%) 1,103 (100.0%)


# Determine which bonus to payout
set.seed(2026)
dat_completed$which_bonus <- sample(
  c("match", "ne"),
  size = nrow(dat_completed),
  replace = TRUE
)


# Determine bonus (sum = 1,227.6, mean = 0.372, SD = 0.424)
dat_completed$bonus <- NA
dat_completed <- dat_completed |>
  mutate(
    bonus = case_when(
      which_bonus == "match" & condition == "baseline" & match == 1 ~ 1,
      which_bonus == "match" & condition == "baseline" & match == 0 ~ 0.2,
      which_bonus == "match" & condition == "cp" & match == 1 ~ 1,
      which_bonus == "match" & condition == "cp" & match == 0 ~ 0,
      which_bonus == "match" & condition == "norm" & match == 1 ~ 1.2,
      which_bonus == "match" & condition == "norm" & match == 0 ~ 0.2,
      which_bonus == "ne" & ne == "Socially inappropriate" ~ 0.4,
      which_bonus == "ne" & ne != "Socially inappropriate" ~ 0,
      TRUE ~ NA_real_
    )
  )


# Which data collection
dat_completed <- dat_completed |> mutate(
  batch = case_when(
    str_starts(StartDate, "2026-09-01") ~ 1,
    str_starts(StartDate, "2026-09-02") ~ 2,
    TRUE ~ NA_real_
  )
)


# Output
batch_1_approve <- dat_completed |> filter(batch == 1) |> select(PROLIFIC_PID)
batch_2_approve <- dat_completed |> filter(batch == 2) |> select(PROLIFIC_PID)
batch_1_bonus <- dat_completed |> filter(batch == 1, bonus > 0) |> select(PROLIFIC_PID, bonus)
batch_2_bonus <- dat_completed |> filter(batch == 2, bonus > 0) |> select(PROLIFIC_PID, bonus)


# Anonymization and merge with bonus data
dat_anonymous <- dat_consented |> select(-c(
  "IPAddress", "LocationLatitude", "LocationLongitude", "pid", "PROLIFIC_PID"
))
dat_anonymous <- dat_anonymous |> left_join(
  dat_completed |> select(id, batch, which_bonus, bonus),
  by = "id"
)


# Save
# write_csv(batch_1_approve, file = "raw_data/CP_STUDY2_TO_APPROVE_1.csv")
# write_csv(batch_2_approve, file = "raw_data/CP_STUDY2_TO_APPROVE_2.csv")
# write_csv(batch_1_bonus, file = "raw_data/CP_STUDY2_BONUS_1.csv")
# write_csv(batch_2_bonus, file = "raw_data/CP_STUDY2_BONUS_2.csv")
# write_csv(
#   dat_anonymous,
#   file = "processed_data/s2_data_anonymized_raw.csv"
# )
