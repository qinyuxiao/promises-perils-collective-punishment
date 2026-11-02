library(tidyverse)

# Set working directory to the directory of this script
misty::setsource()

dat_to_clean <- read_csv("processed_data/s2_data_anonymized_raw.csv")


# 1. Recode responses
dat_to_clean <- dat_to_clean |>
  mutate(
    pn = case_when(
      pn == "Very inappropriate" ~ 1,
      pn == "Inappropriate" ~ 2,
      pn == "Somewhat inappropriate" ~ 3,
      pn == "Somewhat appropriate" ~ 4,
      pn == "Appropriate" ~ 5,
      pn == "Very appropriate" ~ 6,
      TRUE ~ NA_real_
    ),
    ne = case_when(
      ne == "Very socially inappropriate" ~ 1,
      ne == "Socially inappropriate" ~ 2,
      ne == "Somewhat socially inappropriate" ~ 3,
      ne == "Somewhat socially appropriate" ~ 4,
      ne == "Socially appropriate" ~ 5,
      ne == "Very socially appropriate" ~ 6,
      TRUE ~ NA_real_
    ),
    attention = case_when(
      attention == "5\nVery attentive" ~ 5,
      attention == "4" ~ 4,
      attention == "3" ~ 3,
      attention == "2" ~ 2,
      attention == "1\nNot at all attentive" ~ 1,
      TRUE ~ NA_real_
    ),
    actual_match = case_when(
      actual_match == "No, I did not obtain a match." ~ 0,
      actual_match == "Yes, I actually obtained a match. I chose not to report one." ~ 1,
      TRUE ~ NA_real_
    )
  )


# 2. Add indicator variables for exclusion

# IDs associated with more than one response
# The ID with four responses is my own due to preview on Prolific
id_counts <- table(dat_to_clean$id)
id_with_more_than_one_response <- as.numeric(names(id_counts[id_counts > 1]))

dat_to_clean <- dat_to_clean |>
  mutate(
    # Responding to the survey more than once
    with_multiple_response = case_when(
      id %in% id_with_more_than_one_response ~ 1,
      TRUE ~ 0
    ),
    # Failing the honeypot
    fail_honeypot = as.integer(grepl("tremendously", purpose, ignore.case = TRUE)),
    # Likely a bot based on reCAPTCHA
    bot_based_on_recaptcha = case_when(
      Q_RecaptchaScore < 0.5 ~ 1,
      Q_RecaptchaScore >= 0.5 ~ 0,
      TRUE ~ 0
    ),
    # Duplicated responses based on Qualtrics fraud detection
    duplicated_respondent_qualtrics = case_when(
      Q_DuplicateRespondent ~ 1,
      TRUE ~ 0
    ),
    # Multiple submission based on Qualtrics fraud detection
    multiple_submission_qualtrics = case_when(
      Q_BallotBoxStuffing ~ 1,
      TRUE ~ 0
    ),
    # Attention lower than 4
    inattentive = case_when(
      attention < 4 ~ 1,
      attention >= 4 ~ 0,
      TRUE ~ 0
    )
  )


# 3. Generate the cleaned dataset
dat_cleaned <- dat_to_clean |>
  select(
    id,
    completed,
    completion_time = `Duration (in seconds)`,
    record_date = RecordedDate,
    consent,
    condition,
    mg_c1, mg_c2, mg_c3, mg_c4t, mg_c4f, mg_c5, mg_c6,
    match, ee, pn, ne,
    actual_match, explain_disadv_lie,
    age, gender, cob, cor,
    purpose, error, attention,
    CQ_first_errors,
    CQ_attempts,
    which_bonus, bonus,
    with_multiple_response,
    fail_honeypot,
    bot_based_on_recaptcha,
    duplicated_respondent_qualtrics,
    multiple_submission_qualtrics,
    inattentive,
    everything()
  )

# Save
# write_csv(dat_cleaned, file = "processed_data/s2_data_cleaned.csv")
