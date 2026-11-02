Scripts

- s1_power_analysis.R: This script conducts the power and sensitivity analyses for the pilot study, as reported in the 'pre-registration'. However, note that the parameters used in the analyses differed vastly from what was eventually observed. Therefore, they may have little referential value.

- s1_preprocessing.R: This script imports the raw data file for the pilot study (CP_STUDY1_RAW_DO_NOT_EDIT.csv), removes those who did not consent to participate, calculates bonus payments (stored in BONUS.csv), and anonymizes the dataset. These data files are not shared publicly. The output is 's1_data_anonymized_raw.csv'.

- s1_cleaning.R: This script imports the anonymized raw data file (s1_data_anonymized_raw.csv) and does the following: (i) recodes the scale responses, which were coded as texts in the raw data file, as numbers; (ii) reverse-scores D16 responses and computes the D-score by taking their average; (iii) adds subject IDs and removes unnecessary columns; (iv) outputs cleaned data file ('s1_data_cleaned.csv').

- s1_analysis.qmd: This Quarto file imports s1_data_cleaned.csv and does all analyses related to the pilot study. The rendered .html file is s1_analysis.html.

- s2_power_analysis.qmd: This Quarto file conducts the power analysis for the registered study. It is rendered into s2_power_analysis.html.

- The scripts starting with 'figure_' are used to create figures.