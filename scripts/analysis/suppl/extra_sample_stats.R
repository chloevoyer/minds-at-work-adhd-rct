# Comorbidity Stats-------------------------------------------------------------
  # First, get total participants with comorbid disorders
  total_participants_with_comorbid <- analysis_data %>%
    filter(sd_dx_other == 1, !is.na(sd_dx_other_type_code)) %>%
    nrow()
  
  # Count disorders
  comorbid_summary <- analysis_data %>%
    filter(randomized == 1) %>%
    filter(sd_dx_other == 1, !is.na(sd_dx_other_type_code)) %>%
    separate_rows(sd_dx_other_type_code, sep = ",\\s*") %>%
    mutate(disorder = str_trim(sd_dx_other_type_code)) %>%
    filter(disorder != "") %>%  # Remove any empty strings
    count(disorder, sort = TRUE) %>%
    mutate(
      percentage_of_disorders = round(100 * n / sum(n), 1),
      percentage_of_participants = round(100 * n / total_participants_with_comorbid, 1),
      rank = row_number()
    ) %>%
    rename(disorder_code = disorder, frequency = n)
  
  # Add summary info
  cat("Total participants with comorbid disorders:", total_participants_with_comorbid, "\n")
  cat("Total disorder instances:", sum(comorbid_summary$frequency), "\n\n")
  
  print(comorbid_summary)
  
  dx_and_mx <- analysis_data %>%
    filter(randomized == 1 & sd_dx_adhd == 1 & sd_adhd_mx == 1) %>%
    nrow()
  dx_and_no_mx <- analysis_data %>%
    filter(randomized == 1 & sd_dx_adhd == 1 & sd_adhd_mx == 0) %>%
    nrow()
  
  no_dx_and_no_mx <- analysis_data %>%
    filter(randomized == 1 & sd_dx_adhd == 0 & sd_adhd_mx == 0) %>%
    nrow()
  print(dx_and_mx)
  print(no_dx_and_no_mx)
  
# Highly Educated Stats---------------------------------------------------------
  # Run the education summary on your main dataset
  education_data <- create_education_summary(analysis_data)
  
  # To get education summary by group (Control vs Experimental)
  group_education_data <- create_education_by_group(analysis_data)
  
# Percent change in outcome measures--------------------------------------------
  results <- calculate_percent_change(wide_data, completer_only = FALSE)  # Includes all participants
