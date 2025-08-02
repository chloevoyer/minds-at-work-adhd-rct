create_summary <- function(data, group_name) {
  data <- data %>%
    filter(ID %in% as.character(vars$included_ids)) %>%
    filter(time %in% c(0, "Pre")) %>%
    mutate(
      # Create display versions as characters with proper ordering
      sd_sex_display = case_when(
        as.character(sd_sex) == "0" ~ "Male",
        as.character(sd_sex) == "1" ~ "Female",
        TRUE ~ as.character(sd_sex)
      ),
      sd_sex_display = factor(sd_sex_display, levels = c("Male", "Female")),
      
      sd_gender_display = case_when(
        as.character(sd_gender) == "0" ~ "Man",
        as.character(sd_gender) == "1" ~ "Woman",
        as.character(sd_gender) == "2" ~ "Other",
        TRUE ~ as.character(sd_gender)
      ),
      sd_gender_display = factor(sd_gender_display, levels = c("Man", "Woman", "Other")),
      
      sd_edu_display = case_when(
        as.character(sd_edu) == "1" ~ "High School",
        as.character(sd_edu) == "2" ~ "Vocational", 
        as.character(sd_edu) == "3" ~ "College",
        as.character(sd_edu) == "4" ~ "University",
        TRUE ~ as.character(sd_edu)
      ),
      sd_edu_display = factor(sd_edu_display, levels = c("High School", "Vocational", "College", "University")),
      
      sd_psyc_related = factor(sd_psyc_related, levels = c(0, 1), labels = c("No", "Yes")),
      
      ethnicity_collapsed = case_when(
        sd_ethnicity == "White" ~ "White",
        sd_ethnicity == "Black" ~ "Black",
        sd_ethnicity == "Indigenous" ~ "Indigenous",
        !is.na(sd_ethnicity) ~ "Other",
        TRUE ~ NA_character_
      ),
      
      sd_dx_adhd = case_when(
        as.character(sd_dx_adhd) == "1" ~ "Yes",
        as.character(sd_dx_adhd) == "0" ~ "No",
        TRUE ~ as.character(sd_dx_adhd)
      ),
      
      sd_dx_other = case_when(
        as.character(sd_dx_other) == "1" ~ "Yes",
        as.character(sd_dx_other) == "0" ~ "No",
        TRUE ~ as.character(sd_dx_other)
      ),
      
      sd_adhd_mx = case_when(
        as.character(sd_adhd_mx) == "1" ~ "Yes",
        as.character(sd_adhd_mx) == "0" ~ "No",
        TRUE ~ as.character(sd_adhd_mx)
      ),
      
      sd_tx = case_when(
        as.character(sd_tx) == "1" ~ "Yes",
        as.character(sd_tx) == "0" ~ "No",
        TRUE ~ as.character(sd_tx)
      ),
      
      # Job tenure categories (3-category version)
      job_tenure_cat3 = case_when(
        sd_job_tenure_yrs < 1 ~ "<1 year",
        sd_job_tenure_yrs >= 1 & sd_job_tenure_yrs <= 5 ~ "1-5 years", 
        sd_job_tenure_yrs > 5 ~ ">5 years",
        TRUE ~ NA_character_
      ),
      job_tenure_cat3 = factor(job_tenure_cat3, 
                                   levels = c("<1 year", "1-5 years", ">5 years")),
  
      # Job tenure categories (5-category version)
      job_tenure_cat5 = case_when(
        sd_job_tenure_yrs <= (1/12) ~ "≤1 mth",
        sd_job_tenure_yrs > (1/12) & sd_job_tenure_yrs <= 0.5 ~ "1-6 mths",
        sd_job_tenure_yrs > 0.5 & sd_job_tenure_yrs <= 1 ~ "6m-≤1 yr", 
        sd_job_tenure_yrs > 1 & sd_job_tenure_yrs <= 2 ~ "1-2 yrs",
        sd_job_tenure_yrs > 2 & sd_job_tenure_yrs <= 5 ~ "2-5 yrs",
        sd_job_tenure_yrs > 5 ~ " >5 yrs",
        TRUE ~ NA_character_
      ),
      
      job_tenure_cat5 = factor(job_tenure_cat5, 
                                   levels = c("≤1 mth", "1-6 mths", "6m-≤1 yr", "1-2 yrs", "2-5 yrs", " >5 yrs")
      ),
      
      sd_disclosure = case_when(
        as.character(sd_disclosure) == "1" ~ "Yes",
        as.character(sd_disclosure) == "0" ~ "No",
        TRUE ~ as.character(sd_disclosure)
      ),
      
      sd_acc = case_when(
        as.character(sd_acc) == "1" ~ "Yes",
        as.character(sd_acc) == "0" ~ "No",
        TRUE ~ as.character(sd_acc)
      )
    )
  
  data <- data %>%
    mutate(
      sd_lang_lower = tolower(sd_lang),
      lang_list = str_extract_all(sd_lang_lower, "french|english|spanish|arabic|italian|creole|chinese|russian|german|indigenous|other"),
      lang_count = lengths(lang_list),
      
      sd_languages = case_when(
        lang_count == 1 & str_detect(sd_lang_lower, "french") ~ "French",
        lang_count == 1 & str_detect(sd_lang_lower, "english") ~ "English",
        lang_count == 1 & str_detect(sd_lang_lower, "spanish|arabic|italian|creole|chinese|russian|german|indigenous|other") ~ "Other",
        lang_count >= 2|3 ~ "Multilingual",
        TRUE ~ NA_character_
      )
    ) %>%
    mutate(
      sd_languages = factor(sd_languages, levels = c("French", "English", "Bilingual", "Multilingual", "Other"))
    ) %>%
    select(-sd_lang_lower, -lang_list, -lang_count)
  
  n_total <- nrow(data)
  
  count_percent <- function(var) {
    data %>%
      group_by({{ var }}) %>%
      summarise(
        n = n(),
        percent = round(100 * n() / n_total, 1),
        .groups = "drop"
      ) %>%
      # Ensure factor variables are converted to character for proper display
      mutate(across(where(is.factor), as.character))
  }

  get_color_function <- function(group_name) {
    if (grepl("both|combined|total", tolower(group_name))) {
      return(function(x) crayon::bold(colours$orange(x)))
    } else if (grepl("control", tolower(group_name))) {
      return(function(x) crayon::bold(colours$red(x)))
    } else if (grepl("experimental|treatment", tolower(group_name))) {
      return(function(x) crayon::bold(colours$teal(x)))
    } else {
      return(function(x) x)  # No color if group not recognized
    }
  }
  
  color_func <- get_color_function(group_name)
  
  # Function to apply color to specific values
  colorize_value <- function(value, n, percent) {
    target_values <- c("Female", "Woman", "University", "White", "French", "Yes")
    # Add check for NA or empty values
    if (is.na(value) || length(value) == 0) {
      return(sprintf("  %s: n = %d (%.1f%%)", "Missing", n, percent))
    } else if (value %in% target_values) {
      return(color_func(sprintf("  %s: n = %d (%.1f%%)", value, n, percent)))
    } else {
      return(sprintf("  %s: n = %d (%.1f%%)", value, n, percent))
    }
  }
  
  print_summary <- function(label, df, var_col) {
    cat("\n", label, ":\n", sep = "")
    for (i in 1:nrow(df)) {
      val <- df[[var_col]][i]
      # Convert factor to character to get labels instead of numbers
      if(is.factor(val)) {
        val <- as.character(val)
      }
      val <- ifelse(is.na(val), "Missing", val)
      colored_output <- colorize_value(val, df$n[i], df$percent[i])
      cat(colored_output, "\n")
    }
  }
  
  age_stats <- data %>%
    summarise(
      mean_age = mean(sd_age, na.rm = TRUE),
      sdv_age = sd(sd_age, na.rm = TRUE),
      min_age = min(sd_age, na.rm = TRUE),
      max_age = max(sd_age, na.rm = TRUE)
    )
  
  working_hrs_stats <- data %>%
    summarise(
      mean_working_hrs = mean(sd_real_hrs, na.rm = TRUE),
      sd_working_hrs = sd(sd_real_hrs, na.rm = TRUE),
      min_working_hrs = min(sd_real_hrs, na.rm = TRUE),
      max_working_hrs = max(sd_real_hrs, na.rm = TRUE)
    )
  
  job_tenure_stats <- data %>%
    summarise(
      mean_job_tenure = mean(sd_job_tenure_yrs, na.rm = TRUE),
      sd_job_tenure = sd(sd_job_tenure_yrs, na.rm = TRUE),
      min_job_tenure = min(sd_job_tenure_yrs, na.rm = TRUE),
      max_job_tenure = max(sd_job_tenure_yrs, na.rm = TRUE)
    )
  
  age_dx_adhd_stats <- data %>%
    summarise(
      mean_age_dx_adhd = mean(sd_dx_start_age_yrs, na.rm = TRUE),
      sd_age_dx_adhd = sd(sd_dx_start_age_yrs, na.rm = TRUE),
      min_age_dx_adhd = min(sd_dx_start_age_yrs, na.rm = TRUE),
      max_age_dx_adhd = max(sd_dx_start_age_yrs, na.rm = TRUE)
    )
  
  ASRS_stats <- data %>%
    summarise(
      mean_asrs_A = mean(ASRS_A_total, na.rm = TRUE),
      sd_asrs_A = sd(ASRS_A_total, na.rm = TRUE),
      mean_asrs_B = mean(ASRS_B_total, na.rm = TRUE),
      sd_asrs_B = sd(ASRS_B_total, na.rm = TRUE),
      mean_asrs_total = mean(ASRS_total, na.rm = TRUE),
      sd_asrs_total = sd(ASRS_total, na.rm = TRUE)
    )
  
  age_and_recent_dx_summary <- data %>%
    filter(!is.na(sd_dx_start_age_yrs), !is.na(sd_age)) %>%
    mutate(years_since_diagnosis = sd_age - sd_dx_start_age_yrs) %>%
    summarise(
      # Age at diagnosis groups
      age_12_or_less_n = sum(sd_dx_start_age_yrs <= 12),
      age_12_or_less_pct = round(100 * sum(sd_dx_start_age_yrs <= 12) / n(), 1),
      age_18_or_less_n = sum(sd_dx_start_age_yrs <= 18),
      age_18_or_less_pct = round(100 * sum(sd_dx_start_age_yrs <= 18) / n(), 1),
      age_over_18_n = sum(sd_dx_start_age_yrs > 18),
      age_over_18_pct = round(100 * sum(sd_dx_start_age_yrs > 18) / n(), 1),
      
      # Recent diagnosis (past 5 years)
      recent_dx_n = sum(years_since_diagnosis <= 5),
      recent_dx_pct = round(100 * sum(years_since_diagnosis <= 5) / n(), 1),
      older_dx_n = sum(years_since_diagnosis > 5),
      older_dx_pct = round(100 * sum(years_since_diagnosis > 5) / n(), 1),
      
      total_valid = n()
    )
  
  # Print header
  cat("\n", group_name, " group (N = ", n_total, ")\n", sep = "")
  cat(strrep("-", 40), "\n")
  
  # Color the age output
  age_output <- sprintf("M = %.2f, SD = %.2f, range = %.1f – %.1f", age_stats$mean_age, age_stats$sdv_age, age_stats$min_age, age_stats$max_age)
  cat("Age:", color_func(age_output), "\n")
  
  # Summaries
  print_summary("Sex", count_percent(sd_sex_display), "sd_sex_display")
  print_summary("Gender", count_percent(sd_gender_display), "sd_gender_display")
  print_summary("Education", count_percent(sd_edu_display), "sd_edu_display")
  print_summary("Psych-related field", count_percent(sd_psyc_related), "sd_psyc_related")
  print_summary("Ethnicity (collapsed)", count_percent(ethnicity_collapsed), "ethnicity_collapsed")
  
  # Languages (special handling)
  language_stats <- count_percent(sd_languages)
  cat("\nLanguages Spoken:\n")
  for (i in seq_len(nrow(language_stats))) {
    lang_val <- as.character(language_stats$sd_languages[i])
    colored_output <- colorize_value(lang_val, language_stats$n[i], language_stats$percent[i])
    cat(colored_output, "\n")
  }
  
  print_summary("Concurrent psychological treatment", count_percent(sd_tx), "sd_tx")
  print_summary("ADHD Diagnosis", count_percent(sd_dx_adhd), "sd_dx_adhd")
  
  # Color the job tenure output
  age_dx_adhd_output <- sprintf("M = %.2f, SD = %.2f, range = %.1f – %.1f", age_dx_adhd_stats$mean_age_dx_adhd, age_dx_adhd_stats$sd_age_dx_adhd, age_dx_adhd_stats$min_age_dx_adhd, age_dx_adhd_stats$max_age_dx_adhd)
  cat("\nMean Age ADHD Dx:", color_func(age_dx_adhd_output), "\n")
  
  cat("\nAge at ADHD Diagnosis:\n")
  age_under12_output <- sprintf("  ≤12 years: n = %d (%.1f%%)", 
                                age_and_recent_dx_summary$age_12_or_less_n, 
                                age_and_recent_dx_summary$age_12_or_less_pct)
  age_under18_output <- sprintf("  ≤18 years: n = %d (%.1f%%)", 
                                age_and_recent_dx_summary$age_18_or_less_n, 
                                age_and_recent_dx_summary$age_18_or_less_pct)
  age_over18_output <- sprintf("  >18 years: n = %d (%.1f%%)", 
                               age_and_recent_dx_summary$age_over_18_n, 
                               age_and_recent_dx_summary$age_over_18_pct)
  cat(age_under12_output, "\n")
  cat(age_under18_output, "\n")
  cat(color_func(age_over18_output), "\n")  # Highlight the ≤18 group
  
  cat("\nTime Since ADHD Diagnosis:\n")
  recent_dx_output <- sprintf("  ≤5 years: n = %d (%.1f%%)", 
                              age_and_recent_dx_summary$recent_dx_n, 
                              age_and_recent_dx_summary$recent_dx_pct)
  older_dx_output <- sprintf("  >5 years: n = %d (%.1f%%)", 
                             age_and_recent_dx_summary$older_dx_n, 
                             age_and_recent_dx_summary$older_dx_pct)
  cat(color_func(recent_dx_output), "\n")  # Highlight the recent diagnosis group
  cat(older_dx_output, "\n")
  
  print_summary("ADHD Medication Use", count_percent(sd_adhd_mx), "sd_adhd_mx")
  print_summary("Psychiatric Comorbidity", count_percent(sd_dx_other), "sd_dx_other")
  print_summary("ADHD Dx Disclosure at Work", count_percent(sd_disclosure), "sd_disclosure")
  print_summary("ADHD Accommodations", count_percent(sd_acc), "sd_acc")
  
  # Color the avg working hours output
  working_hrs_output <- sprintf("M = %.2f, SD = %.2f, range = %.1f – %.1f", working_hrs_stats$mean_working_hrs, working_hrs_stats$sd_working_hrs, working_hrs_stats$min_working_hrs, working_hrs_stats$max_working_hrs)
  cat("\nWorking hours:", color_func(working_hrs_output), "\n")
  
  # Color the job tenure output
  job_tenure_output <- sprintf("M = %.2f, SD = %.2f, range = %.1f – %.1f", job_tenure_stats$mean_job_tenure, job_tenure_stats$sd_job_tenure, job_tenure_stats$min_job_tenure, job_tenure_stats$max_job_tenure)
  cat("\nJob tenure:", color_func(job_tenure_output), "\n")
  print_summary("Job tenure categories", count_percent(job_tenure_cat3), "job_tenure_cat3")
  print_summary("Job tenure categories", count_percent(job_tenure_cat5), "job_tenure_cat5")
  
  # Color the ASRS output - REORDERED: Inattention first, then Hyperactivity, then Total
  asrs_a_output <- sprintf("M = %.2f, SD = %.2f", ASRS_stats$mean_asrs_A, ASRS_stats$sd_asrs_A)
  asrs_b_output <- sprintf("M = %.2f, SD = %.2f", ASRS_stats$mean_asrs_B, ASRS_stats$sd_asrs_B)
  asrs_total_output <- sprintf("M = %.2f, SD = %.2f", ASRS_stats$mean_asrs_total, ASRS_stats$sd_asrs_total)
  cat("\nASRS:", "\n  Inattention: ", color_func(asrs_b_output), "\n  Hyperactivity: ", color_func(asrs_a_output), "\n  Total: ", color_func(asrs_total_output), "\n")
  
}


summary_by_group_total <- function(data, group_var, value_var, label = "ASRS Mean Summary") {
  
  # Function to get the appropriate color for each group
  get_color_for_group <- function(group_name) {
    if (group_name == 0) {
      return(function(x) crayon::bold(colours$red(x)))
    } else if (group_name == 1) {
      return(function(x) crayon::bold(colours$teal(x)))
    } else if (group_name == "Total") {
      return(function(x) crayon::bold(colours$orange(x)))
    } else {
      return(function(x) x)  # No color for unrecognized groups
    }
  }
  
  # Define group labels if group_var is numeric
  data <- data %>%
    filter(ID %in% as.character(vars$included_ids)) %>%
    filter(time %in% c(0, "Pre")) %>%
    mutate(
      group_label = case_when(
        {{ group_var }} == 0 ~ "Control",
        {{ group_var }} == 1 ~ "Experimental",
        TRUE ~ as.character({{ group_var }})
      )
    )
  
  # group-level summary
  by_group <- data %>%
    group_by(group_label) %>%
    summarise(
      mean = mean({{ value_var }}, na.rm = TRUE),
      sd = sd({{ value_var }}, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Total summary
  total <- data %>%
    summarise(
      group_label = "Total",
      mean = mean({{ value_var }}, na.rm = TRUE),
      sd = sd({{ value_var }}, na.rm = TRUE)
    )
  
  # Combine and reorder: Experimental first, then Control, then Total
  summary_table <- bind_rows(by_group, total) %>%
    mutate(group_label = factor(group_label, levels = c("Experimental", "Control", "Total"))) %>%
    arrange(group_label)
  
  # Print with colors
  cat("\n", label, ": M (SD)\n", sep = "")
  for (i in seq_len(nrow(summary_table))) {
    group_name <- as.character(summary_table$group_label[i])
    color_func <- get_color_for_group(group_name)
    
    # Create the formatted string
    formatted_line <- sprintf("  %s = %.2f (%.2f)", 
                              group_name, 
                              summary_table$mean[i], 
                              summary_table$sd[i])
    
    # Apply color and print
    cat(color_func(formatted_line), "\n")
  }
  
  invisible(summary_table)
}