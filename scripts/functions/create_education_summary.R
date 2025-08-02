# Education Level Summary Analysis
# ===================================

# Load required libraries if not already loaded
# library(dplyr)
# library(knitr)

# Function to create education summary (replace 'your_data' with your actual dataset name)
create_education_summary <- function(data) {
  
  # Filter to included participants if using analysis subset
  if(exists("vars$included_ids")) {
    data <- data %>% filter(ID %in% vars$included_ids)
  }
  
  # 1. Basic summary of sd_edu (primary education level)
  cat("=== PRIMARY EDUCATION LEVEL SUMMARY (sd_edu) ===\n")
  edu_summary <- data %>%
    count(sd_edu, name = "n") %>%
    mutate(
      percent = round(100 * n / sum(n), 1),
      cum_percent = round(cumsum(percent), 1)
    ) %>%
    arrange(sd_edu)
  
  print(edu_summary)
  
  # 2. University cycle summary (sd_uni_cycle)
  cat("\n=== UNIVERSITY DEGREE LEVEL SUMMARY (sd_uni_cycle) ===\n")
  uni_summary <- data %>%
    mutate(
      uni_level = case_when(
        is.na(sd_uni_cycle) ~ "No University Degree",
        sd_uni_cycle == 1 ~ "Bachelor's/Undergraduate",
        sd_uni_cycle == 2 ~ "Post-graduate",
        TRUE ~ "Other"
      )
    ) %>%
    count(uni_level, name = "n") %>%
    mutate(percent = round(100 * n / sum(n), 1))
  
  print(uni_summary)
  
  # 3. Combined education hierarchy
  cat("\n=== COMBINED EDUCATION HIERARCHY ===\n")
  combined_summary <- data %>%
    mutate(
      education_level = case_when(
        # Post-graduate level
        sd_uni_cycle == 2 ~ "Post-graduate",
        # University undergraduate
        sd_uni_cycle == 1 ~ "University (Bachelor's)",
        # Non-university education levels
        sd_edu == "College" ~ "College",
        sd_edu == "Vocational" ~ "Vocational",
        sd_edu == "High School" ~ "High School",
        sd_edu == "Other" ~ "Other Education",
        # Handle any missing cases
        TRUE ~ "Unspecified"
      ),
      # Create broader categories
      education_category = case_when(
        sd_uni_cycle %in% c(1, 2) ~ "University Degree",
        sd_edu %in% c("High School", "Vocational", "College") ~ "Non-University",
        TRUE ~ "Other/Unspecified"
      )
    ) %>%
    count(education_level, name = "n") %>%
    mutate(percent = round(100 * n / sum(n), 1)) %>%
    arrange(desc(n))
  
  print(combined_summary)
  
  # 4. University vs Non-University breakdown
  cat("\n=== UNIVERSITY vs NON-UNIVERSITY BREAKDOWN ===\n")
  broad_summary <- data %>%
    mutate(
      education_category = case_when(
        sd_uni_cycle %in% c(1, 2) ~ "University Degree",
        sd_edu %in% c("High School", "Vocational", "College") ~ "Non-University",
        TRUE ~ "Other/Unspecified"
      )
    ) %>%
    count(education_category, name = "n") %>%
    mutate(percent = round(100 * n / sum(n), 1))
  
  print(broad_summary)
  
  # 5. Detailed cross-tabulation
  cat("\n=== DETAILED CROSS-TABULATION ===\n")
  cross_tab <- data %>%
    mutate(
      uni_cycle_label = case_when(
        is.na(sd_uni_cycle) ~ "No Uni Degree",
        sd_uni_cycle == 1 ~ "Bachelor's",
        sd_uni_cycle == 2 ~ "Post-graduate",
        TRUE ~ "Other"
      )
    ) %>%
    count(sd_edu, uni_cycle_label, name = "n") %>%
    arrange(sd_edu, uni_cycle_label)
  
  print(cross_tab)
  
  # 6. Summary statistics
  cat("\n=== KEY STATISTICS ===\n")
  total_n <- nrow(data)
  uni_degree_n <- sum(data$sd_uni_cycle %in% c(1, 2), na.rm = TRUE)
  postgrad_n <- sum(data$sd_uni_cycle == 2, na.rm = TRUE)
  non_uni_n <- sum(data$sd_edu %in% c("High School", "Vocational", "College"), na.rm = TRUE)
  
  cat("Total participants:", total_n, "\n")
  cat("University degree holders:", uni_degree_n, "(", round(100 * uni_degree_n / total_n, 1), "%)\n")
  cat("Post-graduate degree holders:", postgrad_n, "(", round(100 * postgrad_n / total_n, 1), "%)\n")
  cat("Non-university education:", non_uni_n, "(", round(100 * non_uni_n / total_n, 1), "%)\n")
  
  # Return the processed data for further analysis if needed
  final_data <- data %>%
    mutate(
      education_level = case_when(
        sd_uni_cycle == 2 ~ "Post-graduate",
        sd_uni_cycle == 1 ~ "University (Bachelor's)",
        sd_edu == "College" ~ "College",
        sd_edu == "Vocational" ~ "Vocational",
        sd_edu == "High School" ~ "High School",
        sd_edu == "Other" ~ "Other Education",
        TRUE ~ "Unspecified"
      ),
      education_category = case_when(
        sd_uni_cycle %in% c(1, 2) ~ "University Degree",
        sd_edu %in% c("High School", "Vocational", "College") ~ "Non-University",
        TRUE ~ "Other/Unspecified"
      ),
      is_postgraduate = sd_uni_cycle == 2,
      has_university_degree = sd_uni_cycle %in% c(1, 2)
    )
  
  return(invisible(final_data))
}

# Usage examples:
# For your main dataset:
# education_data <- create_education_summary(merged_data)

# For analysis subset:
# education_data <- create_education_summary(analysis_data)

# For sociodemographic subset:
# education_data <- create_education_summary(sociodemo_data)

# To create a summary by group:
create_education_by_group <- function(data) {
  if(exists("vars$included_ids")) {
    data <- data %>% filter(ID %in% vars$included_ids)
  }
  
  cat("\n=== EDUCATION SUMMARY BY group ===\n")
  
  group_summary <- data %>%
    mutate(
      education_category = case_when(
        sd_uni_cycle == 2 ~ "Post-graduate",
        sd_uni_cycle == 1 ~ "University (Bachelor's)",
        sd_edu %in% c("High School", "Vocational", "College") ~ "Non-University",
        TRUE ~ "Other/Unspecified"
      )
    ) %>%
    count(group, education_category, name = "n") %>%
    group_by(group) %>%
    mutate(
      total_group = sum(n),
      percent = round(100 * n / total_group, 1)
    ) %>%
    ungroup()
  
  print(group_summary)
  
  # Create a nice table format
  wide_summary <- group_summary %>%
    select(group, education_category, n, percent) %>%
    mutate(display = paste0(n, " (", percent, "%)")) %>%
    select(-n, -percent) %>%
    tidyr::pivot_wider(names_from = education_category, values_from = display, values_fill = "0 (0%)")
  
  cat("\n=== FORMATTED TABLE BY group ===\n")
  print(wide_summary)
  
  return(invisible(group_summary))
}

# Usage:
# group_education_data <- create_education_by_group(analysis_data)
