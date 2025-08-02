# Function to calculate percentage change for QWLQ and CFWQ scores by group
calculate_percent_change <- function(data, completer_only = FALSE) {
  
  # Filter for completer if specified
  if (completer_only) {
    data <- data %>% filter(completer == 1)
  }
  
  # Calculate group means and % changes
  summary_stats <- data %>%
    filter(!is.na(group)) %>%
    group_by(group) %>%
    summarise(
      n = n(),
      
      # QWLQ Statistics
      QWLQ_pre_mean = mean(QWLQ_total_pre, na.rm = TRUE),
      QWLQ_post_mean = mean(QWLQ_total_post, na.rm = TRUE),
      QWLQ_absolute_change_mean = mean(QWLQ_total_post - QWLQ_total_pre, na.rm = TRUE),
      QWLQ_absolute_change_sd = sd(QWLQ_total_post - QWLQ_total_pre, na.rm = TRUE),
      
      # CFWQ Statistics
      CFWQ_pre_mean = mean(CFWQ_total_pre, na.rm = TRUE),
      CFWQ_post_mean = mean(CFWQ_total_post, na.rm = TRUE),
      CFWQ_absolute_change_mean = mean(CFWQ_total_post - CFWQ_total_pre, na.rm = TRUE),
      CFWQ_absolute_change_sd = sd(CFWQ_total_post - CFWQ_total_pre, na.rm = TRUE),
      
      .groups = 'drop'
    ) %>%
    # Calculate percentage changes from group means
    mutate(
      QWLQ_percent_change = round(((QWLQ_post_mean - QWLQ_pre_mean) / QWLQ_pre_mean) * 100, 1),
      CFWQ_percent_change = round(((CFWQ_post_mean - CFWQ_pre_mean) / CFWQ_pre_mean) * 100, 1)
    ) %>%
    # Round other statistics
    mutate(
      across(contains("mean") | contains("sd"), ~ round(.x, 2))
    )
  
  # Create a formatted summary for easy reporting
  formatted_summary <- summary_stats %>%
    select(group, n, QWLQ_percent_change, CFWQ_percent_change) %>%
    pivot_longer(
      cols = c(QWLQ_percent_change, CFWQ_percent_change),
      names_to = "measure",
      values_to = "percent_change"
    ) %>%
    mutate(
      measure = case_when(
        measure == "QWLQ_percent_change" ~ "QWLQ",
        measure == "CFWQ_percent_change" ~ "CFWQ"
      )
    ) %>%
    pivot_wider(
      names_from = group,
      values_from = percent_change
    )
  
  # Print formatted results
  cat("=== PERCENTAGE CHANGE SUMMARY ===\n\n")
  
  cat("QWLQ (Quality of Work Life):\n")
  qwlq_exp <- summary_stats$QWLQ_percent_change[summary_stats$group == 1]
  qwlq_ctrl <- summary_stats$QWLQ_percent_change[summary_stats$group == 0]
  cat(sprintf("  Experimental: %.1f%% change\n", qwlq_exp))
  cat(sprintf("  Control: %.1f%% change\n", qwlq_ctrl))
  
  cat("\nCFWQ (Cognitive Functioning at Work):\n")
  cfwq_exp <- summary_stats$CFWQ_percent_change[summary_stats$group == 1]
  cfwq_ctrl <- summary_stats$CFWQ_percent_change[summary_stats$group == 0]
  cat(sprintf("  Experimental: %.1f%% change\n", cfwq_exp))
  cat(sprintf("  Control: %.1f%% change\n", cfwq_ctrl))
  
  cat("\n=== SAMPLE SENTENCES FOR REPORTING ===\n\n")
  
  # Generate sample sentences
  qwlq_sentence <- sprintf(
    "Intervention participants showed a %.1f%% %s in QWLQ over the study period, versus %.1f%% in controls.",
    abs(qwlq_exp),
    ifelse(qwlq_exp >= 0, "increase", "decrease"),
    qwlq_ctrl
  )
  
  cfwq_sentence <- sprintf(
    "For CFWQ, intervention participants showed a %.1f%% %s, compared to %.1f%% in controls.",
    abs(cfwq_exp),
    ifelse(cfwq_exp >= 0, "increase", "decrease"),
    cfwq_ctrl
  )
  
  cat("QWLQ:", qwlq_sentence, "\n")
  cat("CFWQ:", cfwq_sentence, "\n\n")
  
  # Return comprehensive results
  results <- list(
    detailed_summary = summary_stats,
    formatted_summary = formatted_summary,
    sample_sentences = list(
      QWLQ = qwlq_sentence,
      CFWQ = cfwq_sentence
    )
  )
  
  return(results)
}

# Usage examples:
# results <- calculate_percent_change(analysis_data, completer_only = TRUE)  # Includes all participants

# To access specific components:
# results$detailed_summary          # Full statistical summary
# results$formatted_summary         # Simplified comparison table
# results$individual_changes        # Individual participant changes
# results$sample_sentences          # Ready-to-use sentences for reporting
