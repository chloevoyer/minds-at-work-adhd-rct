# ==============================================================================
# Preliminary analyses for adverse effects
# ==============================================================================

# Step 1: Filter and prepare data for adverse effects analysis
  adverse_data <- long_data %>%
    filter(cohort == 1 & consent == 1) %>%
    filter(time %in% c(0, 1)) %>%
    select(ID, group, time, completer, QWLQ_total, CFWQ_total) %>%
    arrange(ID, time)

# Step 2: Calculate dropout rates by group
  results$adverse_effects$dropout_rates <- adverse_data %>%
    filter(time == 1) %>%  # Use post-intervention data for dropout calculation
    group_by(group) %>%
    dplyr::summarize(
      total_participants = n(),
      completer_count = sum(completer == 1, na.rm = TRUE),
      dropouts_count = sum(completer == 0, na.rm = TRUE),
      dropout_rate = round(dropouts_count / total_participants * 100, 1),
      .groups = 'drop'
    )
  
    print("Dropout rates by group for cohort 1:")
    print(results$adverse_effects$dropout_rates)

# Step 3: Prepare data for paired comparisons (complete cases only)
  paired_data <- adverse_data %>%
    group_by(ID) %>%
    filter(n() == 2) %>%  # Only participants with both pre and post data
    arrange(time) %>%
    summarise(
      group = first(group),
      completer = first(completer),
      QWLQ_pre = first(QWLQ_total),
      QWLQ_post = last(QWLQ_total),
      CFWQ_pre = first(CFWQ_total),
      CFWQ_post = last(CFWQ_total),
      QWLQ_change = QWLQ_post - QWLQ_pre,
      CFWQ_change = CFWQ_post - CFWQ_pre,
      .groups = 'drop'
    ) %>%
    mutate(group = factor(group, levels = c(0, 1)))

# Step 4: Check normality for change scores
  check_normality(paired_data, "QWLQ_change")
  check_normality(paired_data, "CFWQ_change")

# Additional check for homogeneity of variances (for between-group t-test)
  cat("\n===== Checking homogeneity of variances =====\n")
  car::leveneTest(QWLQ_change ~ group, data = paired_data)
  car::leveneTest(CFWQ_change ~ group, data = paired_data)

# Summary recommendation
  cat("\n===== Summary of Assumption Checks =====\n")
  cat("Based on these checks, here are the recommended approaches:\n\n")
  provide_recommendation(paired_data, "QWLQ_change")
  provide_recommendation(paired_data, "CFWQ_change")

# Step 5: Perform both parametric and non-parametric tests for outcome changes
  # Run the analyses for both outcomes
  results$adverse_effects$qwl_results <- analyze_outcome_both_tests(
    paired_data,
    "QWLQ_pre", "QWLQ_post", "Quality of Work Life"
  )
  
  results$adverse_effects$cfw_results <- analyze_outcome_both_tests(
    paired_data,
    "CFWQ_pre", "CFWQ_post", "Cognitive Functioning at Work"
  )
  
# Step 5: Check for adverse effects in both outcomes
  cat("\n\n===== ADVERSE EFFECTS ASSESSMENT =====\n")
  results$adverse_effects$qwl_adverse <- check_adverse_effects(results$adverse_effects$qwl_results)
  results$adverse_effects$cfw_adverse <- check_adverse_effects(results$adverse_effects$cfw_results)

# Overall safety assessment
  cat("\n\n===== OVERALL SAFETY ASSESSMENT =====\n")
  if(results$adverse_effects$qwl_adverse || results$adverse_effects$cfw_adverse) {
    cat("\nPOTENTIAL ADVERSE EFFECTS DETECTED in at least one outcome measure.\n")
    cat("Further investigation recommended before proceeding with additional cohorts.\n")
  } else {
    cat("\nNO ADVERSE EFFECTS DETECTED in any outcome measure.\n")
    cat("Both parametric and non-parametric tests confirm the absence of significant\n")
    cat("decreases in experimental group outcomes or significant disadvantages\n")
    cat("compared to the control group.\n")
    cat("\nIt appears safe to continue with subsequent cohorts.\n")
  }

# Step 7: Create visualization data that matches function expectations
# Need to include group.factor and time.factor for proper labeling
  results$adverse_effects$qwl_plot_data <- long_data %>%
    filter(cohort == 1 & consent == 1) %>%
    filter(time %in% c(0, 1)) %>%  # Only pre and post
    select(ID, group, group.factor, time, time.factor, QWLQ_total) %>%
    filter(!is.na(QWLQ_total)) %>%
    mutate(
      time.factor = factor(time.factor, levels = c("Pre", "Post")),  # Only keep relevant levels
      # Create unique group factor mapping per ID to avoid join issues
      group.factor = first(group.factor),
      .by = ID
    )
  
  results$adverse_effects$cfw_plot_data <- long_data %>%
    filter(cohort == 1 & consent == 1) %>%
    filter(time %in% c(0, 1)) %>%  # Only pre and post
    select(ID, group, group.factor, time, time.factor, CFWQ_total) %>%
    filter(!is.na(CFWQ_total)) %>%
    mutate(
      time.factor = factor(time.factor, levels = c("Pre", "Post")),  # Only keep relevant levels
      # Create unique group factor mapping per ID to avoid join issues
      group.factor = first(group.factor),
      .by = ID
    )
  
# Step 8: Create plots using the properly formatted data
plots$adverse_effects$qwl_plot <- create_raw_data_plot(
  outcome_name = "QWL",
  y_axis_label = "Quality of Work Life Raw Total Score",
  plot_title = "Changes in Quality of Work Life (Cohort 1)",
  show_error_bars = FALSE,
  error_bar_type = "se",
  error_bar_width = 0.05,
  custom_data = results$adverse_effects$qwl_plot_data,
  time_var = "time",
  group_var = "group"
)

plots$adverse_effects$cfw_plot <- create_raw_data_plot(
  outcome_name = "CFW", 
  y_axis_label = "Cognitive Functioning at Work Raw Total Score",
  plot_title = "Changes in Cognitive Functioning at Work (Cohort 1)",
  show_error_bars = FALSE,
  error_bar_type = "se",
  error_bar_width = 0.05,
  custom_data = results$adverse_effects$cfw_plot_data,
  time_var = "time",
  group_var = "group"
)
