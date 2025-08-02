#===============================================================================
# QWL
#===============================================================================

vars$QWL_outcomes <- list(
  QWLQ_total              = "QWLQ_total",
  QWLQ_sub_belonging      = "QWLQ_sub_belonging",
  QWLQ_sub_good_worker    = "QWLQ_sub_good_worker",
  QWLQ_sub_coworkers      = "QWLQ_sub_coworkers",
  QWLQ_sub_supervisors    = "QWLQ_sub_supervisors",
  QWLQ_sub_tasks          = "QWLQ_sub_tasks",
  QWLQ_sub_conditions     = "QWLQ_sub_conditions",
  QWLQ_sub_environment    = "QWLQ_sub_environment",
  QWLQ_sub_management     = "QWLQ_sub_management",
  QWLQ_domain_A           = "QWLQ_domain_A",
  QWLQ_domain_B           = "QWLQ_domain_B"
)

# Choose which outcome to analyze
  QWL_outcome <- vars$QWL_outcomes$QWLQ_total
  
#===============================================================================
  # Create dataset from long_data (already in long format)
  lmm_data$QWL <- analysis_data %>%
    # Filter to included relevant timepoints
    filter(
      time %in% c(0, 1)  # Only Pre and Post for main analysis
    ) %>%
    # Select relevant variables
    select(
      ID, group, group.factor, time, time.factor, cluster, cluster.factor, completer, 
      sd_age, sd_sex, ASRS_total,
      !!sym(QWL_outcome)
    ) %>%
    # Convert ID to a factor for modeling
    mutate(ID = as.factor(ID)) %>%
    # Rename outcome variable for consistency with your existing code
    rename(outcome = !!sym(QWL_outcome))

# Fit mixed-effects model
  lmm_models$QWL.lm1 <- lmer(outcome ~ time.factor * group.factor + (1 | ID), data = lmm_data$QWL)

# Model summary
  print_model_results(lmm_models$QWL.lm1, "QWL MIXED MODEL", include_emm = FALSE) # set to TRUE to include estimated marginal means (post-hoc comparisons)

# ==============================================================================
# Create plots (raw + model interaction)
  qwl_raw_plot <- create_raw_data_plot(
    outcome_name = "QWL",
    custom_data = lmm_data$QWL, 
    y_axis_label = "Quality of Work Life Outcome",
    horizontal_line = 200,
    cut_at_hline = TRUE,
    show_hline_on_axis = TRUE,
    show_confidence_bands = FALSE,
    show_jitter = TRUE,
    show_data_labels = TRUE,
    time_var = "time.factor",      # Must match the variable name used in your model
    group_var = "group.factor"     # Must match the variable name used in your model
  )

  qwl_model_plots <- create_model_plot(
    raw_data = lmm_data$QWL,
    model = lmm_models$QWL.lm1,
    outcome_name = "QWL",
    y_axis_label = "Quality of Work Life Outcome",
    show_confidence_bands = TRUE,
    show_jitter = FALSE,
    show_data_labels = FALSE,
    time_var = "time.factor", 
    group_var = "group.factor"
  )
  
#===============================================================================
# Create table
  qwl_table <- create_lmm_table(
    model = lmm_models$QWL.lm1,
    outcome_name = "QWL",
    save_file = FALSE
  )
