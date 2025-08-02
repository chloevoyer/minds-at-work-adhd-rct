#===============================================================================
# CFW
#===============================================================================

vars$CFW_outcomes <- list(
  CFWQ_total              = "CFWQ_total",
  CFWQ_domain_memory      = "CFWQ_domain_memory",
  CFWQ_domain_attention   = "CFWQ_domain_attention",
  CFWQ_domain_performance = "CFWQ_domain_performance",
  CFWQ_sub_memory         = "CFWQ_sub_memory",
  CFWQ_sub_language       = "CFWQ_sub_language",
  CFWQ_sub_exec_function  = "CFWQ_sub_exec_function",
  CFWQ_sub_speed          = "CFWQ_sub_speed",
  CFWQ_sub_control        = "CFWQ_sub_control",
  CFWQ_sub_name_memory    = "CFWQ_sub_name_memory",
  CFWQ_22v_total_rev      = "CFWQ_22v_total_rev"
)

# Choose which outcome to analyze
  CFW_outcome = vars$CFW_outcomes$CFWQ_total

#===============================================================================
# Create dataset from long_data (already in long format)
  lmm_data$CFW <- analysis_data %>%
    # Filter to included relevant timepoints
    filter(
      time %in% c(0, 1)  # Only Pre and Post for main analysis
    ) %>%
    # Select relevant variables
    select(
      ID, group, group.factor, time, time.factor, cluster, cluster.factor, completer, 
      sd_age, sd_sex, ASRS_total,
      !!sym(CFW_outcome)
    ) %>%
    # Convert ID to a factor for modeling
    mutate(ID = as.factor(ID)) %>%
    # Rename outcome variable for consistency with your existing code
    rename(outcome = !!sym(CFW_outcome))

# Fit mixed-effects model
  lmm_models$CFW.lm1 <- lmer(outcome ~ time.factor * group.factor + (1 | ID), data = lmm_data$CFW)

  # Model summary
  print_model_results(lmm_models$CFW.lm1, "CFW MIXED MODEL", include_emm = FALSE) # set to TRUE to include estimated marginal means (post-hoc comparisons)
  
# Compute mean values for annotation
  CFW_summary_data <- lmm_data$CFW %>%
    group_by(group, time) %>%
    summarise(mean_CFW = mean(outcome, na.rm = TRUE), .groups = 'drop') %>%
    mutate(hjust = ifelse(time == "Pre", 1.5, -0.5))  # Adjust horizontal position based on Time

# Plot interaction
  cfw_raw_plot <- create_raw_data_plot(
    outcome_name = "CFW",
    y_axis_label = "Cognitive Functioning at Work Outcome",
    plot_title = "Cognitive Functioning at Work Over Time",
    horizontal_line = 58,
    cut_at_hline = TRUE,
    show_hline_on_axis = TRUE,
    show_confidence_bands = FALSE,
    show_jitter = TRUE,
    show_data_labels = TRUE,
    time_var = "time.factor",
    group_var = "group.factor"
  )
  
  cfw_model_plot <- create_model_plot(
    raw_data = lmm_data$CFW,
    model = lmm_models$CFW.lm1,
    outcome_name = "CFW",
    y_axis_label = "Cognitive Functioning at Work Outcome",
    show_confidence_bands = TRUE,
    show_jitter = FALSE,
    show_data_labels = FALSE,
    time_var = "time.factor",     
    group_var = "group.factor"
  )

# Create table
  cfw_table <- create_lmm_table(
    model = lmm_models$CFW.lm1,
    outcome_name = "CFW",
    save_file = FALSE
  )

#===============================================================================
# Get the unique IDs from your LMM datasets
  vars$included_ids <- unique(unlist(lapply(lmm_data, function(x) x$ID)))
