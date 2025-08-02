# Function to check normality for a specific variable by group------------------
check_normality <- function(data, var_name) {
  cat("\n===== Checking normality for", var_name, "=====\n")
  
  # Split data by group
  exp_data <- data %>% filter(group == 1) %>% pull(!!sym(var_name))
  control_data <- data %>% filter(group == 0) %>% pull(!!sym(var_name))
  
  # Shapiro-Wilk test
  cat("\nShapiro-Wilk Test (p > 0.05 suggests normality):\n")
  cat("Experimental group:", shapiro.test(exp_data)$p.value, "\n")
  cat("Control group:", shapiro.test(control_data)$p.value, "\n")
  
  # Descriptive statistics
  cat("\nDescriptive Statistics:\n")
  exp_stats <- data %>% 
    filter(group == 1) %>% 
    dplyr::summarize(
      n = sum(!is.na(!!sym(var_name))),
      mean = mean(!!sym(var_name), na.rm = TRUE),
      median = median(!!sym(var_name), na.rm = TRUE),
      sd = sd(!!sym(var_name), na.rm = TRUE),
      min = min(!!sym(var_name), na.rm = TRUE),
      max = max(!!sym(var_name), na.rm = TRUE),
      skew = psych::skew(!!sym(var_name)),
      kurtosis = psych::kurtosi(!!sym(var_name))
    )
  
  control_stats <- data %>% 
    filter(group == 0) %>% 
    dplyr::summarize(
      n = sum(!is.na(!!sym(var_name))),
      mean = mean(!!sym(var_name), na.rm = TRUE),
      median = median(!!sym(var_name), na.rm = TRUE),
      sd = sd(!!sym(var_name), na.rm = TRUE),
      min = min(!!sym(var_name), na.rm = TRUE),
      max = max(!!sym(var_name), na.rm = TRUE),
      skew = psych::skew(!!sym(var_name)),
      kurtosis = psych::kurtosi(!!sym(var_name))
    )
  
  cat("Experimental group:\n")
  print(exp_stats)
  cat("Control group:\n")
  print(control_stats)
  
  # Create plots (histograms and Q-Q plots)
  par(mfrow = c(2, 2))
  
  # Histograms
  hist(exp_data, main = paste("Experimental -", var_name), xlab = var_name)
  hist(control_data, main = paste("Control -", var_name), xlab = var_name)
  
  # Q-Q plots
  qqnorm(exp_data, main = paste("Q-Q Plot: Experimental -", var_name))
  qqline(exp_data)
  qqnorm(control_data, main = paste("Q-Q Plot: Control -", var_name))
  qqline(control_data)
  
  # Reset plot parameters
  par(mfrow = c(1, 1))
  
  # Check for outliers using boxplots - FIXED VERSION
  formula_str <- paste(var_name, "~ group")
  boxplot(as.formula(formula_str), data = data,
          main = paste("Boxplot of", var_name, "by group"),
          ylab = var_name)
  
  # If assumptions are violated, suggest non-parametric alternative
  sw_exp_p <- shapiro.test(exp_data)$p.value
  sw_control_p <- shapiro.test(control_data)$p.value
  
  if (sw_exp_p < 0.05 || sw_control_p < 0.05 || 
      abs(exp_stats$skew) > 1 || abs(control_stats$skew) > 1) {
    cat("\nNOTE: Normality assumption may be violated. Consider using non-parametric tests:\n")
    
    # Within-group tests (Wilcoxon signed-rank test)
    exp_wilcox <- wilcox.test(exp_data, mu = 0, alternative = "two.sided")
    control_wilcox <- wilcox.test(control_data, mu = 0, alternative = "two.sided")
    
    # Between-group test (Mann-Whitney U test)
    group_wilcox <- wilcox.test(
      data %>% filter(group == 1) %>% pull(!!sym(var_name)),
      data %>% filter(group == 0) %>% pull(!!sym(var_name))
    )
    
    cat("Wilcoxon signed-rank test (Experimental):", exp_wilcox$p.value, "\n")
    cat("Wilcoxon signed-rank test (Control):", control_wilcox$p.value, "\n")
    cat("Mann-Whitney U test (between groups):", group_wilcox$p.value, "\n")
  } else {
    cat("\nNormality assumption appears to be met for this variable.\n")
  }
}

# Function to provide recommendation based on normality and variance tests------
provide_recommendation <- function(data, var_name) {
  # Split data by group
  exp_data <- data %>% filter(group == 1) %>% pull(!!sym(var_name))
  control_data <- data %>% filter(group == 0) %>% pull(!!sym(var_name))
  
  # Check normality
  sw_exp_p <- shapiro.test(exp_data)$p.value
  sw_control_p <- shapiro.test(control_data)$p.value
  
  # Check variances
  lev_test <- car::leveneTest(reformulate("group", var_name), data = data)
  
  # Make recommendation
  if (sw_exp_p < 0.05 || sw_control_p < 0.05) {
    cat(var_name, "- Normality violated: Use Wilcoxon signed-rank test for within-group and Mann-Whitney U test for between-group comparisons.\n")
  } else if (lev_test$`Pr(>F)`[1] < 0.05) {
    cat(var_name, "- Normality met but variances unequal: Use t-tests with Welch correction.\n")
  } else {
    cat(var_name, "- All assumptions met: Standard t-tests appropriate.\n")
  }
}

# Step 4: Perform both parametric and non-parametric tests for outcome changes--
analyze_outcome_both_tests <- function(data, pre_var, post_var, var_name) {
  cat("\n===== Analysis for", var_name, "=====\n")
  
  # Calculate change scores
  data$change <- data[[post_var]] - data[[pre_var]]
  
  # Calculate mean and SD by group
  group_stats <- data %>%
    group_by(group) %>%
    dplyr::summarize(
      n = sum(!is.na(change)),
      pre_mean = mean(!!sym(pre_var), na.rm = TRUE),
      post_mean = mean(!!sym(post_var), na.rm = TRUE),
      change_mean = mean(change, na.rm = TRUE),
      change_sd = sd(change, na.rm = TRUE)
    )
  
  cat("\nDescriptive Statistics:\n")
  print(group_stats)
  
  # PARAMETRIC TESTS
  cat("\n-- Parametric Tests --\n")
  
  # Within-group t-tests
  exp_ttest <- t.test(data$change[data$group == 1], mu = 0)
  control_ttest <- t.test(data$change[data$group == 0], mu = 0)
  
  # Between-group t-tests (both with and without Welch correction)
  group_ttest <- t.test(
    data$change[data$group == 1],
    data$change[data$group == 0],
    var.equal = FALSE  # Welch correction
  )
  
  group_ttest_eq <- t.test(
    data$change[data$group == 1],
    data$change[data$group == 0],
    var.equal = TRUE  # Equal variance assumption
  )
  
  cat("\nExperimental group t-test (vs. no change):\n")
  cat("t =", round(exp_ttest$statistic, 2), 
      ", df =", round(exp_ttest$parameter, 2),
      ", p =", round(exp_ttest$p.value, 3), "\n")
  
  cat("\nControl group t-test (vs. no change):\n")
  cat("t =", round(control_ttest$statistic, 2), 
      ", df =", round(control_ttest$parameter, 2),
      ", p =", round(control_ttest$p.value, 3), "\n")
  
  cat("\nBetween-group t-test with Welch correction:\n")
  cat("t =", round(group_ttest$statistic, 2), 
      ", df =", round(group_ttest$parameter, 2),
      ", p =", round(group_ttest$p.value, 3), "\n")
  
  cat("\nBetween-group t-test with equal variances:\n")
  cat("t =", round(group_ttest_eq$statistic, 2), 
      ", df =", round(group_ttest_eq$parameter, 2),
      ", p =", round(group_ttest_eq$p.value, 3), "\n")
  
  # NON-PARAMETRIC TESTS
  cat("\n-- Non-parametric Tests --\n")
  
  # Wilcoxon signed-rank test (within group)
  tryCatch({
    exp_wilcox <- wilcox.test(data$change[data$group == 1], mu = 0, exact = FALSE)
    control_wilcox <- wilcox.test(data$change[data$group == 0], mu = 0, exact = FALSE)
    
    # Mann-Whitney U test (between groups)
    group_wilcox <- wilcox.test(
      data$change[data$group == 1],
      data$change[data$group == 0],
      exact = FALSE
    )
    
    cat("\nExperimental group Wilcoxon signed-rank test (vs. no change):\n")
    cat("V =", exp_wilcox$statistic, ", p =", round(exp_wilcox$p.value, 3), "\n")
    
    cat("\nControl group Wilcoxon signed-rank test (vs. no change):\n")
    cat("V =", control_wilcox$statistic, ", p =", round(control_wilcox$p.value, 3), "\n")
    
    cat("\nBetween-group Mann-Whitney U test:\n")
    cat("W =", group_wilcox$statistic, ", p =", round(group_wilcox$p.value, 3), "\n")
  }, error = function(e) {
    cat("Error in non-parametric tests:", e$message, "\n")
  })
  
  # Return results
  return(list(
    outcome = var_name,
    group_stats = group_stats,
    parametric = list(
      exp_ttest = exp_ttest,
      control_ttest = control_ttest,
      group_ttest = group_ttest,
      group_ttest_eq = group_ttest_eq
    ),
    nonparametric = if(exists("exp_wilcox")) list(
      exp_wilcox = exp_wilcox,
      control_wilcox = control_wilcox,
      group_wilcox = group_wilcox
    ) else NULL
  ))
}

# Step 5: Check for adverse effects---------------------------------------------
check_adverse_effects <- function(results) {
  cat("\nAdverse Effects Check for", results$outcome, ":\n")
  
  # Initialize variables
  param_adverse <- FALSE
  param_worse <- FALSE
  nonparam_adverse <- FALSE
  nonparam_worse <- FALSE
  
  # Check parametric tests
  if(!is.null(results$parametric)) {
    # Check if experimental group shows significant decrease
    if(results$parametric$exp_ttest$estimate[1] < 0 && results$parametric$exp_ttest$p.value < 0.05) {
      param_adverse <- TRUE
      cat("- PARAMETRIC TEST WARNING: Significant decrease in experimental group\n")
    }
    
    # Check if experimental group is significantly worse than control
    mean_diff <- mean(results$group_stats$change_mean[results$group_stats$group == 1]) - 
      mean(results$group_stats$change_mean[results$group_stats$group == 0])
    
    if(mean_diff < 0 && results$parametric$group_ttest$p.value < 0.05) {
      param_worse <- TRUE
      cat("- PARAMETRIC TEST WARNING: Experimental group significantly worse than control\n")
    }
  }
  
  # Check non-parametric tests
  if(!is.null(results$nonparametric)) {
    exp_median <- median(results$group_stats$change_mean[results$group_stats$group == 1])
    control_median <- median(results$group_stats$change_mean[results$group_stats$group == 0])
    
    # Check if experimental group shows significant decrease
    if(exp_median < 0 && results$nonparametric$exp_wilcox$p.value < 0.05) {
      nonparam_adverse <- TRUE
      cat("- NON-PARAMETRIC TEST WARNING: Significant decrease in experimental group\n")
    }
    
    # Check if experimental group is significantly worse than control
    if(exp_median < control_median && results$nonparametric$group_wilcox$p.value < 0.05) {
      nonparam_worse <- TRUE
      cat("- NON-PARAMETRIC TEST WARNING: Experimental group significantly worse than control\n")
    }
  }
  
  # Overall assessment
  adverse_detected <- param_adverse || param_worse || nonparam_adverse || nonparam_worse
  
  if(adverse_detected) {
    cat("\nPOTENTIAL ADVERSE EFFECT DETECTED for", results$outcome, "\n")
  } else {
    cat("\nNo adverse effects detected for", results$outcome, "\n")
    
    # Check for improvement
    if(results$parametric$exp_ttest$estimate[1] > 0 && results$parametric$exp_ttest$p.value < 0.05) {
      cat("NOTE: Statistically significant IMPROVEMENT detected in experimental group\n")
      cat("      (t =", round(results$parametric$exp_ttest$statistic, 2), 
          ", p =", round(results$parametric$exp_ttest$p.value, 3), ")\n")
      
      if(!is.null(results$nonparametric) && results$nonparametric$exp_wilcox$p.value < 0.05) {
        cat("      Confirmed by non-parametric test (p =", 
            round(results$nonparametric$exp_wilcox$p.value, 3), ")\n")
      }
    }
  }
  
  return(adverse_detected)
}