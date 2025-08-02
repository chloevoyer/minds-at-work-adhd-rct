#===============================================================================
# group differences: ASRS scores
#===============================================================================

results <- list()

# Independent samples t-test
# To check normality assumptions
results$baseline_diffs$asrs$shapiro_exp <- shapiro.test(analysis_data$ASRS_mean[analysis_data$group.factor == "Experimental"])
results$baseline_diffs$asrs$shapiro_ctrl <- shapiro.test(analysis_data$ASRS_mean[analysis_data$group.factor == "Control"])
print(results$baseline_diffs$asrs$shapiro_exp)
print(results$baseline_diffs$asrs$shapiro_ctrl)

results$baseline_diffs$asrs$baseline_test <- t.test(ASRS_mean ~ group, data = analysis_data)
print(results$baseline_diffs$asrs$baseline_test)

# Or for more detailed statistics
results$baseline_diffs$asrs$group.factor_stats <- analysis_data %>%
  group_by(group) %>%
  summarise(
    n = n(),
    mean = mean(ASRS_total, na.rm = TRUE),
    sd = sd(ASRS_total, na.rm = TRUE),
    se = sd/sqrt(n)
  )

print(results$baseline_diffs$asrs$group.factor_stats)

mean(analysis_data$ASRS_total, na.rm = TRUE)
sd(analysis_data$ASRS_total, na.rm = TRUE)

# For non-parametric alternative (if data not normally distributed)
results$baseline_diffs$wilcox_test <- wilcox.test(ASRS_mean ~ group, data = analysis_data)
print(results$baseline_diffs$wilcox_test)

#===============================================================================
# group differences: QWL total pre
#===============================================================================

# To check normality assumptions
results$baseline_diffs$qwl$shapiro_exp <- shapiro.test(analysis_data$QWLQ_total[analysis_data$group.factor == "Experimental"])
results$baseline_diffs$qwl$shapiro_ctrl <- shapiro.test(analysis_data$QWLQ_total[analysis_data$group.factor == "Control"])
print(results$baseline_diffs$qwl$shapiro_exp)
print(results$baseline_diffs$qwl$shapiro_ctrl)

# Independent samples t-test
results$baseline_diffs$qwl$baseline_test <- t.test(QWLQ_total ~ group, data = analysis_data)
print(results$baseline_diffs$qwl$baseline_test)

# For non-parametric alternative (if data not normally distributed)
results$baseline_diffs$qwl$wilcox_test <- wilcox.test(QWLQ_total ~ group, data = analysis_data)
print(results$baseline_diffs$qwl$wilcox_test)

#===============================================================================
# group differences: CFW total pre
#===============================================================================

# To check normality assumptions
results$baseline_diffs$cfw$shapiro_exp <- shapiro.test(analysis_data$CFWQ_total[analysis_data$group.factor == "Experimental"])
results$baseline_diffs$cfw$shapiro_ctrl <- shapiro.test(analysis_data$CFWQ_total[analysis_data$group.factor == "Control"])
print(results$baseline_diffs$cfw$shapiro_exp)
print(results$baseline_diffs$cfw$shapiro_ctrl)

# Independent samples t-test
results$baseline_diffs$cfw$baseline_test <- t.test(CFWQ_total ~ group, data = analysis_data)
print(results$baseline_diffs$cfw$baseline_test)

# For non-parametric alternative (if data not normally distributed)
results$baseline_diffs$cfw$wilcox_test <- wilcox.test(CFWQ_total ~ group, data = analysis_data)
print(results$baseline_diffs$cfw$wilcox_test)

