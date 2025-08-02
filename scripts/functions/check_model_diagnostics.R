check_model_diagnostics <- function(model, data_var_name = NULL) {
  # Create a properly formatted data frame for plotting
  model_frame <- model.frame(model)
  plot_data <- data.frame(
    fitted = fitted(model),
    resid = residuals(model)
  )
  
  # Add grouping variables if available in the model frame
  if("group" %in% names(model_frame)) {
    plot_data$group <- model_frame$group
  }
  
  # Get the response variable name
  response_var <- names(model.frame(model))[1]
  
  # Create a multi-panel plot layout
  old_par <- par(no.readonly = TRUE)
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
  
  # 1. Linearity check: Residuals vs. fitted
  plot(fitted(model), resid(model), 
       main = "1. Residuals vs. Fitted Values",
       xlab = "Fitted values", ylab = "Residuals")
  abline(h = 0, lty = 2, col = "red")
  lines(lowess(fitted(model), resid(model)), col = "blue")
  
  # 2. Normality of residuals: QQ plot
  qqnorm(resid(model), main = "2. Normal Q-Q Plot of Residuals")
  qqline(resid(model), col = "red")
  
  # 3. Homoscedasticity: Scale-location plot
  plot(fitted(model), sqrt(abs(resid(model))), 
       main = "3. Scale-Location Plot",
       xlab = "Fitted values", ylab = "√|Standardized residuals|")
  lines(lowess(fitted(model), sqrt(abs(resid(model)))), col = "red")
  
  # 4. Histogram of residuals
  hist(resid(model), main = "4. Histogram of Residuals", 
       xlab = "Residuals", breaks = 15, col = "lightblue", border = "white")
  
  # Reset par settings
  par(old_par)
  
  # Additional diagnostics with proper error handling
  # cat(colours$red("\n===================== ", crayon::bold("MODEL DIAGNOSTICS"), "=====================\n"))
  
  # Shapiro-Wilk test for normality
  sw_test <- try(shapiro.test(resid(model)), silent = TRUE)
  if (!inherits(sw_test, "try-error")) {
    cat("Shapiro-Wilk normality test: W =", round(sw_test$statistic, 4), 
        ", p-value =", round(sw_test$p.value, 4), "\n")
    if (sw_test$p.value < 0.05) {
      cat("  * Residuals appear to deviate from normality (p < 0.05)\n")
    } else {
      cat("  * No significant deviation from normality (p ≥ 0.05)\n")
    }
  }
  
  # group-specific plots (if group is in the model)
  if ("group" %in% names(model_frame)) {
    cat("\nPlotting group-specific diagnostics...\n")
    print(lattice::xyplot(resid ~ fitted | group, data = plot_data,
                          main = "Residuals vs. Fitted Values by group",
                          xlab = "Fitted Values", ylab = "Residuals",
                          panel = function(x, y, ...) {
                            lattice::panel.xyplot(x, y, ...)
                            lattice::panel.abline(h = 0, lty = 2)
                          }))
  }
  
  # Autocorrelation plot
  cat("\nAutocorrelation of residuals (check for serial correlation):\n")
  acf_result <- acf(resid(model), plot = FALSE)
  plot(acf_result, main = "Autocorrelation of Residuals")
  
  # Random effects summary
  cat("\nRandom effects summary:\n")
  random_effects <- ranef(model)
  cat("- Number of levels:", nrow(random_effects$ID), "\n")
  cat("- Range of random intercepts:", 
      round(range(random_effects$ID)[1], 2), "to", 
      round(range(random_effects$ID)[2], 2), "\n")
  
  # Plot random effects normality
  par(mfrow = c(1, 1))
  qqnorm(random_effects$ID[,1], main = "Normal Q-Q Plot of Random Effects")
  qqline(random_effects$ID[,1], col = "red")
  
  invisible(model) # Return the model invisibly
}