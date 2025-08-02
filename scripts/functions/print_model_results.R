print_model_results <- function(model, model_name = "MODEL", include_emm = FALSE) {
  # Custom styles
  main_header <- function(x) crayon::bold(crayon::cyan(x))
  sub_header <- function(x) crayon::bold(crayon::yellow(x))
  
  # Main header
  cat(main_header(paste0("\n", strrep("=", 50), "\n")))
  cat(main_header(paste0("   ", model_name, " RESULTS")))
  cat(main_header(paste0("\n", strrep("=", 50), "\n")))
  
  # Diagnostics
  cat(sub_header("\n✅ MODEL DIAGNOSTICS\n"))
  cat(strrep("-", 30), "\n")
  check_model_diagnostics(model)
  
  # Summary
  cat(sub_header("\n📊 MODEL SUMMARY\n"))
  cat(strrep("-", 30), "\n")
  print(summary(model))
  
  # ANOVA
  cat(sub_header("\n🔬 ANOVA RESULTS\n"))
  cat(strrep("-", 30), "\n")
  print(anova(model))
  
  # Model performance
  # library(performance)
  cat(sub_header("\n📈 MODEL PERFORMANCE\n"))
  cat(strrep("-", 30), "\n")
  print(model_performance(model))
  
  # Estimated marginal means (conditional)
  if(include_emm) {
    cat(sub_header("\n📈 ESTIMATED MARGINAL MEANS\n"))
    cat(strrep("-", 30), "\n")
    
    tryCatch({
      emm <- emmeans(model, ~ group * time)
      cat("Estimated Marginal Means:\n")
      print(emm)
      
      cat("\nPairwise Contrasts:\n")
      emm_contrasts <- contrast(emm, "tukey")
      print(emm_contrasts)
      
      cat("\nPairs Summary:\n")
      print(pairs(emm_contrasts))
      
    }, error = function(e) {
      cat("Error calculating EMMs:", e$message, "\n")
    })
  }
  
  cat(main_header(paste0("\n")))
}