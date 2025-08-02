# Calculate completion rates for randomized participants
calculate_completion_rates <- function(data) {
  
  # Filter randomized participants and calculate completion rates
  completion_stats <- data %>%
    filter(randomized == 1) %>%  # Only randomized participants
    filter(ID %in% vars$included_ids) %>%  # Only included IDs
    group_by(group.factor) %>%
    summarise(
      n_randomized = n(),
      n_completers = sum(completer == 1, na.rm = TRUE),
      n_dropouts = sum(completer == 0, na.rm = TRUE),
      completion_rate = round((n_completers / n_randomized) * 100, 1),
      dropout_rate = round((n_dropouts / n_randomized) * 100, 1),
      .groups = "drop"
    )
  
  # Calculate overall stats
  overall_stats <- data %>%
    filter(randomized == 1) %>%
    filter(ID %in% vars$included_ids) %>%
    summarise(
      group.factor = "Overall",
      n_randomized = n(),
      n_completers = sum(completer == 1, na.rm = TRUE),
      n_dropouts = sum(completer == 0, na.rm = TRUE),
      completion_rate = round((n_completers / n_randomized) * 100, 1),
      dropout_rate = round((n_dropouts / n_randomized) * 100, 1)
    )
  
  # Combine group and overall stats
  final_stats <- bind_rows(completion_stats, overall_stats)
  
  return(final_stats)
}

# Print formatted results
print_completion_results <- function(data) {
  
  completion_stats <- calculate_completion_rates(data)
  
  cat("\n☑️️ COMPLETION RATES\n")
  cat("====================\n\n")
  
  # Print table
  cat("Completion Rates by group:\n")
  print(completion_stats)
  
  cat("\n\nSUMMARY SENTENCES:\n")
  cat("------------------\n")
  
  # Overall summary
  overall <- completion_stats[completion_stats$group.factor == "Overall", ]
  cat(sprintf("Overall: %d participants were randomized, of whom %d (%.1f%%) completed the study.\n",
              overall$n_randomized, overall$n_completers, overall$completion_rate))
  
  # Group-specific summaries
  for(i in 1:nrow(completion_stats)) {
    if(completion_stats$group.factor[i] != "Overall") {
      cat(sprintf("%s group: %d randomized, %d completer (%.1f%% completion rate).\n",
                  completion_stats$group.factor[i], 
                  completion_stats$n_randomized[i],
                  completion_stats$n_completers[i],
                  completion_stats$completion_rate[i]))
    }
  }
  
  # Comparative statement
  control_rate <- completion_stats$completion_rate[completion_stats$group.factor == "Control"]
  exp_rate <- completion_stats$completion_rate[completion_stats$group.factor == "Experimental"]
  
  if(length(control_rate) > 0 && length(exp_rate) > 0) {
    cat(sprintf("\nCompletion rates were %.1f%% in the Control group and %.1f%% in the Experimental group.\n",
                control_rate, exp_rate))
  }
}

print_completion_results(wide_data)
