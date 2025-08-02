create_model_plot <- function(raw_data, model, outcome_name, y_axis_label, plot_title = NULL, 
                              show_confidence_bands = TRUE, show_jitter = TRUE, show_data_labels = TRUE, 
                              horizontal_line = NULL, show_hline_on_axis = FALSE, cut_at_hline = FALSE, 
                              output_path = paths$plots, display_plot = TRUE,
                              time_var = "time", group_var = "group") {
  
  library(ggeffects)
  library(ggrepel)
  
  # Smart variable detection for time variable
  if (is.numeric(raw_data[[time_var]])) {
    # Look for corresponding .factor version
    time_factor_var <- paste0(time_var, ".factor")
    if (time_factor_var %in% names(raw_data)) {
      cat("Using", time_factor_var, "for time labels\n")
      time_labels_var <- time_factor_var
    } else {
      # Fallback to original numeric variable
      time_labels_var <- time_var
    }
  } else {
    # Already a factor, use as is
    time_labels_var <- time_var
  }
  
  # Smart variable detection for group variable
  if (is.numeric(raw_data[[group_var]])) {
    # Look for corresponding .factor version
    group_factor_var <- paste0(group_var, ".factor")
    if (group_factor_var %in% names(raw_data)) {
      cat("Using", group_factor_var, "for group labels\n")
      group_labels_var <- group_factor_var
    } else {
      # Fallback to original numeric variable
      group_labels_var <- group_var
    }
  } else {
    # Already a factor, use as is
    group_labels_var <- group_var
  }
  
  # Get predicted values from the model - use original variable names for model
  predictions_data <- ggpredict(model, terms = c(time_var, group_var))
  
  # Get unique levels from PREDICTIONS DATA (only timepoints used in model)
  # This ensures we only show timepoints that were actually modeled
  unique_times_from_predictions <- sort(unique(predictions_data$x))
  unique_groups_from_predictions <- sort(unique(predictions_data$group))
  
  # Map back to factor labels if available
  if(is.factor(raw_data[[time_labels_var]])) {
    # Find which factor levels correspond to the predicted timepoints
    time_factor_levels <- levels(raw_data[[time_labels_var]])
    # Map numeric predictions back to factor labels
    if(is.numeric(unique_times_from_predictions)) {
      unique_times <- time_factor_levels[unique_times_from_predictions]
    } else {
      unique_times <- unique_times_from_predictions
    }
  } else {
    unique_times <- unique_times_from_predictions
  }
  
  if(is.factor(raw_data[[group_labels_var]])) {
    # Find which factor levels correspond to the predicted groups
    group_factor_levels <- levels(raw_data[[group_labels_var]])
    # Map numeric predictions back to factor labels
    if(is.numeric(unique_groups_from_predictions)) {
      unique_groups <- group_factor_levels[unique_groups_from_predictions]
    } else {
      unique_groups <- unique_groups_from_predictions
    }
  } else {
    unique_groups <- unique_groups_from_predictions
  }
  
  # Define get_group_colors function
  get_group_colors <- function(group_names) {
    # Use predefined colors where available, generate others
    colors <- character(length(group_names))
    for (i in seq_along(group_names)) {
      if (group_names[i] %in% names(vars$predefined_colors)) {
        colors[i] <- vars$predefined_colors[group_names[i]]
      } else {
        # Fallback to default ggplot colors or custom palette
        colors[i] <- scales::hue_pal()(length(group_names))[i]
      }
    }
    names(colors) <- group_names
    return(colors)
  }
  
  # Call the function with unique_groups to get the colors
  plot_colors <- get_group_colors(unique_groups)
  
  # Handle factor conversion for predictions data
  if (is.numeric(predictions_data$x)) {
    # Map numeric values to actual time labels using the label variable
    if (is.factor(raw_data[[time_labels_var]])) {
      # Use factor levels as labels
      time_mapping <- setNames(levels(raw_data[[time_labels_var]]), 
                               1:length(levels(raw_data[[time_labels_var]])))
      predictions_data$x <- factor(predictions_data$x, 
                                   levels = sort(unique(predictions_data$x)), 
                                   labels = time_mapping[as.character(sort(unique(predictions_data$x)))])
    } else {
      # Use numeric values as labels
      time_mapping <- setNames(unique_times, sort(unique(as.numeric(factor(unique_times)))))
      predictions_data$x <- factor(predictions_data$x, 
                                   levels = sort(unique(predictions_data$x)), 
                                   labels = time_mapping[as.character(sort(unique(predictions_data$x)))])
    }
  }
  
  if (is.numeric(predictions_data$group)) {
    # Map numeric values to actual group labels using the label variable
    if (is.factor(raw_data[[group_labels_var]])) {
      # Use factor levels as labels
      group_mapping <- setNames(levels(raw_data[[group_labels_var]]), 
                                1:length(levels(raw_data[[group_labels_var]])))
      predictions_data$group <- factor(predictions_data$group, 
                                       levels = sort(unique(predictions_data$group)), 
                                       labels = group_mapping[as.character(sort(unique(predictions_data$group)))])
    } else {
      # Use numeric values as labels
      group_mapping <- setNames(unique_groups, sort(unique(as.numeric(factor(unique_groups)))))
      predictions_data$group <- factor(predictions_data$group, 
                                       levels = sort(unique(predictions_data$group)), 
                                       labels = group_mapping[as.character(sort(unique(predictions_data$group)))])
    }
  }
  
  # Add numeric version of time variable
  predictions_data$x.num <- as.numeric(predictions_data$x)
  
  if (is.null(output_path)) {
    output_path <- "."
  }
  
  # Ensure raw_data has consistent factor levels using label variables
  # Filter raw_data to only include timepoints that were actually modeled
  raw_data_filtered <- raw_data %>%
    filter(if(is.numeric(.data[[time_var]])) {
      .data[[time_var]] %in% unique_times_from_predictions
    } else {
      .data[[time_labels_var]] %in% unique_times
    }) %>%
    mutate(
      time_for_plot = .data[[time_labels_var]],
      group_for_plot = .data[[group_labels_var]],
      time_continuous = if(is.numeric(.data[[time_var]])) .data[[time_var]] else as.numeric(.data[[time_var]])
    )
  
  # Base plot elements
  base_plot <- ggplot()
  
  # Conditionally add confidence intervals
  if (show_confidence_bands) {
    base_plot <- base_plot +
      geom_ribbon(data = predictions_data,
                  aes(x = x.num, ymin = conf.low, ymax = conf.high, fill = group),
                  alpha = 0.2)
  }
  
  # Conditionally add horizontal line
  if (!is.null(horizontal_line)) {
    base_plot <- base_plot +
      geom_hline(yintercept = horizontal_line, linetype = 2, colour = "gray")
  }
  
  # Conditionally add jitter points
  if (show_jitter) {
    base_plot <- base_plot +
      geom_jitter(data = raw_data_filtered, 
                  aes(x = time_continuous, y = outcome, color = group_for_plot),
                  alpha = 0.3, width = 0.1)
  }
  
  # Add model predictions
  base_plot <- base_plot +
    geom_line(data = predictions_data, 
              aes(x = x.num, y = predicted, color = group, group = group, linetype = group),
              linewidth = 1.2) +
    geom_point(data = predictions_data,
               aes(x = x.num, y = predicted, color = group, shape = group),
               size = 3)
  
  # Create dynamic shapes and linetypes
  n_groups <- length(unique_groups)
  
  if (n_groups <= 6) {
    shape_values <- c(19, 15, 17, 18, 8, 13)[1:n_groups]
    linetype_values <- c("solid", "22", "dotted", "dashed", "dotdash", "longdash")[1:n_groups]
  } else {
    shape_values <- rep(c(19, 15, 17, 18, 8, 13), length.out = n_groups)
    linetype_values <- rep(c("solid", "22", "dotted", "dashed", "dotdash", "longdash"), length.out = n_groups)
  }
  
  names(shape_values) <- unique_groups
  names(linetype_values) <- unique_groups
  
  # Add data labels if requested
  if (show_data_labels) {
    base_plot <- base_plot +
      geom_text_repel(data = predictions_data,
                      aes(label = sprintf("%.2f", predicted),
                          x = x.num, y = predicted, color = group),
                      size = 4, show.legend = FALSE,
                      direction = "both", min.segment.length = 0)
  }
  
  # Apply scales and styling
  plot <- base_plot +
    scale_x_continuous(breaks = 1:length(unique_times), 
                       labels = unique_times,
                       limits = c(0.8, length(unique_times) + 0.2)) +
    scale_linetype_manual(values = linetype_values) +
    scale_shape_manual(values = shape_values) + 
    scale_color_manual(values = plot_colors) +
    scale_fill_manual(values = plot_colors) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 12),           
      axis.text.y = element_text(size = 11),           
      axis.title = element_text(size = 14),          
      legend.text = element_text(size = 10),         
      legend.title = element_text(size = 11),        
      axis.title.x = element_text(margin = margin(t = 15)),  
      axis.title.y = element_text(margin = margin(r = 15)),  
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt")
    ) +
    labs(color = "Group", fill = "Group", linetype = "Group", shape = "Group", 
         x = "Time", y = y_axis_label)
  
  # Add title if provided
  if (!is.null(plot_title)) {
    plot <- plot + labs(title = plot_title)
  }
  
  if (display_plot) {
    print(plot)
  }
  
  # Save plot
  ggsave(
    filename = file.path(output_path, paste0(outcome_name, "_lmm_plot.jpg")), 
    plot = plot,
    width = 12, height = 8, dpi = 900, units = "in"
  )
  
  cat("LMM model plot created\n")
  
  return(list(
    plot = plot,
    predictions = predictions_data
  ))
}
