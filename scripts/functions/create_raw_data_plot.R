create_raw_data_plot <- function(outcome_name, y_axis_label, plot_title = NULL, 
                                 show_confidence_bands = TRUE, show_jitter = TRUE, show_data_labels = TRUE, 
                                 show_error_bars = FALSE, error_bar_type = "se", error_bar_width = 0.1, 
                                 horizontal_line = NULL, show_hline_on_axis = FALSE, cut_at_hline = FALSE, 
                                 output_path = paths$plots, display_plots = TRUE, custom_data = NULL,
                                 time_var = "time", group_var = "group") {
  
  library(ggrepel)
  
  if (is.null(output_path)) {
    output_path <- "."
  }
  
  # Define variable names based on outcome
  if (!is.null(custom_data) && "outcome" %in% names(custom_data)) {
    outcome_var <- "outcome"
  } else if (outcome_name == "QWL") {
    outcome_var <- "QWLQ_total"
  } else if (outcome_name == "CFW") {
    outcome_var <- "CFWQ_total"
  } else {
    stop("outcome_name must be either 'QWL' or 'CFW'")
  }
  
  # Use custom data if provided, otherwise use analysis_data
  if (!is.null(custom_data)) {
    # Look for variable containing "outcome" first, then fallback to outcome_var
    outcome_col <- names(custom_data)[str_detect(names(custom_data), "outcome")]
    if (length(outcome_col) > 0) {
      outcome_col <- outcome_col[1]
    } else {
      outcome_col <- outcome_var
    }
    
    raw_data <- custom_data %>%
      select(ID, all_of(group_var), all_of(time_var), all_of(outcome_col)) %>%
      rename(outcome = !!outcome_col, group = !!group_var, time = !!time_var) %>%
      filter(!is.na(outcome))
    
  } else {
    raw_data <- analysis_data %>%
      select(ID, all_of(group_var), all_of(time_var), all_of(outcome_var)) %>%
      rename(outcome = if("outcome" %in% names(.)) "outcome" else !!outcome_var, 
             group = !!group_var, time = !!time_var) %>%
      filter(!is.na(outcome))
  }
  
  # Smart variable detection for time variable
  if (is.numeric(raw_data$time)) {
    # Look for corresponding .factor version in the original data
    time_factor_var <- paste0(time_var, ".factor")
    if (!is.null(custom_data) && time_factor_var %in% names(custom_data)) {
      cat("Using", time_factor_var, "for time labels\n")
      # Extract the factor version and add to raw_data
      factor_data <- custom_data %>%
        select(ID, !!time_factor_var) %>%
        rename(time_factor = !!time_factor_var)
      raw_data <- raw_data %>% left_join(factor_data, by = "ID")
      time_labels_source <- "time_factor"
    } else if (exists("analysis_data") && time_factor_var %in% names(analysis_data)) {
      cat("Using", time_factor_var, "for time labels from analysis_data\n")
      factor_data <- analysis_data %>%
        select(ID, !!time_factor_var) %>%
        rename(time_factor = !!time_factor_var) %>%
        distinct()
      raw_data <- raw_data %>% left_join(factor_data, by = "ID")
      time_labels_source <- "time_factor"
    } else {
      # Fallback to original numeric variable
      time_labels_source <- "time"
    }
  } else {
    # Already a factor, use as is
    time_labels_source <- "time"
  }
  
  # Smart variable detection for group variable
  if (is.numeric(raw_data$group)) {
    # Look for corresponding .factor version in the original data
    group_factor_var <- paste0(group_var, ".factor")
    if (!is.null(custom_data) && group_factor_var %in% names(custom_data)) {
      cat("Using", group_factor_var, "for group labels\n")
      # Extract the factor version and add to raw_data
      factor_data <- custom_data %>%
        select(ID, !!group_factor_var) %>%
        rename(group_factor = !!group_factor_var)
      raw_data <- raw_data %>% left_join(factor_data, by = "ID")
      group_labels_source <- "group_factor"
    } else if (exists("analysis_data") && group_factor_var %in% names(analysis_data)) {
      cat("Using", group_factor_var, "for group labels from analysis_data\n")
      factor_data <- analysis_data %>%
        select(ID, !!group_factor_var) %>%
        rename(group_factor = !!group_factor_var) %>%
        distinct()
      raw_data <- raw_data %>% left_join(factor_data, by = "ID")
      group_labels_source <- "group_factor"
    } else {
      # Fallback to original numeric variable
      group_labels_source <- "group"
    }
  } else {
    # Already a factor, use as is
    group_labels_source <- "group"
  }
  
  # Get unique levels and create factor levels dynamically
  unique_times <- if(time_labels_source == "time_factor" && "time_factor" %in% names(raw_data)) {
    if(is.factor(raw_data$time_factor)) levels(raw_data$time_factor) else sort(unique(raw_data$time_factor))
  } else {
    sort(unique(raw_data$time))
  }
  
  unique_groups <- if(group_labels_source == "group_factor" && "group_factor" %in% names(raw_data)) {
    if(is.factor(raw_data$group_factor)) levels(raw_data$group_factor) else sort(unique(raw_data$group_factor))
  } else {
    sort(unique(raw_data$group))
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
  
  # Convert to factors if they aren't already, using appropriate label source
  raw_data <- raw_data %>%
    mutate(
      time_for_plot = if(time_labels_source == "time_factor" && "time_factor" %in% names(.)) {
        factor(time_factor, levels = unique_times)
      } else {
        factor(time, levels = unique_times)
      },
      group_for_plot = if(group_labels_source == "group_factor" && "group_factor" %in% names(.)) {
        factor(group_factor, levels = unique_groups)
      } else {
        factor(group, levels = unique_groups)
      },
      time_numeric = as.numeric(time_for_plot)
    )
  
  # Calculate group means and error measures from raw data
  summary_data <- raw_data %>%
    group_by(time_for_plot, group_for_plot) %>%
    summarise(
      predicted = mean(outcome, na.rm = TRUE),
      se = sd(outcome, na.rm = TRUE) / sqrt(sum(!is.na(outcome))),
      sd = sd(outcome, na.rm = TRUE),
      n = sum(!is.na(outcome)),
      .groups = "drop"
    ) %>%
    rename(time = time_for_plot, group = group_for_plot) %>%
    mutate(
      conf.low = predicted - (1.96 * se),   
      conf.high = predicted + (1.96 * se),
      error_low = case_when(
        error_bar_type == "se" ~ predicted - se,
        error_bar_type == "sd" ~ predicted - sd,
        error_bar_type == "ci" ~ conf.low,
        TRUE ~ predicted - se
      ),
      error_high = case_when(
        error_bar_type == "se" ~ predicted + se,
        error_bar_type == "sd" ~ predicted + sd,
        error_bar_type == "ci" ~ conf.high,
        TRUE ~ predicted + se
      ),
      x = time,
      x.num = as.numeric(time)
    )
  
  # Create the plot
  plot <- ggplot()
  
  # Conditionally add confidence intervals
  if (show_confidence_bands) {
    plot <- plot +
      geom_ribbon(data = summary_data,
                  aes(x = x.num, ymin = conf.low, ymax = conf.high, fill = group),
                  alpha = 0.2)
  }
  
  # Conditionally add horizontal line
  if (!is.null(horizontal_line)) {
    plot <- plot +
      geom_hline(yintercept = horizontal_line, linetype = 2, colour = "gray")
  }
  
  # Conditionally add jitter points
  if (show_jitter) {
    plot <- plot +
      geom_jitter(data = raw_data, 
                  aes(x = time_numeric, y = outcome, color = group_for_plot),
                  alpha = 0.3, width = 0.1)
  }
  
  # Add error bars if requested
  if (show_error_bars) {
    plot <- plot +
      geom_errorbar(data = summary_data,
                    aes(x = x.num, ymin = error_low, ymax = error_high, color = group),
                    width = error_bar_width, position = position_dodge(width = 0.2))
  }
  
  # Add line and points for means
  plot <- plot +
    geom_line(data = summary_data, 
              aes(x = x.num, y = predicted, color = group, group = group, linetype = group),
              linewidth = 1.2) +
    geom_point(data = summary_data,
               aes(x = x.num, y = predicted, color = group, shape = group),
               size = 3)
  
  # Conditionally add data labels
  if (show_data_labels) {
    plot <- plot +
      geom_text_repel(data = summary_data,
                      aes(label = sprintf("%.2f", predicted),
                          x = x.num, y = predicted, color = group),
                      size = 4, show.legend = FALSE, 
                      direction = "both", min.segment.length = 0)
  }
  
  # Create dynamic shapes, linetypes, and colors based on number of groups
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
  
  # Apply scales and theme
  plot <- plot +
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
    )
  
  # Handle y-axis breaks and limits (rest of the function remains the same)
  if (!is.null(horizontal_line)) {
    y_limits <- range(c(summary_data$predicted, summary_data$conf.low, summary_data$conf.high), na.rm = TRUE)
    
    if (cut_at_hline) {
      y_limits[2] <- horizontal_line
    }
    
    if (show_hline_on_axis || cut_at_hline) {
      if (cut_at_hline) {
        custom_breaks <- pretty(c(y_limits[1], horizontal_line), n = 5)
        custom_breaks <- custom_breaks[custom_breaks <= horizontal_line]
        if (!horizontal_line %in% custom_breaks) {
          custom_breaks <- c(custom_breaks, horizontal_line)
        }
      } else {
        temp_plot <- plot
        built_plot <- ggplot_build(temp_plot)
        default_breaks <- built_plot$layout$panel_params[[1]]$y$breaks
        custom_breaks <- sort(unique(c(default_breaks, horizontal_line)))
      }
      
      plot <- plot +
        scale_y_continuous(breaks = custom_breaks, limits = y_limits)
    } else if (cut_at_hline) {
      plot <- plot +
        scale_y_continuous(limits = y_limits)
    }
  }
  
  # Add labels
  if (is.null(plot_title)) {
    plot <- plot +
      labs(color = "Group", fill = "Group", linetype = "Group", shape = "Group", 
           x = "Time", y = y_axis_label)
  } else {
    plot <- plot +
      labs(title = plot_title,
           color = "Group", fill = "Group", linetype = "Group", shape = "Group", 
           x = "Time", y = y_axis_label)
  }
  
  if (display_plots) {
    cat("Displaying Raw Data Plot...\n")
    print(plot)
  }
  
  # Save plot
  ggsave(
    filename = file.path(output_path, paste0(outcome_name, "_raw_data_plot.jpg")), 
    plot = plot,
    width = 12, height = 8, dpi = 900, units = "in"
  )
  
  return(list(
    plot = plot,
    summary_data = summary_data,
    raw_data = raw_data
  ))
}