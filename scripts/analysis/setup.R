# Clear existing data and graphics
  rm(list=ls())
  graphics.off()

# Set paths
  paths <- list(
    data_raw = "data/raw_data",
    data_out = "data/clean_data",
    functions = "scripts/functions",
    scripts = "scripts/analysis",
    suppl = "scripts/analysis/suppl",
    output = "output",
    plots = "output/plots",
    tables = "output/tables"
  )

# Source all functions----------------------
  files.sources <- list.files(file.path(paths$functions), pattern = "\\.R$", full.names = TRUE, recursive = FALSE)
  sapply(files.sources, source)

# Load packages-------------------------------
  packages <- c("tidyverse", "Hmisc", "lme4", "lmerTest", "emmeans", "performance", "ggrepel", "sjPlot", "rmarkdown")
  load_packages(packages)

# Define vars list
  vars <- list()
  lmm_models <- list()
  lmm_data <- list ()
  plots <- list()
  
# Define custom colours as a named list
  colours <- list(
    yellow = crayon::make_style("#ffcc01"),
    orange = crayon::make_style("#F0BE3A"),
    dark_orange = crayon::make_style("#ff9901"),
    red = crayon::make_style("#CD5C5C"),
    dark_red = crayon::make_style("#fb5858"),
    blue = crayon::make_style("#72D0FF"),
    teal = crayon::make_style("#00bfc4"),
    light_green = crayon::make_style("#96D5A3"),
    green = crayon::make_style("#74AF66"),
    purple = crayon::make_style("#A77BCA")
  )

  # Define group colours for plotting
  vars$predefined_colors = c(
    "Control" = "#F8766D",     # Custom red #F8766D
    "Experimental" = "#00bfc4", # Custom blue #619CFF
    "SMI" = "#f0a640",       # Custom orange #F0A640
    "SMI-ADHD" = "#9c8dc3"       # Custom teal #13c2c7 #9c8dc3
  )
  
# Remove the values from your environment---------------------------------------
  rm(packages, files.sources)
  