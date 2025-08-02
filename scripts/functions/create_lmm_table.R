create_lmm_table <- function(model, 
                             outcome_name, 
                             table_title = NULL,
                             save_file = TRUE,
                             output_path = NULL,
                             use_viewer = TRUE) {
  
  # Set default output path
  if (is.null(output_path) && exists("paths")) {
    output_path <- paths$tables
  } else if (is.null(output_path)) {
    output_path <- "."
  }
  
  # Create outcome-specific labels
  outcome_labels <- list(
    QWL = list(
      title = "Table 3. Summary of Quality of Work Life Linear Mixed Model Results",
      dv_label = "QWL Outcome",
      filename = "QWL_lmm_results.html"
    ),
    CFW = list(
      title = "Table 4. Summary of Cognitive Functioning at Work Linear Mixed Model Results", 
      dv_label = "CFW Outcome",
      filename = "CFW_lmm_results.html"
    )
  )
  
  # Use custom title if provided, otherwise use default
  if (is.null(table_title)) {
    if (outcome_name %in% names(outcome_labels)) {
      table_title <- outcome_labels[[outcome_name]]$title
      dv_label <- outcome_labels[[outcome_name]]$dv_label
      filename <- outcome_labels[[outcome_name]]$filename
    } else {
      table_title <- paste("Summary of", outcome_name, "Linear Mixed Model Results")
      dv_label <- paste(outcome_name, "Outcome")
      filename <- paste0(outcome_name, "_lmm_results.html")
    }
  } else {
    # If custom title provided, create generic labels
    dv_label <- paste(outcome_name, "Outcome")
    filename <- paste0(outcome_name, "_lmm_results.html")
  }
  
  # Create the table
  table_result <- sjPlot::tab_model(
    model,
    show.intercept = TRUE,
    show.est = TRUE,
    show.ci = 0.95,
    show.ci50 = FALSE,
    show.se = TRUE,
    show.std = NULL,
    std.response = TRUE,
    show.p = TRUE,
    show.stat = TRUE,
    show.df = TRUE,
    show.zeroinf = TRUE,
    show.r2 = TRUE,
    show.icc = TRUE,
    show.re.var = TRUE,
    show.ngroups = TRUE,
    show.fstat = FALSE,
    show.aic = TRUE,
    show.aicc = FALSE,
    show.dev = FALSE,
    show.loglik = TRUE,
    show.obs = TRUE,
    show.reflvl = FALSE,
    terms = NULL,
    rm.terms = NULL,
    order.terms = c(1, 3, 2, 4),  # Reorder: Intercept, Group, Time, Interaction
    keep = NULL,
    drop = NULL,
    title = table_title,
    pred.labels = c("(Intercept)", "Group", "Time", "Group × Time"),
    dv.labels = c(dv_label),
    wrap.labels = 25,
    bootstrap = FALSE,
    seed = NULL,
    vcov.fun = NULL,
    vcov.args = NULL,
    string.pred = "Variable",
    string.est = "Estimate",
    string.std = "std. Beta",
    string.ci = "95% CI",
    string.se = "SE",
    string.std_se = "standardized SE",
    string.std_ci = "standardized CI",
    string.p = "p",
    string.std.p = "std. p",
    string.df = "df",
    string.stat = "t",
    string.std.stat = "std. Statistic",
    string.resp = "Response",
    string.intercept = "(Intercept)",
    strings = NULL,
    ci.hyphen = "&nbsp;&comma;&nbsp;",
    minus.sign = "&minus;",
    collapse.ci = FALSE,
    collapse.se = FALSE,
    linebreak = TRUE,
    col.order = c("est", "se", "std.est", "std.se", "df.error", "stat", "std.stat", "p", "std.p", "ci", "std.ci", "ci.inner", "ci.outer", "response.level"),
    digits = 2,
    digits.p = 3,
    digits.rsq = 3,
    digits.re = 2,
    emph.p = TRUE,
    p.val = NULL, # kr
    df.method = "satterthwaite",
    p.style = c("numeric"),
    p.threshold = c(0.05, 0.01, 0.001),
    p.adjust = NULL,
    case = "parsed",
    auto.label = TRUE,
    prefix.labels = c("none", "varname", "label"),
    CSS = list(
      css.table = "border-collapse: collapse; border: none; border-bottom: 1px solid black; padding: 0.3em;",
      css.thead = "border-top: 2px solid black; border-bottom: 1px solid black; padding: 0.3em;",
      css.tdata = "border: none; padding: 0.3em;",
      css.arc = "color: black;",
      css.firstsumrow = "border: none;",
      css.summary = "margin-left: 20px;",
      css.summarydata = "margin-left: 0px;",
      css.randomeffects = "padding-left: 30px; font-style: italic;",
      css.modelinfo = "padding-left: 20px;",
      css.caption = "font-weight: bold; text-align: left; margin-bottom: 0.5em;"
    ),
    file = if (save_file) file.path(output_path, filename) else NULL,
    use.viewer = use_viewer,
    encoding = "UTF-8"
  )
  
  # Force display in viewer
  if (use_viewer && interactive()) {
    print(table_result)
  }
  
  # Return the table result
  return(table_result)
}