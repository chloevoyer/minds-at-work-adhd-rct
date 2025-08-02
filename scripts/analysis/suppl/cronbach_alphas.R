# Calculate Cronbach's Alpha for QWLQ (40 items)
# ==============================================================================
  qwlq_items_pre <- long_data %>%
    filter(ID %in% vars$included_ids) %>%
    filter(time == 0) %>%
    select(matches("^QWLQ_[a-z]+_[1-9][0-9]?$"))
  
  qwlq_items_post<- long_data %>%
    filter(ID %in% vars$included_ids) %>%
    filter(time == 1) %>%
    select(matches("^QWLQ_[a-z]+_[1-9][0-9]?$"))
  
  qwlq_keys <- rep(1, 40)  # Start with all items as positive
  qwlq_keys[c(3, 18, 34)] <- -1  # Reverse only the items you know should be reversed

qwlq_alpha_pre <- psych::alpha(qwlq_items_pre, keys = qwlq_keys)
qwlq_alpha_post <- psych::alpha(qwlq_items_post, keys = qwlq_keys)
cat("QWLQ Pre-intervention Cronbach's Alpha:", round(qwlq_alpha_pre$total$raw_alpha, 3), "\n")
cat("QWLQ Post-intervention Cronbach's Alpha:", round(qwlq_alpha_post$total$raw_alpha, 3), "\n")

# Calculate Cronbach's Alpha for CFWQ (29 items, reversed scores)
# ==============================================================================
# Pre-intervention CFWQ (using reversed items)
cfwq_items_pre <- long_data %>%
  filter(ID %in% vars$included_ids) %>%
  filter(time == 0) %>%
  select(matches("^CFWQ_[a-z]+_[1-9][0-9]?$"))

cfwq_items_post<- long_data %>%
  filter(ID %in% vars$included_ids) %>%
  filter(time == 1) %>%
  select(matches("^CFWQ_[a-z]+_[1-9][0-9]?$"))

cfwq_alpha_pre <- psych::alpha(cfwq_items_pre, check.keys = TRUE)
cfwq_alpha_post <- psych::alpha(cfwq_items_post, check.keys = TRUE)

cat("CFWQ Pre-intervention Cronbach's Alpha:", round(cfwq_alpha_pre$total$raw_alpha, 3), "\n")
cat("CFWQ Post-intervention Cronbach's Alpha:", round(cfwq_alpha_post$total$raw_alpha, 3), "\n")
