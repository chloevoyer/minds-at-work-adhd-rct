# Summaries
create_summary(analysis_data %>% filter(group == 1), "Experimental")
create_summary(analysis_data %>% filter(group == 0), "Control")
create_summary(analysis_data, "Both")
summary_by_group_total(analysis_data, group, ASRS_total)

source(file.path(paths$suppl, "extra_sample_stats.R"))
source(file.path(paths$suppl, "completion_rates.R"))
