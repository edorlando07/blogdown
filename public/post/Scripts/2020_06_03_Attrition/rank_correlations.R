rank_correlations <- function(data) {
  
  data <- data %>%
    binarize() %>%
    correlate("Attrition__Yes") %>%
    mutate(correlation_abs = abs(correlation)) %>%
    select(feature, correlation_abs) %>%
    group_by(feature) %>%
    summarise(correlation_abs = max(correlation_abs)) %>%
    ungroup %>%
    mutate(corr_rank = rank(-correlation_abs)) %>%
    select(feature, corr_rank)
}