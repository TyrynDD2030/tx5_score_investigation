
plot_enumerator_cohens_d <- function(data, province = "Overall", score_var = n_elom_total_score) {
  
  # Capture the input variable
  score_var <- enquo(score_var)
  
  # Subset province if not overall
  if (province != "Overall") {
    data <- data %>% filter(elom_prov == province)
  }
  
  # Compute overall mean and overall standard deviation
  overall_mean <- mean(pull(data, !!score_var), na.rm = TRUE)
  overall_sd   <- sd(pull(data, !!score_var), na.rm = TRUE)
  
  # Compute enumerator-level statistics
  df_summary <- data %>%
    filter(id_enumerator != 451) %>% 
    group_by(id_enumerator) %>%
    summarize(
      mn    = mean(!!score_var, na.rm = TRUE),  
      se    = sd(!!score_var, na.rm = TRUE) / sqrt(n()),  
      lower = mn - 1.96 * se,  
      upper = mn + 1.96 * se,  
      n     = n(),
      .groups = "drop"
    )
  
  # Flag cases based on 0.8 overall SDs from the overall mean
  df_summary <- df_summary %>%
    mutate(
      color_flag = case_when(
        mn > overall_mean + 0.8 * overall_sd ~ "Above +0.8 Overall SD",
        mn < overall_mean - 0.8 * overall_sd ~ "Below -0.8 Overall SD",
        TRUE ~ "Within ±0.8 Overall SD"
      ),
      color_flag = factor(color_flag, 
                          levels = c("Above +0.8 Overall SD", 
                                     "Within ±0.8 Overall SD", 
                                     "Below -0.8 Overall SD"))
    ) %>%
    arrange(mn) %>%
    mutate(idx = row_number())
  
  # Plot
  ggplot(df_summary, aes(x = idx, y = mn, ymin = lower, ymax = upper, color = color_flag)) +
    geom_pointrange(size = 0.5, fatten = 1) +
    geom_hline(yintercept = overall_mean, color = "darkgreen", linetype = "dashed", size = 1) +
    scale_x_continuous(
      breaks = df_summary$idx,
      labels = df_summary$id_enumerator,
      sec.axis = sec_axis(~., breaks = df_summary$idx, labels = df_summary$n)
    ) +
    coord_flip() +
    theme_minimal(base_size = 14) +
    labs(
      x = NULL,
      y = paste("Mean", rlang::as_name(score_var), "± 95% CI"),
      color = "Flagged if >0.8 Overall SDs from Mean"
    ) +
    theme(
      axis.text.y = element_text(size = 8),
      axis.text.y.right = element_text(size = 8),
      legend.position = "bottom"
    ) +
    scale_color_manual(values = c(
      "Above +0.8 Overall SD" = "red", 
      "Within ±0.8 Overall SD" = "gray50", 
      "Below -0.8 Overall SD" = "blue"
    ))
}

