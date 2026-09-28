# Plot helpers (ggplot2) ------------------------------------------------------

SEX_COLOURS <- c(Males = "#2A6F97", Females = "#C8553D")
COMPONENT_COLOURS <- c(YLL = "#3D405B", YLD = "#81B29A")

theme_bod <- function() {
  ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank(),
                   legend.position = "top",
                   plot.title.position = "plot")
}

age_factor <- function(age) {
  lv <- sort(unique(age))
  factor(age_group_labels(lv)[match(age, lv)], levels = age_group_labels(lv))
}

#' Mirrored age-sex pyramid of counts (males left, females right)
plot_age_sex_pyramid <- function(long, value_label = "Count", title = NULL) {
  df <- long
  df$age_group <- age_factor(df$age)
  df$signed <- ifelse(df$sex == "Males", -df$value, df$value)
  lim <- max(abs(df$signed), na.rm = TRUE)
  ggplot2::ggplot(df, ggplot2::aes(x = age_group, y = signed, fill = sex)) +
    ggplot2::geom_col(width = 0.85) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = function(x) format_num(abs(x)),
                                limits = c(-lim, lim)) +
    ggplot2::scale_fill_manual(values = SEX_COLOURS, name = NULL) +
    ggplot2::labs(x = "Age group", y = value_label, title = title) +
    theme_bod()
}

#' DALY pyramid split into YLL and YLD, males left / females right
plot_daly_pyramid <- function(by_stratum) {
  df <- by_stratum[by_stratum$measure %in% c("YLL", "YLD"), ]
  df$age_group <- age_factor(df$age)
  df$component <- factor(df$measure, levels = c("YLD", "YLL"))
  df$signed <- ifelse(df$sex == "Males", -df$estimate, df$estimate)
  lim <- max(tapply(df$estimate, paste(df$age, df$sex), sum))
  ggplot2::ggplot(df, ggplot2::aes(x = age_group, y = signed, fill = component)) +
    ggplot2::geom_col(width = 0.85) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40") +
    ggplot2::annotate("text", x = Inf, y = -lim * 0.95, label = "Males",
                      hjust = 0, vjust = 1.2, fontface = "bold", colour = SEX_COLOURS[["Males"]]) +
    ggplot2::annotate("text", x = Inf, y = lim * 0.95, label = "Females",
                      hjust = 1, vjust = 1.2, fontface = "bold", colour = SEX_COLOURS[["Females"]]) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_y_continuous(labels = function(x) format_num(abs(x)),
                                limits = c(-lim, lim) * 1.05) +
    ggplot2::scale_fill_manual(values = COMPONENT_COLOURS, name = NULL,
                               breaks = c("YLL", "YLD")) +
    ggplot2::labs(x = "Age group", y = "DALYs (point estimate)") +
    theme_bod()
}

#' Point estimate with 95% UI by age group and sex for one measure
plot_measure_uncertainty <- function(by_stratum, measure = "DALY", conf = 0.95) {
  df <- by_stratum[by_stratum$measure == measure, ]
  df$age_group <- age_factor(df$age)
  pd <- ggplot2::position_dodge(width = 0.6)
  ggplot2::ggplot(df, ggplot2::aes(x = age_group, y = estimate, colour = sex)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                           width = 0.4, position = pd) +
    ggplot2::geom_point(size = 2.2, position = pd) +
    ggplot2::scale_colour_manual(values = SEX_COLOURS, name = NULL) +
    ggplot2::scale_y_continuous(labels = function(x) format_num(x)) +
    ggplot2::labs(x = "Age group",
                  y = sprintf("%s (estimate and %d%% UI)", measure, round(conf * 100))) +
    theme_bod() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   panel.grid.major.y = ggplot2::element_line(colour = "grey90"),
                   panel.grid.major.x = ggplot2::element_blank())
}

#' Economic burden components with 95% UI
plot_economic_burden <- function(totals, currency) {
  df <- totals[totals$measure %in% c("Direct costs", "Productivity loss", "Total economic burden"), ]
  df$measure <- factor(df$measure, levels = c("Direct costs", "Productivity loss", "Total economic burden"))
  ggplot2::ggplot(df, ggplot2::aes(x = measure, y = estimate)) +
    ggplot2::geom_col(fill = c("#5B8E7D", "#3D405B", "#8C6D46"), width = 0.6) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), width = 0.2) +
    ggplot2::scale_y_continuous(labels = function(x) format_num(x)) +
    ggplot2::labs(x = NULL, y = paste0("Amount (", currency, ")")) +
    theme_bod() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(colour = "grey90"),
                   panel.grid.major.x = ggplot2::element_blank())
}
