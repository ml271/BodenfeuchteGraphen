########################################################################################################################
createYearMeanPlot <- function(
    mean_data,
    plot_name,
    sub_plot_name,
    legend_label,
    limits = NULL,
    expand = waiver()) {

    variable_number <- mean_data %>%
        pull(variable) %>%
        unique() %>%
        length()
    assertthat::assert_that(variable_number == 1)

    title.string <- paste0("Bodenfeuchte an der Versuchsfläche ", plot_name)
    Encoding(title.string) <- "UTF-8"
    subtitle_string <- mean_data %>%
        pull(variable) %>%
        unique() %>%
        stringr::str_match("[0-9]{2}") %>%
        paste0("In ", ., " cm Tiefe")

    base.plot <- ggplot() +
        theme(panel.grid.minor.y = element_blank(), panel.grid.major = element_line(colour = "grey")) +
        theme_bw() +
        ggtitle(title.string, subtitle = subtitle_string) +
        scale_y_continuous(name = "Bodenfeuchte (Vol.-%)", limits = limits, expand = expansion(0.01))

    sd_polygon <- create_sd_polygon(mean_data$Datum, mean_data$mean_value, mean_data$sd_value)
    p.prev <- base.plot +
        geom_polygon(data = sd_polygon, mapping = aes(x = x, y = y, fill = "Standardabweichung")) +
        geom_line(data = mean_data,
            mapping = aes(x = Datum, y = mean_value, color = legend_label), size = 0.75)

    month.breaks <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
    month.abbrv <- c("Jan", "Feb", "M\U00E4r", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez")
    p.final <- p.prev +
        scale_x_continuous(name = "Monat", minor_breaks = month.breaks, breaks = month.breaks,
            labels = month.abbrv, expand = expand) +
        theme(legend.position = "none")

    return(p.final)
}
