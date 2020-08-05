########################################################################################################################
addSingleYear <- function(base_plot, target_year_data, color_label) {
    out_plot <- base_plot +
        geom_line(
            data = target_year_data,
            mapping = aes(x = Datum, y = mean_value, color = color_label),
            size = 1.3)
    return(out_plot)
}
