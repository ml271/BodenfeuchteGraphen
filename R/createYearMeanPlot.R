########################################################################################################################
createYearMeanPlot <- function(
    mean_data,
    plot_name,
    sub_plot_name,
    legend_label,
    limits = NULL,
    expand = waiver()) {
    
    if (mean_data[, length(unique(variable)) > 1]) {
        stop("Pass only tables with a single variable value")
    }
    
    title.string <- paste0("Bodenfeuchte an der Versuchsfläche ", plot_name)
    Encoding(title.string) <- "UTF-8"
    
    base.plot <- ggplot() +
        theme(panel.grid.minor.y = element_blank(), panel.grid.major = element_line(colour = "grey")) +
        theme_bw() +
        ggtitle(title.string, subtitle = 
                paste0("In ", str_match(mean_data[, unique(variable)], "[0-9]{2}"), " cm Tiefe")) +
        scale_y_continuous(name = "Bodenfeuchte (Vol.-%)", limits = limits, expand = expand_scale(0.01))
    
    sd_polygon <- mean_data[, .(
            x = c(Datum, rev(Datum)),
            y = c(value + sd, rev(value - sd)))]
    p.prev <- base.plot +
        geom_polygon(data = sd_polygon, mapping = aes(x = x, y = y, fill = "Standardabweichung")) +
        geom_line(data = mean_data,
            mapping = aes(x = Datum, y = value, color = legend_label), size = 0.75)
    
    month.breaks <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
    month.abbrv <- c("Jan", "Feb", "M\U00E4r", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez")
    p.final <- p.prev +
        scale_x_continuous(name = "Monat", minor_breaks = month.breaks, breaks = month.breaks, 
            labels = month.abbrv, expand = expand) +
        theme(legend.position = "none")
    
    return(p.final)
}
