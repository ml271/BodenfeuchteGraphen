########################################################################################################################
createCompletePlot <- function(
    data,
    selected_variable,
    target_years,
    out_path,
    limits = NULL,
    file_extension = "png",
    moving_average = NULL) {
    
    previous_mean_sd <- data %>%
        filter(year(Datum) < min(target_years)) %>%
        filter(variable == selected_variable) %>%
        data.table() %>%
        createMeanSdTable(moving_average = moving_average)
    
    plot_name <- data %>%
        pull(Plot) %>%
        unique()
    sub_plot_name <- data %>%
        pull(SubPlot) %>%
        unique()
    
    min_year <- data %>%
        pull(Datum) %>%
        year() %>%
        min()
    max_year <- min(target_years) - 1
    mean_label = paste(min_year, max_year, sep = " - ")
    
    mean_plot <- createYearMeanPlot(
        mean_data = previous_mean_sd,
        plot_name, sub_plot_name,
        legend_label = mean_label,
        limits = limits)
    

    colours <- c("#000000", "#C0C0C0", inferno(length(target_years), begin = 0.9, end = 0.5))
    names(colours) <- c(mean_label, "Standardabweichung", target_years)
    mean_plot <- addColourScheme(mean_plot, colours)
    
    
    # Loop does not seem possible as the color of all single years is overwriten by the last one
    target_mean_sd <- data %>%
        filter(year(Datum) == target_years[1]) %>%
        filter(variable == selected_variable) %>%
        data.table() %>%
        createMeanSdTable(moving_average = moving_average)
    mean_plot <- addSingleYear(mean_plot, target_mean_sd, as.character(target_years[1]))
    
    target_mean_sd <- data %>%
        filter(year(Datum) == target_years[2]) %>%
        filter(variable == selected_variable) %>%
        data.table() %>%
        createMeanSdTable(moving_average = moving_average)
    mean_plot <- addSingleYear(mean_plot, target_mean_sd, as.character(target_years[2]))
    
    final_plot <- mean_plot +
        theme(legend.position = "right")
    
    out_file <- paste(plot_name, sub_plot_name, selected_variable, "cm", paste(target_years, collapse = "_"), sep = "_")
    ggsave(paste0(out_path, "/", out_file, ".", file_extension), final_plot)
}
