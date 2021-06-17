########################################################################################################################
createCompletePlot <- function(
    data,
    selected_variable,
    target_years,
    limits = NULL,
    file_extension = "png",
    moving_average = NULL) {

    data <- data  %>%
        mutate(variable = stringr::str_match(as.character(variable), "[0-9]{2}")) %>%
        filter(variable == selected_variable)

    previous_mean_sd <- data %>%
        filter(lubridate::year(Datum) < min(target_years)) %>%
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

    colours <- c("#000000", "#C0C0C0", viridis::inferno(length(target_years), begin = 0.9, end = 0.5))
    names(colours) <- c(mean_label, "Standardabweichung", target_years)
    mean_plot <- addColourScheme(mean_plot, colours)

    # Loop does not seem possible as the color of all single years is overwriten by the last one
    target_mean <- data %>%
        filter(lubridate::year(Datum) %in% target_years) %>%
        mutate(Datum = as.Date(Datum)) %>%
        group_by(Datum, variable) %>%
        summarise(mean_value = mean(value, na.rm = TRUE))

    if (is.numeric(moving_average)) {
        target_mean <- target_mean %>%
            group_by(variable) %>%
            mutate(mean_value = zoo::rollapply(mean_value, width = moving_average, FUN = mean, na.rm = TRUE, partial = TRUE))
    }

    mean_plot <- target_mean %>%
        filter(lubridate::year(Datum) == target_years[1]) %>%
        mutate(Datum = lubridate::yday(Datum)) %>%
        addSingleYear(mean_plot, ., as.character(target_years[1]))

    mean_plot <- target_mean %>%
        filter(lubridate::year(Datum) == target_years[2]) %>%
        mutate(Datum = lubridate::yday(Datum)) %>%
        addSingleYear(mean_plot, ., as.character(target_years[2]))

    final_plot <- mean_plot +
        theme(legend.position = "right")
    return(final_plot)
}
