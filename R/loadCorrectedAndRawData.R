########################################################################################################################
loadCorrectedAndRawData <- function(level2_path, plot_name, sub_plot_name) {
    al_corrected_data <- S4Level2::loadCorrectedData(plot_name, sub_plot_name) %>%
        filter(stringr::str_detect(variable, "^[0-9]{2}_FDR")) %>%
        filter(!is.na(value)) %>%
        mutate(Plot = plot_name)

    max_corrected_datum <- al_corrected_data %>%
        pull(Datum) %>%
        max()

    raw_data <- S4Level2::getData(
        start_date = max_corrected_datum + 60,
        end_date = "2300-01-01",
        plot = plot_name,
        sub_plot = sub_plot_name) %>%
        select(-Logger) %>%
        filter(stringr::str_detect(variable, "^[0-9]{2}_FDR")) %>%
        filter(!is.na(value))

    rbindlist(list(al_corrected_data, raw_data), use.names = TRUE, fill = TRUE)
}
