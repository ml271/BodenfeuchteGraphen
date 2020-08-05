########################################################################################################################
loadCorrectedAndRawData <- function(level2_path, plot_name, sub_plot_name) {
    level2_plot <- S4Level2::loadL2Object(level2_path) %>%
        S4Level2::getObjectByURI(plot_name)
    al_corrected_data <- level2_plot %>%
        S4Level2::loadCorrectedData(sub_plot_name) %>%
        filter(stringr::str_detect(variable, "^[0-9]{2}_FDR")) %>%
        filter(!is.na(value))

    max_corrected_datum <- al_corrected_data %>%
        summarise(max = max(Datum)) %>%
        pull()

    raw_data <- level2_plot %>%
        S4Level2::getData(start.date = max_corrected_datum + 60, sub.plot = sub_plot_name) %>%
        select(-Logger) %>%
        filter(stringr::str_detect(variable, "^[0-9]{2}_FDR")) %>%
        filter(!is.na(value))

    rbindlist(list(al_corrected_data, raw_data), use.names = TRUE, fill = TRUE) %>%
        mutate(variable = stringr::str_match(as.character(variable), "[0-9]{2}"))
}
