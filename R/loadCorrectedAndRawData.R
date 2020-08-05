########################################################################################################################
loadCorrectedAndRawData <- function(plot_name, sub_plot_name) {
    al_corrected_data <- loadCorrectedData(loadL2Object(plot_name), sheet.name = sub_plot_name)
    
    al_corrected_fdr <- al_corrected_data %>%
        filter(str_detect(variable, "^[0-9]{2}_FDR")) %>%
        filter(!is.na(value))
    rm(al_corrected_data)
    
    max_corrected_datum <- al_corrected_fdr %>%
        summarise(max = max(Datum)) %>%
        pull()
    
    raw_data <- getData(loadL2Object(plot_name), start.date = max_corrected_datum, sub.plot = sub_plot_name) %>%
        select(-Logger) %>%
        filter(str_detect(variable, "^[0-9]{2}_FDR")) %>%
        filter(!is.na(value))
    
    full_data <- rbindlist(list(al_corrected_fdr, raw_data), use.names = TRUE, fill = TRUE)
}
