########################################################################################################################
createMeanSdTable <- function(data, moving_average = NULL) {
    # calculate daily mean and sd for all years up the most recent one
    mean_sd_table <- data %>%
        mutate(Datum = lubridate::yday(Datum),
               value_sd = value) %>% # Added as dplyr can't summarise multiple times from a single column
        group_by(Datum, variable) %>%
        summarise(mean_value = mean(value, na.rm = TRUE),
                  sd_value = sd(value_sd, na.rm = TRUE)) %>%
        filter(Datum != 366) # removal of extra leap day (often strange values)

    if (is.numeric(moving_average)) {
        mean_sd_table <- mean_sd_table %>%
            group_by(variable) %>%
            mutate_at(vars(mean_value, sd_value), zoo::rollapply, width = moving_average, FUN = mean, na.rm = TRUE, partial = TRUE)
    }
    return(mean_sd_table)
}
