########################################################################################################################
createMeanSdTable <- function(data, moving_average = NULL) {
    # calculate daily mean and sd for all years up the most recent one
    mean_sd_table <- data[, .(
            value = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE)),
        by = .(Datum = yday(Datum), variable)]
    setkey(mean_sd_table, variable, Datum)
    
    mean_sd_table <- mean_sd_table[Datum != 366] # removal of extra leap day (often strange values)
    if (is.numeric(moving_average)) {
        mean_sd_table[, ":=" (
                value = rollapply(value, width = moving_average, FUN = mean, na.rm = TRUE, partial = TRUE),
                sd = rollapply(sd, width = moving_average, FUN = mean, na.rm = TRUE, partial = TRUE)),
            by = .(variable)]
    }
    return(mean_sd_table)
}
