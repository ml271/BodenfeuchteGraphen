

###################### AGGREGATE to daily values, kick nas, #########################
agg_daily <- function(df, na.ratio) # and dropping varibales when containing more than ?? % of missing data
{
    # Aggregate Data by for each singel day if for each day the data has at least 50% non-na-values
    agg_date <- df %>%
        #aggregate daily values if day more than 50% of data
        mutate(Datum =  lubridate::date(Datum)) %>%
        group_by(Datum,Plot, SubPlot, variable) %>%
        summarise(
            ratio=sum(is.na(value))/n(),
            value=mean(value, na.rm=TRUE)*ifelse(ratio >= na.ratio, NA, 1)
        ) %>% ungroup()


    # select variables containg more then 50 % NA
    cols_drop <- agg_date %>% group_by(Plot, SubPlot, variable) %>%
        summarise(ratio=sum(is.na(value))/n()) %>% ungroup() %>%
        filter( ratio >= na.ratio) %>% pull(variable)

    # drop those variables
    if ( length(cols_drop) >= 1) {
        agg_date_clean <- agg_date %>%  filter( variable != cols_drop) %>% select(-ratio)
        print(paste( "Variable(s): ", cols_drop, "dropped, because more then 50 % missing values"))
    } else {
        agg_date_clean <- agg_date %>% select(-ratio)
        print(" No variables dropped")
    }

    agg_depths <- agg_date_clean %>%
        #aggregate depths
        mutate(variable = as.factor(stringr::str_match(variable, "[0-9]{2}"))) %>%
        group_by(Datum, Plot, SubPlot, variable) %>%
        summarise(value = mean(value, na.rm=TRUE)) %>%  ungroup()

    return(agg_depths)
}
