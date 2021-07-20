
gather_data <-function(plot_name, subplot_name, Sensor_name)

    # Thsi function gathers data from
    # 1) S4level2::DataLocation(w:/data) which contains corrected old data and
    # 2) it collects recent data from Datamangement/my2021data.Rdata for 2021 data
{
    mean_dat <- S4Level2::getData( paste0("2010-01-01"), paste0("2020-12-31"), plot_name , subplot_name)
    #mean_dat_wide <- pivot_wider(mean_dat, id_cols = Datum, names_from = variable, values_from = value)
    print(paste("Range mean dat :", range(mean_dat$Datum)))
    #print(mean_dat %>% distinct(variable) %>% print(n=100))

    dat_2021 <- all_2021 %>%  filter( Plot == plot_name & SubPlot == subplot_name)
    #dat_2021_wide <- pivot_wider(dat_2021, id_cols = Datum, names_from = variable, values_from = value)
    print(paste("Range 2021 dat: ", range(dat_2021$Datum)))
    #print(dat_2021 %>% distinct(variable) %>% print(n=100))

    ############## select only Bodenfeuchte out of data #######
    Bofu_mean <-  mean_dat %>%
        pivot_wider(id_cols = c(Datum, Plot, SubPlot), names_from = variable, values_from = value) %>%
        select(Datum,Plot, SubPlot, contains(Sensor_name)) %>%
        pivot_longer( names_to = "variable", values_to = "value", cols= -c(Datum, Plot, SubPlot)) %>%
        mutate(type = )


    Bofu_2021 <-  dat_2021 %>%
        pivot_wider(id_cols = c(Datum, Plot, SubPlot), names_from = variable, values_from = value) %>%
        select(Datum,Plot,SubPlot, contains(Sensor_name)) %>%
        pivot_longer( names_to = "variable", values_to = "value", cols= -c(Datum, Plot, SubPlot))

    dat <- rbind(Bofu_mean, Bofu_2021) %>% arrange(Datum)
    return(dat)
}



