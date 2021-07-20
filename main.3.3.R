############## IMPORTS ##########################
devtools::load_all()
library(lubridate)
library(tidyverse)
library(data.table)
library(naniar)
library(ggplot2)
devtools::load_all("W:/R/S4Level2")
devtools::load_all("W:/R/MyUtilities")
devtools::load_all("W:/R/LoggerImports")# Load all packages and functions of this package for main.R
# Initializing and updating S4Level2 needs to be done from S4Level2 itself as using it as a package does not seem to work as intended

############### LODING DATA #####################
S4Level2::connectToExistingDataLocation("W:/Data")
load("W:/R/Datamanagement/my2021data.Rdata")
dat_list = mget(ls(pattern = "2021"))
rm(list = ls(pattern= "2021"))
all_2021 <- rbindlist(dat_list)
rm(dat_list)
############### get mean data & 2021 data #######

plot_name = "Altensteig"
subplot_name = "Fichte"
Sensor_name = "FDR"
year <- 2021

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

############ s QUALITY CONTROL ON 2021 DAT #########################


qu_cont <- function(df){
    # set condition to make quality control #######
    sum(is.na(df$value))
    con_index1 <- df$value == 9999
    sum(con_index1, na.rm= T)
    replacements9999 <- df[con_index1,]

    con_index2 <- df$value >= 100 & df$value != 9999
    sum(con_index2, na.rm= T)
    replacements100 <- df[con_index2,]

    con_index3 <- df$value == -9999
    sum(con_index3, na.rm= T)
    replacements9999m <- df[con_index3,]

    con_index4 <- df$value < 0 & df$value != -9999
    sum(con_index4, na.rm= T)
    replacementsminus <- df[con_index4,]

    # setting values to Na if conditions are true
    df_clean <- df

    df_clean$value[con_index1] <- NA
    df_clean$value[con_index2] <- NA
    df_clean$value[con_index3] <- NA
    df_clean$value[con_index4] <- NA

    return(df_clean)
}

###################### AGGREGATE to daily values, kick nas, #########################
agg_daily <- function(df, na.ratio) # and dropping varibales when containing more than ?? % of missing data
    {
    # Aggregate Data by for each singel day if for each day the data has at least 50% non-na-values
    agg_date <- df %>%
        #aggregate daily values if day more than 50% of data
        mutate(Datum = date(Datum)) %>%
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



####################################################################################

tmp <- gather_data("Altensteig", "Fichte", Sensor_name = c("FDR"))

tmp1 <- qu_cont(tmp)

agg_date_clean <- agg_daily(df = tmp1, na.ratio = 0.5)

current <- agg_date_clean %>%  filter( lubridate::year(Datum)== year) %>%
    mutate( Datum = lubridate::yday(Datum)) %>%
    filter(Datum != 366) %>%
    mutate(type = as.character(year))

plot_name <- current %>% select(Plot) %>% unique(.) %>%  pull() %>% as.character()
subplot_name <- current %>% select(SubPlot) %>% unique(.) %>%  pull() %>% as.character()


ggplot(data = dat_2021) + geom_line(mapping = aes(x = Datum, y= value, col= variable))

meansd_tab <- agg_date_clean %>%  filter( lubridate::year(Datum) <= year) %>%
    #apply moving average and create mean sd table
    BodenfeuchteGraphen::createMeanSdTable(. , moving_average = 10) %>% ungroup() %>%
    mutate(value = mean_value) %>%
    #select(-mean_value) %>%
    mutate(type = "Aggregate")
#-----
join_dat <- meansd_tab %>%  bind_rows( current)
#-------

one_var <- join_dat %>%  filter(variable == "15") %>%  select(-c(Plot, SubPlot))
p <- createYearMeanPlot(mean_data = one_var, plot_name = plot_name, sub_plot_name = subplot_name, limits = c(10,30))
print(p)




d=data.frame(x=c(1,2,3,3,2,1), y=c(1.25,1.0,1.45,1.5, 1.5,1.5), t=c('a', 'a', 'a'), r=c(1,2,3))
ggplot() +
    geom_polygon(data=d, mapping=aes(x=x, y=y, group=t))+
    ylim(0,2)+ xlim(0,4)



polygon <- meansd_dyear %>%
    group_by(variable) %>%
    group_map(~ {
        MyUtilities::createSdPolygon(.x$Datum, .x$mean_value, .x$sd_value) %>%
            mutate(variable = unique(.x$variable)) %>%
            select(variable, Datum = x, value = y)
    }, keep = TRUE) %>%
    bind_rows() %>%
    mutate(type = "Polygon")


joined_data <- meansd_dyear %>%
    select(-sd_value) %>%
    bind_rows(polygon, dat_2021) %>%
    mutate(type = factor(type, levels = c("Polygon", "Aggregate", as.character(year))))

#----------------------------------------------------------------------------------------

ggplot(data = polygon, mapping = aes(x = Datum, y = value)) +
    geom_polygon(mapping= aes(group = variable, fill = variable ))+
    geom_line(data = meansd_dyear, mapping = aes(x = Datum, y= value, col = variable))



    geom_line(aes(color = variable)) +
    facet_wrap(facets = ~ variable, nrow = 3)

BodenfeuchteGraphen::createCombiGraphs(long.table = joined_data, out.path = "W:/R/BodenfeuchteGraphen/grafikoutput")
