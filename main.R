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
rm(dat_2021)
load("W:/R/Datamanagement/my2021data.Rdata")
dat_list = mget(ls(pattern = "2021"))
rm(list = ls(pattern= "2021"))
all_2021 <- rbindlist(dat_list)
rm(dat_list)
############### get mean data & 2021 data #######
year <- 2020
plot_name <- "Altensteig"
subplot_name <- "Fichte"
Sensor_name <- "FDR"

mean_dat <- S4Level2::getData( paste0(year-10,"-01-01"), paste0(year,"-12-31"), plot_name , subplot_name)
#mean_dat_wide <- pivot_wider(mean_dat, id_cols = Datum, names_from = variable, values_from = value)
range(mean_dat$Datum)
mean_dat %>% distinct(variable) %>% print(n=100)


dat_2021 <- all_2021 %>%  filter( Plot == plot_name & SubPlot == subplot_name)
#dat_2021_wide <- pivot_wider(dat_2021, id_cols = Datum, names_from = variable, values_from = value)
range(dat_2021$Datum)
dat_2021 %>% distinct(variable) %>% print(n=100)

############## select only Bodenfeuchte out of data #######
Bofu_2021 <-  dat_2021 %>%
    pivot_wider(id_cols = c(Datum, Plot, SubPlot), names_from = variable, values_from = value) %>%
    select(Datum,Plot,SubPlot, contains(Sensor_name)) %>%
    pivot_longer( names_to = "variable", values_to = "value", cols= -c(Datum, Plot, SubPlot))

Bofu_mean <-  mean_dat %>%
    pivot_wider(id_cols = c(Datum, Plot, SubPlot), names_from = variable, values_from = value) %>%
    select(Datum,Plot, SubPlot, contains(Sensor_name)) %>%
    pivot_longer( names_to = "variable", values_to = "value", cols= -c(Datum, Plot, SubPlot))

        ############### make plot to overview BEFORE data quality control ########
        ggplot(dat_2021[which(str_detect(dat_2021$variable, Sensor_name))]) + geom_point(aes(x = Datum, y = value, col = variable))
        #TODO: Find soulution for overflowing vector allocate!
        ggplot(mean_dat[which(str_detect(mean_dat$variable, Sensor_name))]) + geom_point(aes(x = Datum, y = value, col = variable))

############# set condition to make quality control #######
#TODO: Figure out teh ebst way to make condition replacement of NA
sum(is.na(Bofu_2021$value))
Bofu_2021_clean <- Bofu_2021

con_index1 <- Bofu_2021$value == 9999
sum(con_index1, na.rm= T)
replacements9999 <- Bofu_2021[con_index1,]
Bofu_2021_clean$value[con_index1] <- NA

con_index2 <- Bofu_2021$value >= 100 & Bofu_2021$value != 9999
sum(con_index2, na.rm= T)
replacements100 <- Bofu_2021_clean[con_index2,]
Bofu_2021_clean$value[con_index2] <- NA

con_index3 <- Bofu_2021$value == -9999
sum(con_index3, na.rm= T)
replacements9999m <- Bofu_2021[con_index3,]
Bofu_2021_clean$value[con_index3] <- NA

con_index4 <- Bofu_2021$value <= 0 & Bofu_2021$value != -9999
sum(con_index4, na.rm= T)
replacementsminus <- Bofu_2021_clean[con_index4,]
Bofu_2021_clean$value[con_index4] <- NA


# Aggregate Data by for each singel day if for each day the data has at least 50% non-na-values
agg_date <- Bofu_2021_clean %>%
    mutate(Datum = date(Datum)) %>%
    group_by(Datum,Plot, SubPlot, variable) %>%
    summarise(
        ratio=sum(is.na(value))/n(),
        mean=mean(value, na.rm=TRUE)*ifelse(ratio >= 0.5, NA, 1)
    ) %>% ungroup() %>%  drop_na()

# select variables containg more then 50 % NA
cols_drop <- agg_date %>% group_by(Plot, SubPlot, variable) %>%
    summarise(ratio=sum(is.na(mean))/n()) %>% ungroup() %>%
    filter( ratio >= 0.5) %>% pull(variable)

# drop those variables
if ( length(cols_drop) >= 1) {
    agg_date_clean <- agg_date %>%  filter( variable != cols_drop) %>% select(-ratio)
    print(paste( "Variable(s): ", cols_drop, "dropped, because more then 50 % missing values"))
    } else {
    agg_date_clean <- agg_date %>% select(-ratio)
    print(" No variables dropped")
    }

ggplot(data = agg_date_clean) + geom_point(aes(x = Datum, y= mean, col= variable ))


# aggregate variables of the same depth


####################### plot clena date ##################
ggplot(agg_date) + geom_point(aes(x = DATE, y = mean, col = variable))

ggplot(Bofu_2021_clean[which(str_detect(Bofu_2021_clean$variable, Sensor_name)),]) + geom_point(aes(x = Datum, y = value, col = variable))
#TODO: WHY IS NOT WORKING??
ggplot(Bofu_mean[which(str_detect(Bofu_mean$variable, Sensor_name))]) + geom_point(aes(x = Datum, y = value, col = variable))














########################################################################################################################
target_years = c(2020, 2021)
global_limits = c(5, 41)
moving_average = 10
global_file_type = "png"


# TODO: Replace the following sourced scripts with general functions derived from below to aggregate Plots together
# system.file("extdata", package = "BodenfeuchteGraphen") %>%
#     dir(full.names = TRUE) %>%
#     purrr::walk(~ source(.x))

dat_spl_2021 <- dat_2021 %>%  filter( Plot == plot_name & SubPlot == subplot_name)
dat_spl_2021 %>% distinct(variable) %>% print(n=100)

filtered_data_2021 <- dat_spl_2021 %>%
    pivot_wider(id_cols = Datum, names_from = variable, values_from = value) %>%
    select(Datum, contains("TDR")) %>%
    pivot_longer( names_to = "variable", values_to = "value", cols= -Datum) %>%
    mutate( Plot = plot_name , SubPlot = subplot_name) %>%
    filter(!(variable %in% c( "60K_ES_Fi_TDR","60KR_ES_Fi_TDR" )))
temp <-
    filtered_data_2021 %>%  filter(variable == "60K_ES_Fi_TDR")
# es_15_plot <- createCompletePlot(filtered_data_2021, "15", target_years, global_limits, moving_average = moving_average)
# es_30_plot <- createCompletePlot(filtered_data_2021, "30", target_years, global_limits, moving_average = moving_average)
# es_60_plot <- createCompletePlot(filtered_data_2021, "60", target_years, global_limits, moving_average = moving_average)




full_data <- agg_date_clean %>%
    mutate(Datum = as.Date(Datum)) %>%
    mutate(variable = as.factor(stringr::str_match(variable, "[0-9]{2}"))) %>%
    na.omit()
ggplot(data = full_data) + geom_point(aes(x = Datum, y= mean, col= variable ))

aggregate_depths <- full_data %>%
    #filter(Datum < as.Date(paste0(target_years[1], "-01-01"))) %>%
    mutate(Datum = lubridate::yday(Datum)) %>%
    filter(Datum != 366) %>%
    group_by(variable, Datum) %>%
    summarise(sd_value = sd(value), value = mean(value)) %>%
    mutate_at(vars(value, sd_value), zoo::rollapply, width = moving_average, FUN = mean, na.rm = TRUE, partial = TRUE) %>%
    mutate(type = "Aggregate") %>%
    ungroup()

polygon <- aggregate_depths %>%
    group_by(variable) %>%
    group_map(~ {
        MyUtilities::createSdPolygon(.x$Datum, .x$value, .x$sd_value) %>%
            mutate(variable = unique(.x$variable)) %>%
            select(variable, Datum = x, value = y)
    }, keep = TRUE) %>%
    bind_rows() %>%
    mutate(type = "Polygon")

al_current <- al_full_data %>%
    filter(lubridate::year(Datum) %in% target_years) %>%
    mutate(Jahr = lubridate::year(Datum)) %>%
    group_by(variable, Datum) %>%
    summarise(value = mean(value)) %>%
    mutate_at(vars(value), zoo::rollapply, width = moving_average, FUN = mean, na.rm = TRUE, partial = TRUE) %>%
    mutate(type = as.character(lubridate::year(Datum))) %>%
    mutate(Datum = lubridate::yday(Datum)) %>%
    ungroup()


joined_data <- al_aggregate %>%
    select(-sd_value) %>%
    bind_rows(al_polygon, al_current) %>%
    mutate(type = factor(type, levels = c("Polygon", "Aggregate", "2019", "2020")))

ggplot(data = joined_data, mapping = aes(x = Datum, y = value)) +
    geom_line(aes(color = type)) +
    facet_wrap(facets = ~ variable, nrow = 3)
