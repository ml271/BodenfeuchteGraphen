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
load("./data/my2021data.Rdata")
dat_list = mget(ls(pattern = "2021"))
rm(list = ls(pattern= "2021"))
all_2021 <- rbindlist(dat_list)
rm(dat_list)
############### get mean data & 2021 data #######

plot_name = "Altensteig"
subplot_name = "Fichte"
Sensor_name = "FDR"
year <- 2021


####################################################################################

tmp <- gather_data("Altensteig", "Fichte", Sensor_name = c("FDR"))

tmp1 <- qu_cont(tmp)

agg_date_clean <- agg_daily(df = tmp1, na.ratio = 0.5)

#### Current Year ####
current <- agg_date_clean %>%  filter( lubridate::year(Datum)== year) %>%
    #mutate( Datum = lubridate::yday(Datum)) %>%
    #filter(Datum != 366) %>%
    mutate(type = as.character(year))

ggplot(data = current) + geom_line(mapping = aes(x = Datum, y= value, col= variable))
#plot_name <- current %>% select(Plot) %>% unique(.) %>%  pull() %>% as.character()
#subplot_name <- current %>% select(SubPlot) %>% unique(.) %>%  pull() %>% as.character()

#### Old data for means ###
old_tab <- as.data.table(agg_date_clean) %>%
    filter(lubridate::year(Datum) < 2021)
ggplot(data = old_tab) + geom_line(mapping = aes(x = Datum, y= value, col= variable))

#### single var  each Year ####
old_sub <- agg_date_clean %>%
    #filter(variable == "30") %>%
    mutate(year = lubridate::year(Datum)) %>%
    filter(year >= "2010" & year < "2021") %>%
    mutate(year = as.factor(year)) %>%
    mutate(day = lubridate::yday(Datum))

ggplot(data = old_sub)+ geom_line(aes(x= day, y= value, col = year))+
    ylim(0,45)+
    facet_wrap(facets = ~ variable, nrow = 3)



meansd_tab <- old_sub %>%
    filter( Datum >= "2013-01-02") %>%
    #apply moving average and create mean sd table
    BodenfeuchteGraphen::createMeanSdTable(. , moving_average = 1) %>% ungroup() %>%
    mutate(value = mean_value) %>%
    #select(-mean_value) %>%
    mutate(type = "Aggregate")

ggplot(data = meansd_tab, aes(x= Datum)) +
    geom_ribbon(mapping = aes( ymax = value + sd_value, ymin= value -sd_value, fill = variable))+
    ylim(5,45) + facet_wrap(facets = ~ variable, nrow = 3)


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
