devtools::load_all() # Load all packages and functions of this package for main.R

# Following functions only need to be used once to initialize S4Level2 package
# S4Level2::initializeDataLocation("W:/Data")
# S4Level2::initializeWithPresetPlots()

S4Level2::updateDatabase() # Run if there is new Data


########################################################################################################################
target_years = c(2019, 2020)
global_limits = c(5, 41)
moving_average = 10
global_file_type = "png"

# TODO: Replace the following sourced scripts with general functions derived from below to aggregate Plots together
# system.file("extdata", package = "BodenfeuchteGraphen") %>%
#     dir(full.names = TRUE) %>%
#     purrr::walk(~ source(.x))


plot_name <- "Altensteig"
sub_plot_name <- "Fichte"
data_table <- loadCorrectedAndRawData(plot_name, sub_plot_name)

filtered_data <- data_table %>%
    filter(!(Datum >= as.POSIXct("2020-01-01", tz = "UTC")
             & variable == "60_FDR_Y"))
# al_15_plot <- createCompletePlot(filtered_data, "15", target_years, global_limits, moving_average = moving_average)
# al_30_plot <- createCompletePlot(filtered_data, "30", target_years, global_limits, moving_average = moving_average)
# al_60_plot <- createCompletePlot(filtered_data, "60", target_years, global_limits, moving_average = moving_average)
#
# ggplot(data = filtered_data, mapping = aes(x = Datum))


al_full_data <- filtered_data %>%
    select(-SubPlot, -Plot) %>%
    mutate(Datum = as.Date(Datum)) %>%
    mutate(variable = as.factor(stringr::str_match(variable, "[0-9]{2}"))) %>%
    na.omit()

al_aggregate <- al_full_data %>%
    filter(Datum < as.Date(paste0(target_years[1], "-01-01"))) %>%
    mutate(Datum = lubridate::yday(Datum)) %>%
    filter(Datum != 366) %>%
    group_by(variable, Datum) %>%
    summarise(sd_value = sd(value), value = mean(value)) %>%
    mutate_at(vars(value, sd_value), zoo::rollapply, width = moving_average, FUN = mean, na.rm = TRUE, partial = TRUE) %>%
    mutate(type = "Aggregate") %>%
    ungroup()

al_polygon <- al_aggregate %>%
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
