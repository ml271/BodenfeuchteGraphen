########################################################################################################################
plot_name <- "Conventwald"
sub_plot_name <- "Fichte"
data_table <- loadCorrectedAndRawData(level2_path, plot_name, sub_plot_name)




########################################################################################################################
# Dropping faulty data
filtered_data <- data_table %>%
    filter(value > 0) %>%
    filter(value < 100) %>%
    filter(!(Datum >= as.POSIXct("2019-01-01", tz = "UTC") & variable == "60_FDR_Z")) %>%
    filter(!(Datum >= as.POSIXct("2019-09-03", tz = "UTC") & variable == "15_FDR_X")) %>%
    mutate(variable = stringr::str_replace(variable, "KR", "Y"))


########################################################################################################################
# Create Graphs and manually fix data from them
out_path <- file.path("graphs")
raw_graph_path <- file.path(out_path, plot_name, sub_plot_name)
# createCombiGraphs(as.data.table(filtered_data), raw_graph_path)
# createRawGraphs(as.data.table(filtered_data), raw_graph_path)


# Drop positions
co_15 <- createCompletePlot(filtered_data,
                   selected_variable = "15",
                   target_years = target_years,
                   limits = global_limits,
                   file_extension = global_file_type,
                   moving_average = moving_average)
ggsave(file.path(out_path, "Conventwald_Fichte_15_cm_2019_2020.png"), co_15)

co_30 <- createCompletePlot(filtered_data,
                   selected_variable = "30",
                   target_years = target_years,
                   limits = global_limits,
                   file_extension = global_file_type,
                   moving_average = moving_average)
ggsave(file.path(out_path, "Conventwald_Fichte_30_cm_2019_2020.png"), co_30)

co_60 <- createCompletePlot(filtered_data,
                   selected_variable = "60",
                   target_years = target_years,
                   limits = global_limits,
                   file_extension = global_file_type,
                   moving_average = moving_average)
ggsave(file.path(out_path, "Conventwald_Fichte_60_cm_2019_2020.png"), co_60)

