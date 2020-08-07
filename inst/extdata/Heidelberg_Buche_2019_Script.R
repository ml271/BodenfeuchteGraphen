########################################################################################################################
plot_name <- "Heidelberg"
sub_plot_name <- "Buche"
data_table <- loadCorrectedAndRawData(level2_path, plot_name, sub_plot_name)


########################################################################################################################
# Dropping faulty data
filtered_data <- data_table %>%
    filter(value > 0)


########################################################################################################################
# Create Graphs and manually fix data from them
out_path <- file.path("graphs")
raw_graph_path <- file.path(out_path, plot_name, sub_plot_name)
# createCombiGraphs(as.data.table(filtered_data), raw_graph_path)
# createRawGraphs(as.data.table(filtered_data), raw_graph_path)

createCompletePlot(filtered_data,
    selected_variable = "30",
    target_years = target_years,
    out_path,
    limits = global_limits,
    file_extension = global_file_type,
    moving_average = moving_average)

createCompletePlot(filtered_data,
    selected_variable = "60",
    target_years = target_years,
    out_path,
    limits = global_limits,
    file_extension = global_file_type,
    moving_average = moving_average)


