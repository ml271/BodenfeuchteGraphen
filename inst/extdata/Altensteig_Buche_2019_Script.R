########################################################################################################################
plot_name <- "Altensteig"
sub_plot_name <- "Buche"
data_table <- loadCorrectedAndRawData(level2_path, plot_name, sub_plot_name) %>%



########################################################################################################################
# Dropping faulty data


########################################################################################################################
# Create Graphs and manually fix data from them
#createRawGraphs(as.data.table(data_table), raw_graph_path)
#createCombiGraphs(as.data.table(data_table), raw_graph_path)

out_path <- file.path("graphs", plot_name, sub_plot_name)
createCompletePlot(data_table,
    selected_variable = "15",
    target_years = target_years,
    out_path,
    limits = global_limits,
    file_extension = global_file_type,
    moving_average = moving_average)

createCompletePlot(data_table,
    selected_variable = "30",
    target_years = target_years,
    out_path,
    limits = global_limits,
    file_extension = global_file_type,
    moving_average = moving_average)

createCompletePlot(data_table,
    selected_variable = "60",
    target_years = target_years,
    out_path,
    limits = global_limits,
    file_extension = global_file_type,
    moving_average = moving_average)



