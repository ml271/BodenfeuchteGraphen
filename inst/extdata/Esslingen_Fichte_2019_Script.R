########################################################################################################################
plot <- "Esslingen"
sub_plot <- "Fichte"
data_table <- loadCorrectedAndRawData(plot, sub_plot)


########################################################################################################################
# Dropping faulty data


########################################################################################################################
# Create Graphs and manually fix data from them
raw_graph_path <- file.path(getwd(), "Graphs", plot, sub_plot)
#createRawGraphs(as.data.table(data_table), raw_graph_path)
#createCombiGraphs(as.data.table(data_table), raw_graph_path)

# Drop positions
data_table <- data_table %>%
    mutate(variable = str_match(variable, pattern = "[0-9]{2}"))

out_path <- file.path(raw_graph_path, "..", "..")
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


