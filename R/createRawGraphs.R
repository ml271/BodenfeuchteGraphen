########################################################################################################################
createRawGraphs <- function(long.table, out.path) {
    dir.create(out.path, showWarnings = FALSE)
    for (year in long.table[, unique(year(Datum))]) {
        for (var.name in long.table[, unique(variable)]) {
            p <- ggplot(long.table[year(Datum) == year & variable == var.name], aes(x = Datum, y = value)) +
                geom_point() +
                scale_y_continuous(limits = long.table[, c(min(value), max(value))])
            ggsave(paste0(out.path, "/", var.name, "_", year, ".png"), plot = p)
        }
    }
}
