########################################################################################################################
createCombiGraphs <- function(long.table, out.path) {
    dir.create(out.path, showWarnings = FALSE, recursive = TRUE)
    for (year in long.table[, unique(year(Datum))]) {
        p <- ggplot(long.table[year(Datum) == year], aes(x = Datum, y = value, color = variable)) +
            viridis::scale_color_viridis(discrete = TRUE) +
            scale_y_continuous(limits = long.table[, c(min(value), max(value))]) +
            geom_point()
        dir.create(out.path, showWarnings = FALSE, recursive = TRUE)
        ggsave(paste0(out.path, "/Kombi_", year, ".png"), plot = p)
    }
}
