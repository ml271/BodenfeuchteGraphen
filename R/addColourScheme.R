########################################################################################################################
addColourScheme <- function(plot, colours) {
    out_plot <- plot + scale_color_manual(name = "Tagesmittelwerte", values = colours) +
        scale_fill_manual(name = "", values = colours)
    return(out_plot)
}
