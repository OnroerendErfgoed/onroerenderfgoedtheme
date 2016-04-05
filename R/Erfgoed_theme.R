library(ggplot2)
#' Function to get default font family
#'
#' @return A font family name
get_default_font <- function() {
  extrafont::choose_font(c("FlandersArtSans", "Helvetica"))
}

#' ggplot2theme
#' @param base_size Base fontsize for theme (default = 12)
#' @param base_family Default family used for plot. Defaults to
#' FlandersArtSans, with a fallback to Helvetica.
#' @export
theme_erfgoed <- function(base_size = 12, base_family = get_default_font()) {
  ggplot2::update_geom_defaults("line", list(colour="#944EA1", size=1))
  ggplot2::update_geom_defaults("bar", list(colour="black", fill = "#944EA1"))
  ggplot2::update_geom_defaults("point", list(shape = 1, size = 2.5))

  t <- ggplot2::theme_bw(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(
          panel.grid.major.y = ggplot2::element_line(size= 0.6, colour = "#d9d9d9"),
          panel.grid.major.x = ggplot2::element_line(size= 0.6, colour = "#d9d9d9"),
          panel.grid.minor.y = ggplot2::element_line(size= 0.3, colour = "#b3b3b3", linetype = "dotted"),
          panel.grid.minor.x = ggplot2::element_line(size= 0.3, colour = "#b3b3b3", linetype = "dotted"),

          plot.title = ggplot2::element_text(size = 15, face= "bold", margin = margin(20,20,20,20)),

          axis.text = ggplot2::element_text (size= 12),
          axis.title.y = ggplot2::element_text(size= 12, face = "bold",angle = 90, margin = margin(1,15,1,1)),
          axis.title.x = ggplot2::element_text(size= 12, face = "bold", margin = margin(15,1,1,1)),

          legend.position=("bottom"),
          legend.title = ggplot2::element_blank(),
          legend.background = ggplot2::element_rect (colour = "black"),
          legend.text=ggplot2::element_text(size=10)
          )
}


#' Function to return a default pdf graphics device
#'
#' @param file Output filename
#' @param width Defaults to 9
#' @param height Defaults to 7
#' @return A pdf graphics device
#' @export
oe_pdf <- function(file='output.pdf', width=9, height=7) {
  grDevices::pdf(file, width, height, onefile=TRUE, family=get_default_font(), colormodel='cmyk')
}

#' Function to embed fonts
#'
#' @export
oe_embed_fonts <- extrafont::embed_fonts
