#' Function to get default font family
#'
#' @return A font family name
get_default_font <- function() {
  extrafont::choose_font(c("Calibri", "FlandersArtSans","Helvetica"))
}

#' ggplot2theme
#' @param base_size Base fontsize for theme (default = 12)
#' @param base_family Default family used for plot. Defaults to
#' FlandersArtSans, with a fallback to Helvetica.
#' @param geom_point_size Base size for geom_point.
#' @param geom_line_size Base size for geom_line.
#' @return A ggplot2 theme
#' @export
theme_erfgoed <- function(base_size = 12, base_family = get_default_font(), geom_point_size = 1, geom_line_size=1) {
  ggplot2::update_geom_defaults("line", list(colour="#944EA1", size= geom_line_size))
  ggplot2::update_geom_defaults("bar", list(colour="black", fill = "#944EA1", alpha = 1))
  ggplot2::update_geom_defaults("point", list(shape = 1, size = geom_point_size))

  t_bw <- ggplot2::theme_bw(base_size = base_size, base_family = base_family)
  t_e  <- ggplot2::theme(
            panel.grid.major.y = ggplot2::element_line(size= 0.6, colour = "#d9d9d9"),
            panel.grid.major.x = ggplot2::element_line(size= 0.6, colour = "#d9d9d9"),
            panel.grid.minor.y = ggplot2::element_line(size= 0.3, colour = "#b3b3b3", linetype = "dotted"),
            panel.grid.minor.x = ggplot2::element_line(size= 0.3, colour = "#b3b3b3", linetype = "dotted"),

            plot.title = ggplot2::element_text(size = base_size + 3, face= "bold", margin = ggplot2::margin(20,20,20,20)),

            axis.text = ggplot2::element_text (size= base_size - 2),
            axis.title.y = ggplot2::element_text(size= base_size - 2, face = "bold",angle = 90, margin = ggplot2::margin(1,15,1,1)),
            axis.title.x = ggplot2::element_text(size= base_size - 2, face = "bold", margin = ggplot2::margin(15,1,1,1)),

            legend.position=("bottom"),
            legend.title = ggplot2::element_blank(),
            legend.background = ggplot2::element_rect (colour = "black"),
            legend.text = ggplot2::element_text(size= base_size - 4)
          )
  return(ggplot2::`%+replace%`(t_bw, t_e))
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

#' Function to return a default png graphics device
#'
#' @param file Output filename
#' @param width Defaults to 6.3
#' @param height Defaults to 5.9
#' @param units defaults to inch
#' @param res defaults to 600
#' @return A png graphics device
#' @export
oe_png<- function (file= 'output.png',width = 6.3, height = 5.9, units= "in", res = 600) {
  grDevices::png(file= file, width = width, height = height, units = units, res= res)
}

#' Function to embed fonts (wrapper around extrafont::embed_fonts)
#' @param file	Name of input file.
#' @param format	File format. (see ?embedFonts)
#' @param outfile Name of the output file (with fonts embedded). (Default is same as input file)
#' @param options	Other arguments passed to embedFonts.
#' @export
oe_embed_fonts <- extrafont::embed_fonts



#' wrapper to set defaults for scales::format_format
#'
#' @param x Vector to be formatted.
#' returns a vector formatted with comma as a decimal seperator
#' @export
bcomma <- function(x) format(x, big.mark = ".", decimal.mark ="," , scientific = FALSE)
