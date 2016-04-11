#' colorscheme
#' @return named vector of colours
#' @export
erfgoed_kleuren <- function () {

  colours <- c("#944EA1",
               "#006DDB",
               "#32CD32",
               "#FFFF6D",
               "#920000",
               "#B66DFF",
               "#6DB6FF",
               "#924900",
               "#B6DBFF",
               "#D26E25",
               "#009292",
               "#FF6DB6",
               "#004949",
               "#FFB677")


  colour_names <- c("OE.hoofd",
                    "OE.blauw",
                    "OE.lichtgroen",
                    "OE.geel",
                    "OE.rood",
                    "OE.lichtpaars",
                    "OE.lichtblauw",
                    "OE.bruin",
                    "OE.blauwgrijs",
                    "OE.oranje",
                    "OE.grijsgroen",
                    "OE.roze",
                    "OE.donkergrijs",
                    "OE.lichtroze")

  names(colours) <- colour_names

  return(colours)
}
