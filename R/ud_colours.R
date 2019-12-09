ud_colours <- function(colours = c("grey75", "grey65", "red", "blue", "green"), colors, gcpal=colorblind_pal()(8)) {
  #Allow either colours or colors
  if(!missing(colors)) {
    if(!missing(colours))
      stop("Please specify colours or colors but not both.")
    else
      colours <- colors
  }
  list(colours=colours, gcpal=gcpal)
}
