#' Save currently active rgl 3D graph to bitmap format
#' 
#' Save currently active rgl 3D graph to bitmap format in current orientation
#' 
#' 
#' @importFrom rgl rgl.cur
#' @importFrom rgl rgl.snapshot
#' @aliases rgl2bitmap rgl2png
#' @param file name of output file. Any extension is ignored and added
#' according to the requested output type. If file already exists it is overwritten.
#' @param type desired output type - currently only \code{PNG} is supported.
#' @return \code{NULL}
#' @author Tom Wenseleers
#' @example examples/rgl2bitmap.R
#' @seealso \code{\link{rgl2vector}},\code{\link{rgl2svg}}, \code{\link{rgl2pdf}},
#' \code{\link{rgl2eps}}, \code{\link{rgl2tex}} 
#' @export
#' 
rgl2bitmap = function(file = "Rplot", type = c("PNG")) {
  type = toupper(type)
  type = match.arg(type)
  ext = paste0(".", tolower(type))
  file = sub("^(.*)[.].*", "\\1", file)  # remove extension if given
  file = paste0(file, ext)  # add extension
  if (rgl.cur()==0) stop("No rgl device open to capture plot from")

  rgl.snapshot(file, fmt="png", top=TRUE)
  
  message(paste0("Exported rgl graph as ",file))
  
}

#' @describeIn rgl2bitmap
#' @export
rgl2png = function(...) rgl2bitmap(type = "PNG", ...)

