#' Save currently active rgl 3D graph to bitmap format
#' 
#' Save currently active rgl 3D graph to bitmap format in current orientation
#' 
#' 
#' @aliases rgl2bitmap rgl2png
#' @param file name of output file. Any extension is ignored and added
#' according to the requested output type. If file already exists it is overwritten.
#' @param type desired output type - currently only \code{PNG} is supported.
#' @param \dots passing the \code{rgl2png} arguments to \code{rgl2bitmap}
#' @return No return value
#' @author Tom Wenseleers
#' @example examples/rgl2bitmap.R
#' @export
#' 
rgl2bitmap = function(file = "Rplot", type = c("PNG")) {
  if ( ! requireNamespace("rgl", quietly = TRUE)) {
    stop("Package 'rgl' is required for this function. Please install it.")
  }
  type = toupper(type)
  type = match.arg(type)
  ext = paste0(".", tolower(type))
  file = sub("^(.*)[.].*", "\\1", file)  # remove extension if given
  file = paste0(file, ext)  # add extension
  if (rgl::rgl.cur()==0) stop("No rgl device open to capture plot from")

  rgl::rgl.snapshot(file, fmt="png", top=TRUE)

  message(paste0("Exported rgl graph as ",file))
  
}

#' @describeIn rgl2bitmap
#' Save currently active rgl 3D graph to PNG format
#' @export
rgl2png = function(...) rgl2bitmap(type = "PNG", ...)

