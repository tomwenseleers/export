# Currently marked out because rgl::rgl.postscript() is too buggy
# #' Save currently active rgl 3D graph to vector or Latex format
# #' 
# #' Save currently active rgl 3D graph to vector or Latex format in current orientation
# #' 
# #' 
# #' @importFrom rgl rgl.cur
# #' @importFrom rgl rgl.postscript
# #' @aliases rgl2vector rgl2svg rgl2pdf rgl2eps rgl2tex
# #' @param file name of output file. Any extension is ignored and added
# #' according to the requested output type. If file already exists it is overwritten.
# #' @param type desired output type - currently \code{SVG}, \code{PDF}, \code{EPS}
# #' and \code{TEX} (Latex) are supported. Transparency is supported only in SVG
# #' (Inkscape) and PDF format.
# #' @return \code{NULL}
# #' @author Tom Wenseleers
# #' @example examples/rgl2vector.R
# #' @seealso \code{\link{rgl2bitmap}},\code{\link{rgl2png}} 
# #' @export
# #' 
# rgl2vector = function(file = "Rplot", type = "SVG") {
#   type = toupper(type)
#   type = match.arg(type,c("SVG","PDF","EPS","TEX"))
#   ext = paste0(".", tolower(type))
#   file = sub("^(.*)[.].*", "\\1", file)  # remove extension if given
#   file = paste0(file, ext)  # add extension
#   if (rgl.cur()==0) stop("No rgl device open to capture plot from")
#   
#   rgl.postscript(file, fmt=tolower(type), drawText=FALSE)
#   
#   message(paste0("Exported rgl graph as ",file))
#   
# }
# 
# #' @describeIn rgl2vector
# #' @export
# rgl2svg = function(...) rgl2vector(type = "SVG", ...)
# 
# #' @describeIn rgl2vector
# #' @export
# rgl2pdf = function(...) rgl2vector(type = "PDF", ...)
# 
# #' @describeIn rgl2vector
# #' @export
# rgl2eps = function(...) rgl2vector(type = "EPS", ...)
# 
# #' @describeIn rgl2vector
# #' @export
# rgl2tex = function(...) rgl2vector(type = "TEX", ...)
