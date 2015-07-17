#' Save currently active R graph to bitmap format
#' 
#' Save the currently active R graph or a graph passed as an object or function 
#' to bitmap format with sensible defaults
#' 
#' 
#' @import grDevices
#' @aliases graph2bitmap graph2png graph2tif graph2jpg
#' @param obj given \code{ggplot2} plot or \code{lattice} plot object to export; if
#' set to \code{NULL} the currently active R graph will be exported; not
#' supported for base R plots.
#' @param file name of output file. Any extension is ignored and added
#' according to the requested output type. If file already exists it is overwritten.
#' @param fun plot passed on as a function used to create it; useful especially
#' for base R plots.
#' @param type desired output type - \code{PNG}, \code{TIF} or \code{JPG} are currently supported.
#' \code{PNG} is the preferred format, as it is a lossless format, and compresses better
#' than \code{TIF}.
#' @param aspectr desired width to height aspect ratio. If set to \code{NULL}, the
#' aspect ratio of the graphics device is used. Can also be combined with one
#' value for either the desired width or height of the graph.
#' @param width desired width in inches; can be combined with a desired
#' aspect ratio aspectr.
#' @param height desired height in inches; can be combined with a desired
#' aspect ratio aspectr.
#' @param dpi desired output in dpi; defaults to 600 dpi.
#' @param scaling scale width & height by a certain percentage.
#' @param font desired font to use for labels; defaults to \code{"Arial"} on Windows
#' systems and to \code{"Helvetica"} on other systems.
#' @param bg desired background colour, e.g. \code{"white"} or \code{"transparent"}.
#' @param pngtype use \code{Cairographics} for export or Windows GDI?
#' @param tifftype use \code{Cairographics} for export or Windows GDI?
#' @param tiffcompression compression to use for \code{TIF} files.
#' @param jpegquality quality of \code{JPEG} compression.
#' @param \dots any other options are passed on to \code{ReporteRs}'s \code{\link[ReporteRs]{addPlot}} function.
#' @return \code{NULL}
#' @author Tom Wenseleers
#' @example examples/graph2bitmap.R
#' @seealso \code{\link{graph2office}}, \code{\link{graph2vector}}, \code{\link{graph2svg}}, \code{\link{graph2pdf}},
#' \code{\link{graph2eps}}, \code{\link{graph2tex}} 
#' @export
#' 
graph2bitmap = function(obj = NULL, file = "Rplot", fun = NULL, type = c("PNG","JPG","TIF"), 
                        aspectr = NULL, width = NULL, height = NULL, dpi = 600, 
                        scaling = 100, font = ifelse(Sys.info()["sysname"]=="Windows",
                        "Arial","Helvetica")[[1]], bg = "white", pngtype = 
                        c("cairo-png","windows","cairo"), tifftype = c("cairo","windows"), 
                        tiffcompression = c("lzw","rle","jpeg","zip","lzw+p","zip+p"), jpegquality = 99, ...) {
  type = toupper(type)
  type = match.arg(type)
  if (type=="JPG") type="JPEG"
  if (type=="TIFF") type="TIF"
  pngtype = match.arg(pngtype)
  tifftype = match.arg(tifftype)
  tiffcompression = match.arg(tiffcompression)
  ext = paste0(".", tolower(type))
  file = sub("^(.*)[.].*", "\\1", file)  # remove extension if given
  file = paste0(file, ext)  # add extension
  if (is.null(obj) & is.null(fun)) p = captureplot() else p = obj
  if (inherits(p,"list")) 
    stop("base R plots cannot be passed as objects, use ggplot2 or lattice plots instead")
  myplot = if (is.null(fun)) function(pl = p) print(pl) else fun
  
  plotsize = dev.size()  # also works if no graphics device is open
  w = plotsize[[1]]
  h = plotsize[[2]]
  plotaspectr = plotsize[[1]]/plotsize[[2]]
  if ((!is.null(aspectr))&is.null(height)&is.null(width)) { plotaspectr = aspectr
    if (plotaspectr >= 1) { h = w/plotaspectr } else { w = h*plotaspectr } 
  }
  if ((is.null(height))&(!is.null(width))) { w = width; h = w / plotaspectr }
  if ((is.null(width))&(!is.null(height))) { h = height; w = h / plotaspectr } 
  # if width and height is given override other scaling params
  if ((!is.null(width))&(!is.null(height))) { w = width; h = height }  
  w = w*scaling/100; h = h*scaling/100;
 
  if (type == "PNG") {
      png( filename = file, 
        type = pngtype,
        units = "in", 
        width = w, 
        height = h, 
        family = font,
        res = dpi,
        bg = bg, ...)
    myplot()
    invisible(dev.off())
  }
  
  if (type == "TIF") {
      tiff( filename = file, 
         type = tifftype,
         compression = tiffcompression,
         units = "in", 
         width = w, 
         height = h, 
         family = font, 
         res = dpi,
         bg = bg, ... )
    myplot()
    invisible(dev.off())  
  }
  
  if (type == "JPEG") { 
      jpeg( filename = file, 
          quality = jpegquality,
          units = "in", 
          width = w, 
          height = h, 
          family = font,
          res = dpi,
          bg = bg, ... )
    myplot()
    invisible(dev.off())
  }  
  
  message(paste0("Exported graph as ",file))
  
}

#' @describeIn graph2bitmap
#' @export
graph2png = function(...) graph2bitmap(type = "PNG", ...)

#' @describeIn graph2bitmap
#' @export
graph2tif = function(...) graph2bitmap(type = "TIF", ...)

#' @describeIn graph2bitmap
#' @export
graph2jpg = function(...) graph2bitmap(type = "JPG", ...) 
