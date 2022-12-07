#' Save currently active R graph to vector format
#' 
#' Save the currently active R graph or a graph passed as an object or function 
#' to vector format with sensible defaults
#' 
#' 
#' @importFrom grDevices dev.size
#' @importFrom grDevices svg
#' @importFrom grDevices pdf
#' @importFrom grDevices postscript
#' @importFrom grDevices dev.off
#' @importFrom grDevices cairo_pdf
#' @importFrom grDevices cairo_ps
#' @aliases graph2vector graph2svg graph2pdf graph2eps
#' @param x given \code{ggplot2} plot or \code{lattice} plot object to export; if
#' set to \code{NULL} the currently active R graph will be exported; not
#' supported for base R plots.
#' @param file name of output file. Any extension is ignored and added
#' according to the requested output type. If file already exists it is overwritten.
#' @param fun plot passed on as a function used to create it; useful especially
#' for base R plots.
#' @param type desired output type - \code{SVG}, \code{PDF} or \code{EPS} are currently supported.
#' \code{SVG} is the preferred format, and good for editing in Inkscape; \code{PDF} is good
#' for printing; \code{EPS} is sometimes requested by journals, though lower quality,
#' especially when semi-transparency is used, as this is rasterized to bitmap. 
#' \code{\link{graph2office}} is recommended for vector output to Microsoft Office.
#' @param aspectr desired width to height aspect ratio. If set to \code{NULL}, the
#' aspect ratio of the graphics device is used. Can also be combined with one
#' value for either the desired width or height of the graph.
#' @param width desired width in inches; can be combined with a desired
#' aspect ratio aspectr.
#' @param height desired height in inches; can be combined with a desired
#' aspect ratio aspectr.
#' @param scaling scale width & height by a certain percentage.
#' @param font desired font to use for labels; defaults to \code{"Arial"} on Windows
#' systems and to \code{"Helvetica"} on other systems. Fonts are embedded by default in \code{EPS} output.
#' @param bg desired background colour, e.g. \code{"white"} or \code{"transparent"}.
#' @param cairo logical indicating whether or not to use the \code{cairo} graphics
#' device for output to \code{PDF} or \code{EPS}, defaults to \code{TRUE}, thereby allowing for
#' simulated semi-transparency in \code{EPS} output, by rasterizing semi-transparent
#' sections, and automated font embedding.
#' @param fallback_resolution resolution in dpi to use to rasterize non-supported
#' vector graphics (e.g. semi-transparent vector elements in \code{EPS}) output).
#' @param colormodel desired colormodel in \code{pdf} or \code{eps} output when \code{cairo=FALSE};
#' currently allowed values are \code{"rgb"} (default), \code{"cmyk"}, \code{"srgb"}, \code{"srgb+gray"}, \code{"rgb-nogray"}, 
#' and \code{"gray"} (or \code{"grey"}). 
#' @param \dots any other options are passed on to \code{\link{svg}}, \code{\link{cairo_pdf}}, \code{\link{cairo_ps}}, \code{\link{pdf}} or
#' postscript.
#' @return No return value
#' @author Tom Wenseleers
#' @example examples/graph2vector.R
#' @seealso \code{\link{graph2office}}, \code{\link{graph2bitmap}}, \code{\link{graph2png}}, \code{\link{graph2tif}}, \code{\link{graph2jpg}} 
#' @export
#' 
graph2vector = function(x = NULL, file = "Rplot", fun = NULL, type = "SVG", 
                        aspectr = NULL, width = NULL, height = NULL, scaling = 100, 
                        font = ifelse(Sys.info()["sysname"]=="Windows","Arial",
                                      "Helvetica")[[1]], bg = "white", colormodel="rgb", 
                        cairo = TRUE, fallback_resolution = 600, ...) {
  type = toupper(type)
  type = match.arg(type,c("SVG","PDF","EPS"))
  ext = paste0(".", tolower(type))
  file = sub("^(.*)[.].*", "\\1", file)  # remove extension if given
  file = paste0(file, ext)  # add extension
  obj=x
  if (is.null(obj) & is.null(fun)) p = captureplot() else p = obj
  if (inherits(p,"list")) 
    stop("base R plots cannot be passed as objects, use ggplot2 or lattice plots instead")
  myplot = if (is.null(fun)) function(pl = p) print(pl) else fun
  
  if(!identical(options()$device, FALSE)){
    plotsize = dev.size()
  } else {
    plotsize = c(7,5) # default device size: 10 inch x 10 inch
  }
  
  w = plotsize[[1]]
  h = plotsize[[2]]
  plotaspectr = plotsize[[1]]/plotsize[[2]]
  if ((!is.null(aspectr))&is.null(height)&is.null(width)) { plotaspectr = aspectr
  if (plotaspectr >= 1) { 
    h = w/plotaspectr } else { w = h*plotaspectr } 
  }
  if ((is.null(height))&(!is.null(width))) { w = width; h = w / plotaspectr }
  if ((is.null(width))&(!is.null(height))) { h = height; w = h / plotaspectr } 
  # if width and height is given override other scaling params
  if ((!is.null(width))&(!is.null(height))) { w = width; h = height }  
  w = w*scaling/100; h = h*scaling/100;
  
  if (type == "SVG") {
    svg(filename = file, 
        height = h, 
        width = w,
        family = font,
        onefile = FALSE,
        bg = bg,
        ... )
    myplot()
    dev.off()
  }
  
  if (type == "PDF") {
    #cairo_surface_set_fallback_resolution() # check cairoSurfaceSetFallbackResolution in library(RGtk2)
    if (!cairo) { 
      pdf(file = file,  # also check cairo_pdf
          height = h, 
          width = w,
          family = font,
          onefile = FALSE,
          bg = bg,
          colormodel = colormodel, 
          useDingbats = FALSE,
          ... ) 
    } else { 
      if ("fallback_resolution" %in% names(formals(fun=cairo_ps))) {
        cairo_pdf(filename = file,  
                  height = h, 
                  width = w,
                  family = font, 
                  onefile = FALSE,
                  bg = bg,
                  fallback_resolution = fallback_resolution,
                  ... ) 
      } else {
        cairo_pdf(filename = file,  # also check cairo_pdf
                  height = h, 
                  width = w,
                  family = font, 
                  onefile = FALSE,
                  bg = bg,
                  ... )
      }
    }
    myplot()
    dev.off()
  }
  
  if (type == "EPS") { 
    if (!cairo) { postscript(file = file, 
                             height = h, 
                             width = w,
                             family = font,
                             onefile = FALSE,
                             bg = bg,
                             colormodel = colormodel, 
                             ... ) 
    } else { 
      if ("fallback_resolution" %in% names(formals(fun=cairo_ps)))  {
        cairo_ps(filename = file, 
                 height = h, 
                 width = w,
                 family = font,
                 onefile = FALSE,
                 bg = bg,
                 fallback_resolution = fallback_resolution,
                 ... )
      } else {
        cairo_ps(filename = file, 
                 height = h, 
                 width = w,
                 family = font,
                 onefile = FALSE,
                 bg = bg,
                 ... )
      }
    }
    myplot()
    dev.off()
  }
  
  message(paste0("Exported graph as ",file))
  
}



#' @describeIn graph2vector
#' Save currently active R graph to SVG format
#' @export
graph2svg = function(...) graph2vector(type = "SVG", ...)

#' @describeIn graph2vector
#' Save currently active R graph to PDF format
#' @export
graph2pdf = function(...) graph2vector(type = "PDF", ...)

#' @describeIn graph2vector
#' Save currently active R graph to EPS format
#' @export
graph2eps = function(...) graph2vector(type = "EPS", ...)

