#' Save currently active R graph to vector format
#' 
#' Save the currently active R graph or a graph passed as an object or function 
#' to vector format with sensible defaults
#' 
#' 
#' @aliases graph2vector graph2svg graph2pdf graph2eps
#' @param file name of output file. Any extension is ignored and added
#' according to the requested output type. If file already exists it is overwritten.
#' @param obj given ggplot2 plot or lattice plot object to export; if
#' set to NULL the currently active R graph will be exported; not
#' supported for base R plots.
#' @param fun plot passed on as a function used to create it; useful especially
#' for base R plots.
#' @param type desired output type - SVG, PDF or EPS are currently supported.
#' SVG is the preferred format, and good for editing in Inkscape; PDF is good
#' for printing; EPS is sometimes requested by journals, though lower quality,
#' especially when semi-transparency is used, as this is rasterized to bitmap.
#' @param aspectr desired width to height aspect ratio. If set to NULL, the
#' aspect ratio of the graphics device is used. Can also be combined with one
#' value for either the desired width or height of the graph.
#' @param width desired width in inches; can be combined with a desired
#' aspect ratio aspectr.
#' @param height desired height in inches; can be combined with a desired
#' aspect ratio aspectr.
#' @param scaling scale width & height by a certain percentage.
#' @param font desired font to use for labels; defaults to "sans", which corresponds
#' to "ArialMT" on Windows systems. Fonts are embedded by default in EPS output.
#' @param bg desired background colour, e.g. "white" or "transparent".
#' @param cairo logical indicating whether or not to use the cairo graphics
#' device for output to PDF or EPS, defaults to TRUE, thereby allowing for
#' simulated semi-transparency in EPS output, by rasterizing semi-transparent
#' sections, and automated font embedding.
#' @param colormodel desired colormodel in pdf or eps output when cairo=FALSE;
#' currently allowed values are "rgb" (default), "cmyk", "srgb", "srgb+gray", "rgb-nogray", 
#' and "gray" (or "grey"). 
#' @param \dots any other options are passed on to svg, cairo_pdf, cairo_ps, pdf or
#' postscript.
#' @return NULL
#' @note %% ~~further notes~~
#' @author Tom Wenseleers
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @example examples/graph2vector.R
#' @export
#' 
graph2vector = function(file = "Rplot", obj = NULL, fun = NULL, type = "SVG", 
                        aspectr = NULL, width = NULL, height = NULL, scaling = 100, 
                        font = "sans", bg = "white", 
                        colormodel="rgb", cairo = TRUE, ...) {
  type = toupper(type)
  type = match.arg(type,c("SVG","PDF","EPS"))
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
    if (!cairo) { pdf(file = file,  # also check cairo_pdf
        height = h, 
        width = w,
        family = font,
        onefile = FALSE,
        bg = bg,
        colormodel = colormodel, 
        useDingbats = FALSE,
        ... ) } else { 
          cairo_pdf(filename = file,  # also check cairo_pdf
              height = h, 
              width = w,
              family = font, 
              onefile = FALSE,
              bg = bg,
              ... ) 
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
               ... ) } else { 
                 cairo_ps(filename = file, 
                           height = h, 
                           width = w,
                           family = font,
                           onefile = FALSE,
                           bg = bg,
                           ... )
               }
    myplot()
    dev.off()
  }
  
  message(paste0("Exported graph as ",file))
  
}



#' @describeIn graph2vector
#' @export
graph2svg = function(...) graph2vector(type = "SVG", ...)

#' @describeIn graph2vector
#' @export
graph2pdf = function(...) graph2vector(type = "PDF", ...)

#' @describeIn graph2vector
#' @export
graph2eps = function(...) graph2vector(type = "EPS", ...)

