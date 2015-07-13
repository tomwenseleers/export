#' Save the currently displayed R graph or a graph passed as an object or function with sensible defaults
#' 
#' Export the currently showing R graph, a graphics object or a graph passed as
#' a function to bitmap format
#' 
#' 

graph2bitmap = function(file = "Rplot", obj = NULL, fun = NULL, type = "PNG", 
                        aspectr = NULL, width = NULL, height = NULL, dpi = 600, 
                        scaling = 100, font = "sans", bg = "white", 
                        pngtype = "cairo-png", tifftype = "cairo", 
                        tiffcompression = "lzw", jpegquality = 99, ...) {
  
#' @aliases graph2bitmap graph2png graph2tiff graph2tif graph2jpeg graph2jpg
#' @param file name of output file. Any extension is ignored and added
#' according to the requested output type.
#' @param obj given ggplot2 plot or lattice plot object to export to Office; if
#' set to NULL the currently shown R stats object will be exported; not
#' supported for base R plots.
#' @param fun plot passed on as a function used to create it; useful especially
#' for base R plots.
#' @param type desired output type - DOC for Word document, PPT for Powerpoint
#' or BSDOC or HTML for HTML.
#' @param append logical value - if TRUE and type=PPT it will append the graph
#' to the given file, where file can also be a given corporate template. If
#' append=FALSE any existing file will be overwritten. Only used in graph2ppt,
#' for graph2doc, graph2bsdoc and graph2html append is always set to FALSE.
#' @param scaling scaling in percentage of the total page or slide width or
#' height (excluding page margins in the case of graph2doc). Defaults to 90
#' percent.
#' @param aspectr desired width to height aspect ratio. If set to NULL, the
#' aspect ratio of the graphics device is used.
#' @param width desired width in inches; if specified, aspectr and scaling are
#' ignored.
#' @param height desired height in inches; if specified, aspectr and scaling
#' are ignored.
#' @param vector.graphic logical specifying whether or not to output in
#' editable, vector DrawingML format. Defaults to TRUE, in which case editing
#' the plot in Powerpoint or Word is then possible after first ungrouping the
#' plot elements. If set to FALSE, the plot is first rasterized to PNG bitmap
#' format.
#' @param font desired font to use for labels.
#' @param pointsize desired point size. Only affects label size in case of base
#' R plots.
#' @param \dots any other options are passed on to ReporteR's addPlot function.
#' @return NULL
#' @note %% ~~further notes~~
#' @author Tom Wenseleers
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @examples
#' 
#' \donttest{
#' 
#' ## export of ggplot2 plot
#' library(ggplot2)
#' qplot(Sepal.Length, Petal.Length, data = iris, color = Species, 
#'       size = Petal.Width, alpha = I(0.7))
#' graph2ppt(file="ggplot2 plot.pptx", aspectr=1.7)
#' 
#' # add 2nd slide with same graph in different aspect ratio
#' graph2ppt(file="ggplot2 plot.pptx", aspectr=1.3, append=T) 
#' # add 3d slide with same graph with fixed width & height
#' graph2ppt(file="ggplot2 plot.pptx", width=6, height=5, append=T) 
#' 
#' graph2doc(file="ggplot2 plot.docx", aspectr=1.7) 
#' 
#' graph2html(file="ggplot2 plot.html", aspectr=1.7) 
#' # pass plot as a ggplot2 object
#' p=qplot(Sepal.Length, Petal.Length, data = iris, color = Species, 
#'       size = Petal.Width, alpha = I(0.7))
#' graph2ppt(obj=p,file="ggplot2 plot.pptx", aspectr=1.7) # works OK
#' 
#' 
#' ## export of lattice plot
#' library(lattice)
#' library(effects)
#' fit=lm(prestige ~ type + income*education, data=Prestige)
#' plot(Effect(c("income", "education"), fit, partial.residuals=TRUE),multiline=T, 
#'      span=1, show.fitted=TRUE, ci.style="bands")
#' graph2ppt(file="effect plot.pptx")
#' # pass as object
#' p=plot(Effect(c("income", "education"), fit, partial.residuals=TRUE),multiline=T, 
#'      span=1, show.fitted=TRUE, ci.style="bands")
#' graph2ppt(obj=p,file="boxplot.pptx") # works OK
#' 
#' 
#' ## example export of base R plot
#' boxplot(mpg~cyl,data=mtcars, main="Car Milage Data",xlab="Number of Cylinders", 
#' ylab="Miles Per Gallon",col="cyan2")
#' graph2ppt(file="boxplot.pptx")
#' # passing it as an object does not work
#' p=boxplot(mpg~cyl,data=mtcars, main="Car Milage Data",xlab="Number of Cylinders", 
#' ylab="Miles Per Gallon",col="cyan2")
#' graph2ppt(obj=p,file="boxplot.pptx") # this does not work
#' # passing it as a function also works
#' f=function() boxplot(mpg~cyl,data=mtcars, main="Car Milage Data",col="cyan2")
#' graph2ppt(fun=f, file="boxplot.pptx", aspectr=1.3) # this does work
#' 
#' 
#' 
#' ## heatmap example
#' heatmap(as.matrix(eurodist))
#' graph2ppt(file="heatmap.pptx")
#' }
#' @export graph2bitmap
#' @export graph2png
#' @export graph2tiff
#' @export graph2tif
#' @export graph2jpeg
#' @export graph2jpg
graph2bitmap = function(file = "Rplot", obj = NULL, fun = NULL, type = "PNG", 
                        aspectr = NULL, width = NULL, height = NULL, dpi = 600, 
                        scaling = 100, font = "sans", bg = "white", 
                        pngtype = "cairo-png", tifftype = "cairo", 
                        tiffcompression = "lzw", jpegquality = 99, ...) {
  type = toupper(type)
  type = match.arg(type,c("PNG","JPEG","JPEG","TIFF","TIF"))
  if (type=="JPG") type="JPEG"
  if (type=="TIFF") type="TIF"
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

graph2png = function(...) graph2bitmap(type = "PNG", ...)
graph2tif = function(...) graph2bitmap(type = "TIF", ...)
graph2tiff = function(...) graph2bitmap(type = "TIF", ...)
graph2jpeg = function(...) graph2bitmap(type = "JPEG", ...)  
graph2jpg = function(...) graph2bitmap(type = "JPEG", ...) 
