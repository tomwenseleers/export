#' Save currently active R graph to Microsoft Office / LibreOffice format
#' 
#' Save the currently active R graph or a graph passed as an object or function 
#' to Microsoft Office / LibreOffice format with sensible defaults
#' 
#' @import datasets
#' @importFrom grDevices dev.size png
#' @importFrom devEMF emf
#' @importFrom utils head tail
#' @import officer 
#' @import rvg
#' 
#' @aliases graph2office graph2doc graph2ppt
#' 
#' @param x given \code{ggplot2} plot or \code{lattice} plot object to export; if
#' set to \code{NULL} the currently active R graph will be exported; not
#' supported for base R plots.
#' @param file name of output file. Any extension is ignored and added
#' according to the requested output type.
#' @param fun plot passed on as a function used to create it; useful especially
#' for base R plots.
#' @param type desired output type - \code{DOC} for Word document, \code{PPT} for Powerpoint.
#' @param append logical value - if \code{TRUE} and \code{type=PPT} it will append the graph
#' to the given file, where file can also be a given corporate template. If
#' \code{append=FALSE} any existing file will be overwritten. Currently ignored in
#' Word export.
#' @param aspectr desired width to height aspect ratio. If set to \code{NULL}, the
#' aspect ratio of the active graphics device is used.
#' @param width desired width in inches; can be combined with a desired
#' aspect ratio aspectr.
#' @param height desired height in inches; can be combined with a desired
#' aspect ratio aspectr.
#' @param scaling scale width & height by a certain percentage.
#' @param paper desired paper size to use - "A5" to "A1" for Powerpoint export,
#' or "A5" to "A3" for Word output; default "auto" automatically selects the
#' paper size that fits your graph. Graphs that are too large to fit on a given
#' paper size are scaled down.
#' @param orient desired paper orientation - "auto", "portrait" or "landscape";
#' default to "auto" for Word output and to "landscape" for Powerpoint.
#' @param margins vector with the desired margins that should be left blank in
#' @param center logical specifying whether or not to center the graph
#' in the exported Powerpoint.
#' @param offx if center is set to \code{FALSE}, the desired x offset at which to
#' place one's graph in Powerpoint output.
#' @param offy if center is set to \code{FALSE}, the desired y offset at which to
#' place one's graph in Powerpoint output.
#' @param upscale logical specifying whether or not to upscale one's graph to
#' make it page-filling (excluding the margins). Note that scaling may result
#' in a different look of one's graph relative to how it looks on the screen
#' due to the change in size.
#' @param font desired default Office font family to use for vectorized
#' PowerPoint output and rasterized PowerPoint output. If \code{NULL}, the font
#' is inferred from the plot when possible and otherwise defaults to
#' \code{"Arial"} on Windows systems and to \code{"Helvetica"} on other systems.
#' @param fonts named list of font aliases passed to \code{\link[rvg]{dml}} for
#' vectorized PowerPoint output. If \code{NULL}, \code{font} is used for the
#' \code{sans}, \code{serif}, \code{mono}, and \code{symbol} aliases. Use
#' \code{fonts = list()} to keep \code{rvg}'s automatic font aliases.
#' @param vector.graphic logical specifying whether or not to output in
#' vectorized format. This avoids pixelated images in the document. Note that 
#' for PowerPoint, the image can be edited after first ungrouping the plot 
#' elements. If set to \code{FALSE}, the plot is rasterized to \code{PNG} bitmap
#' format at a resolution of 300 dpi.
#' @param \dots any other options are passed on to \code{rvg}'s 
#' \code{\link[rvg]{dml_pptx}} function if \code{type == "PPT"} or to 
#' \code{devEMF}'s \code{\link[devEMF]{emf}} function if \code{type == "DOC"} (only 
#' when \code{vector.graphics == TRUE}).
#' 
#' @return No return value
#' 
#' @author Tom Wenseleers, Christophe Vanderaa
#' 
#' @example examples/graph2office.R
#' 
#' @seealso \code{\link{graph2vector}}, \code{\link{graph2svg}}, \code{\link{graph2pdf}}, \code{\link{graph2eps}},
#' \code{\link{graph2bitmap}}, \code{\link{graph2png}}, \code{\link{graph2tif}}, \code{\link{graph2jpg}}
#' 
#' @export
#' 
graph2office = function(x = NULL, file = "Rplot", fun = NULL, type = c("PPT","DOC"), 
                        append = FALSE,  aspectr = NULL, width = NULL, height = NULL, scaling=100, 
                        paper = "auto", orient = ifelse(type[1]=="PPT","landscape","auto"),
                        margins = c(top=0.5,right=0.5,bottom=0.5,left=0.5), 
                        center = TRUE, offx = 1, offy = 1, upscale = FALSE, 
                        font = NULL,
                        fonts = NULL,
                        vector.graphic = TRUE, 
                        ...) {
  
  ### 1. Check and format arguments
  margins=rep_len(margins,4)
  names(margins)=c("top","right","bottom","left")
  type = toupper(type)
  type = match.arg(type, c("PPT", "DOC"))
  if (type == "PPT" | type == "PPTX") {
    ext = ".pptx"
    type = "PPT"
  }
  if (type == "DOC" | type == "DOCX") {
    ext = ".docx"
    type = "DOC"
  }
  file = sub("^(.*)[.].*", "\\1", file)  # remove extension if given
  file = paste0(file, ext)  # add extension
  # Format the function call for plotting the graph
  p = resolve_plot(x = x, fun = fun)
  if (inherits(p,"list")) { stop("base R plots cannot be passed as objects, use ggplot2 or lattice plots instead") }
  myplot = if (is.null(fun)){
    function(pl = p) print(pl) 
  } else {
    fun
  } 
  
  ### 2. Prepare the plotting region and the plot apsect
  
  plotdim = plot_dimensions(width = width, height = height, aspectr = aspectr,
                            scaling = scaling)
  w = plotdim[["width"]]
  h = plotdim[["height"]]
  plotaspectr = plotdim[["aspectr"]]
  
  ### 3. Find the best template (docx or pptx) to contain the plot
  # if paper="auto" choose template with best fitting size
  if (append & file.exists(file)) { 
    templ=file 
  } else { 
    if(paper=="auto") {
      templ=besttemplate(w=w,h=h,margins=margins,orient=orient,type=type)
      #templ=paste0(paste( find.package("export"), "inst", "templates",templ, sep = "/" ),ext) 
      templ=paste0(paste( find.package("export"), "templates",templ, sep = "/" ),ext) 
    } else {
      templ=paste( find.package("export"), "templates",paste0(toupper(paper),"_",orient,ext), sep = "/" )
      if (!file.exists(templ)) stop(paste0("template ",templ," not available"))
    }
  }
  
  ### 4. Initialize the slide (ppt) or page (doc) that will contain the plot
  if (type == "PPT") {
    if (append & file.exists(file)){
      doc <- read_pptx(path = file)
    } else {
      doc <- read_pptx(path = templ)
    }
    doc = add_slide(doc, layout = "Blank", master = "Office Theme")
    pagesize = get.slide.size(doc) 
    pagesize["width"]=pagesize["width"]-(margins["left"]+margins["right"])
    pagesize["height"]=pagesize["height"]-(margins["top"]+margins["bottom"])
  }
  if (type == "DOC") {
    if (append & file.exists(file)){
      doc <- read_docx(path = file)
      doc <- body_add_break(doc, pos = "after")
    } else {
      doc <- read_docx(path = templ)
    }
    pagesize <- (doc$sect_dim$page - doc$sect_dim$margins[c(3,2)])/1440 # 1440 is a factor to convert to inches
  }
  
  ### 5. Scale the plot according to the new slide or page
  # should graph still be shrinked to fit or upscaled?
  if ((w>pagesize["width"])|(h>pagesize["height"])) shrink=TRUE else shrink=FALSE 
  if (shrink|upscale) {    
    pageaspectr = pagesize["width"]/pagesize["height"]
    if (pageaspectr >= plotaspectr) {
      xf = plotaspectr/pageaspectr
      yf = 1
    } else {
      xf = 1
      yf = pageaspectr/plotaspectr
    }
    w = pagesize["width"] * xf
    h = pagesize["height"] * yf
  }
  
  ### 6. Print the plot on the slide or page
  if (vector.graphic && showtext_auto_enabled()) {
      warning("showtext_auto() is enabled; exporting a raster image because showtext glyph rendering is not compatible with editable Office DrawingML")
      vector.graphic <- FALSE
  }
  office.fonts <- resolve_office_font(font = font, fonts = fonts, plot = p)
  font <- office.fonts$font
  fonts <- office.fonts$fonts
  if(type=="PPT"){
      if (center) {
          offx = (pagesize["width"] + margins["left"]+margins["right"] - w)/2
          offy = (pagesize["height"] + margins["top"]+margins["bottom"] - h)/2
      }
      if(vector.graphic){
          plot.dml <- if (is_plot_object(p)) {
              dml(ggobj = p, fonts = fonts, ...)
          } else {
              dml(code = myplot(), fonts = fonts, ...)
          }
          doc = ph_with(doc, plot.dml,
                        location = ph_location(left = offx, top = offy, width = w,
                                               height = h), ...)
      } else {
          temp.file <- paste0(tempfile(), ".png")
          grDevices::png(filename = temp.file, height = h, width = w, units = "in", res = 300, family = font)
          myplot()
          dev.off()
          doc <- ph_with(doc, external_img(src = temp.file), location = ph_location(left = offx, top = offy, width = w, height = h))
          unlink(temp.file)
      }
  } else {
      temp.file <- tempfile()
      if(vector.graphic){
          temp.file <- paste0(temp.file, ".emf")
          emf(file = temp.file, height = h, width = w, emfPlus = TRUE, ...)
          myplot()
      } else {
          temp.file <- paste0(temp.file, ".png")
          grDevices::png(filename = temp.file, height = h, width = w, units = "in", res = 300)
          myplot()
      }
      dev.off()
      doc <- body_add_img(doc, src = temp.file, width = w, height = h)
      unlink(temp.file)
  }

  ### 7. End of function, save the file and print message
  print(doc, target = file)
  message(paste0("Exported graph as ",file))
}


#' @describeIn graph2office
#' Save currently active R graph to a Microsoft Office PowerPoint/LibreOffice Impress presentation
#' @export
graph2ppt = function(...) graph2office(type = "PPT", ...)

#' @describeIn graph2office
#' Save currently active R graph to a Microsoft Office Word/LibreOffice Writer document
#' @export
graph2doc = function(...) graph2office(type = "DOC", ...)


