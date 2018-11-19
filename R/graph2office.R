#' Save currently active R graph to Microsoft Office / LibreOffice format
#' 
#' Save the currently active R graph or a graph passed as an object or function 
#' to Microsoft Office / LibreOffice format with sensible defaults
#' 
#' @import datasets
#' @importFrom grDevices dev.size png
#' @importFrom utils head tail
#' @import officer 
#' @import rvg
#' @aliases graph2office graph2doc graph2ppt
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
#' @param vector.graphic logical specifying whether or not to output in
#' editable, vector \code{DrawingML} format. Defaults to \code{TRUE}, in which case editing
#' the plot in Powerpoint or Word is then possible after first ungrouping the
#' plot elements. If set to \code{FALSE}, the plot is rasterized to \code{PNG} bitmap
#' format at a resolution of 300 dpi.
#' @param \dots any other options are passed on to \code{rvg}'s \code{\link[rvg]{dml_pptx}} function.
#' @return \code{NULL}
#' @author Tom Wenseleers, Christophe Vanderaa
#' @example examples/graph2office.R
#' @seealso \code{\link{graph2vector}}, \code{\link{graph2svg}}, \code{\link{graph2pdf}}, \code{\link{graph2eps}},
#' \code{\link{graph2bitmap}}, \code{\link{graph2png}}, \code{\link{graph2tif}}, \code{\link{graph2jpg}}
#' @export
#' 
graph2office = function(x = NULL, file = "Rplot", fun = NULL, type = c("PPT","DOC"), 
                        append = FALSE,  aspectr = NULL, width = NULL, height = NULL, scaling=100, 
                        paper = "auto", orient = ifelse(type[1]=="PPT","landscape","auto"),
                        margins = c(top=0.5,right=0.5,bottom=0.5,left=0.5), 
                        center = TRUE, offx = 1, offy = 1, upscale = FALSE, 
                        vector.graphic = TRUE, 
                        ...) {
  
  ### 1. Check and format arguments
  margins=rep_len(margins,4)
  names(margins)=c("top","right","bottom","left")
  type = toupper(type)
  type = match.arg(type,c("PPT","DOC"))
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
  obj=x
  if (is.null(obj) & is.null(fun)) { 
    p = captureplot() 
  } else { 
    p = obj 
  }
  if (inherits(p,"list")) { stop("base R plots cannot be passed as objects, use ggplot2 or lattice plots instead") }
  myplot = if (is.null(fun)){
    function(pl = p) print(pl) 
  } else {
    fun
  } 
  
  ### 2. Prepare the plotting region and the plot apsect
  if(options()$device == FALSE){
    plotsize = dev.size()
  } else {
    plotsize = c(7,5) # default device size: 10 inch x 10 inch
  }
  
  w = plotsize[[1]]
  h = plotsize[[2]]
  plotaspectr = plotsize[[1]]/plotsize[[2]]
  if ((!is.null(aspectr))&is.null(height)&is.null(width)) { 
    plotaspectr = aspectr
    if (plotaspectr >= 1) { 
      h = w/plotaspectr 
    } else { 
      w = h*plotaspectr 
    } 
  }
  if ((is.null(height))&(!is.null(width))) { plotaspectr = aspectr; w = width; h = w / plotaspectr }
  if ((is.null(width))&(!is.null(height))) { plotaspectr = aspectr; h = height; w = h / plotaspectr } 
  # if width and height is given override other scaling params
  if ((!is.null(width))&(!is.null(height))) { w = width; h = height; plotaspectr = w/h }  
  w = w*scaling/100; h = h*scaling/100;
  
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
  if(type=="PPT"){
    if (center) { 
      offx = (pagesize["width"] + margins["left"]+margins["right"] - w)/2
      offy = (pagesize["height"] + margins["top"]+margins["bottom"] - h)/2
    }
    if(vector.graphic){
      doc = ph_with_vg_at(doc, code = myplot(), left = offx, top = offy, width = w, height = h, ...)
    } else {
      temp.file <- paste0(tempfile(), ".png")
      grDevices::png(filename = temp.file, height = h, width = w, units = "in", res = 300)
      myplot()
      dev.off()
      doc <- ph_with_img_at(doc, src = temp.file, left = offx, top = offy, width = w, height = h)
      unlink(temp.file)
    }
  } else {
    if(vector.graphic){
      doc <- suppressWarnings(body_add_vg(doc, code = myplot(), width = w, height = h, ...))
    } else {
      temp.file <- tempfile()
      grDevices::png(filename = temp.file, height = h, width = w, units = "in", res = 300)
      myplot()
      dev.off()
      doc <- suppressWarnings(body_add_img(doc, src = temp.file, width = w, height = h))
      unlink(temp.file)
    }
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


