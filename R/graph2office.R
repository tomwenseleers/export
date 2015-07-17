#' Save currently active R graph to Microsoft Office / LibreOffice format
#' 
#' Save the currently active R graph or a graph passed as an object or function 
#' to Microsoft Office / LibreOffice format with sensible defaults
#' 
#' @import ReporteRs
#' @import ReporteRsjars
#' @aliases graph2office graph2doc graph2ppt
#' @param obj given \code{ggplot2} plot or \code{lattice} plot object to export; if
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
#' @param font desired font to use; defaults to \code{"Arial"} on Windows
#' systems and to \code{"Helvetica"} on other systems.
#' @param \dots any other options are passed on to \code{ReporteR}'s \code{\link[ReporteRs]{addPlot}} function.
#' @return \code{NULL}
#' @author Tom Wenseleers
#' @example examples/graph2office.R
#' @seealso \code{\link{graph2tex}}, \code{\link{graph2vector}}, \code{\link{graph2svg}}, \code{\link{graph2pdf}}, \code{\link{graph2eps}},
#' \code{\link{graph2bitmap}}, \code{\link{graph2png}}, \code{\link{graph2tif}}, \code{\link{graph2jpg}}
#' @export
#' 
graph2office = function(obj = NULL, file = "Rplot", fun = NULL, type = c("PPT","DOC"), 
                        append = FALSE,  aspectr = NULL, width = NULL, height = NULL, scaling=100, 
                        paper = "auto", orient = ifelse(type[1]=="PPT","landscape","auto"),
                        margins = c(top=0.5,right=0.5,bottom=0.5,left=0.5), 
                        center = TRUE, offx = 1, offy = 1, upscale = FALSE, 
                        vector.graphic = TRUE, font = 
                        ifelse(Sys.info()["sysname"]=="Windows","Arial",
                        "Helvetica")[[1]], ...) {
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
    if (is.null(obj) & is.null(fun)) { p = captureplot() } else { p = obj }
    if (inherits(p,"list")) { stop("base R plots cannot be passed as objects, use ggplot2 or lattice plots instead") }

    myplot = if (is.null(fun)) 
      function(pl = p) print(pl) else fun
    #myplot()
    
    plotsize = dev.size()  # also works if no graphics device is open
    w = plotsize[[1]]
    h = plotsize[[2]]
    plotaspectr = plotsize[[1]]/plotsize[[2]]
    if ((!is.null(aspectr))&is.null(height)&is.null(width)) { plotaspectr = aspectr
                                                              if (plotaspectr >= 1) { h = w/plotaspectr } else { 
                                                                w = h*plotaspectr } 
    }
    if ((is.null(height))&(!is.null(width))) { plotaspectr = aspectr; w = width; h = w / plotaspectr }
    if ((is.null(width))&(!is.null(height))) { plotaspectr = aspectr; h = height; w = h / plotaspectr } 
    # if width and height is given override other scaling params
    if ((!is.null(width))&(!is.null(height))) { w = width; h = height; plotaspectr = w/h }  
    w = w*scaling/100; h = h*scaling/100;
    
    # if paper="auto" choose template with best fitting size
    if (append & file.exists(file)) { templ=file } else { if (paper=="auto") {
      templ=besttemplate(w=w,h=h,margins=margins,orient=orient,type=type)
      templ=paste0(paste( find.package("export"), "templates",templ, sep = "/" ),ext) } else {
      templ=paste( find.package("export"), "templates",paste0(toupper(paper),"_",orient,ext), sep = "/" )
      if (!file.exists(templ)) stop(paste0("template ",templ," not available"))
      }}
            
    pagesize = c(width = 6, height = 6)
    if (type == "PPT") {
        if (append & file.exists(file)) doc = pptx(template = file) else doc = pptx(template = templ)
        doc = addSlide(doc, slide.layout = "Blank")
        pagesize = dim(doc)$slide.dim
        pagesize["width"]=pagesize["width"]-(margins["left"]+margins["right"])
        pagesize["height"]=pagesize["height"]-(margins["top"]+margins["bottom"])
    }
    if (type == "DOC") {
#       if (append & file.exists(file)) 
#         doc = docx(template = file) else doc = docx(template = templ)
#       due to bug in ReporteRs append is current disabled
        doc = docx(template = templ)
        pagesize = dim(doc)$page - dim(doc)$margins[c(4, 3)]
    }
    
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
    
#     if (type == "HTML") 
#         {
#             doc = bsdoc(title = "")
#             # w = (scaling/100) * 10
#             # h = (scaling/100) * 10/plotaspectr
#         }  # for type='BSDOC' argument append and aspectr is currently ignored
#            # for type='BSDOC' scaling=100 will result in width of 10 inches
#            # scaling can be set > or < 100 to expand or shrink
    
    if (type == "PPT") {
        if (center) { offx = (pagesize["width"] + margins["left"]+margins["right"] - w)/2
                      offy = (pagesize["height"] + margins["top"]+margins["bottom"] - h)/2
        }
        doc = addPlot(doc, fun = myplot, vector.graphic = vector.graphic, fontname = font, 
                      offx = offx, offy = offy, width = w, height = h, ...)
    }
    if (type == "DOC") {
#     due to bug in ReporteRs append is current disabled for .DOC    
#     if (append & file.exists(file)) { 
#         if (pagebreak) doc = addPageBreak( doc )
#         doc = addPlot(doc, fun = myplot, vector.graphic = vector.graphic, 
#                       fontname = font, width = w, height = h, ...)} else {
#         doc = addPlot(doc, fun = myplot, vector.graphic = vector.graphic, 
#                       fontname = font, width = w, height = h, bookmark="PLOT", ...)
#                       }        
      doc = addPlot(doc, fun = myplot, vector.graphic = vector.graphic, 
                    fontname = font, width = w, height = h, bookmark="PLOT", ...)  
    }
#     if (type == "HTML") {
#         doc = addPlot(doc, fun = myplot, vector.graphic = vector.graphic, fontname = font, width = w, height = h, 
#             ...)
#     }
writeDoc(doc, file)
    message(paste0("Exported graph as ",file))
}

#' @describeIn graph2office
#' @export
graph2ppt = function(...) graph2office(type = "PPT", ...)

#' @describeIn graph2office
#' @export
graph2doc = function(...) graph2office(type = "DOC", ...)



# for a given graph width, height, page orientation and type
# returns the best suited template
# TO DO: maybe still add additional parameter "pageaspectr" to support A4, 16:9 or 4:3
besttemplate = function(w,h,margins = c(top=1,right=1,bottom=1,left=1),orient="auto",type="PPT") {
# size of A5 to A1 landscape PPT/DOC templates
landscA=list(c(8.27,5.83),c(11.7,8.27),c(16.5,11.7),c(23.4,16.5),c(33.1,23.4))
if (type=="PPT" ) { landscA=
       lapply(landscA,function(x) x-c(margins["left"]+margins["right"],margins["top"]+margins["bottom"])) }
if (type=="DOC") landscA=lapply(landscA,function(x) x-1)[1:3] # allow for 1 inch margins+DOC only supports A5, A4 and A3
sizes=(5:1)[1:length(landscA)]
portrA=lapply(landscA,rev) # size of A5 to A1 landscape PPT/DOC templates
#w=8.9;h=6.7;
if (orient=="auto") orient=ifelse(w>=h,"landscape","portrait")
bestpagesize=suppressWarnings(ifelse( orient=="landscape", sizes[max( min(which(w<=unlist(lapply(landscA,head,n=1)))),
                                                     min(which(h<=unlist(lapply(landscA,tail,n=1)))) )],
                                      sizes[max( min(which(w<=unlist(lapply(portrA,head,n=1)))),
                                                     min(which(h<=unlist(lapply(portrA,tail,n=1)))) )] ))
if (is.na(bestpagesize)) bestpagesize=min(sizes)
return(paste0("A",bestpagesize,"_",orient)) }

# > besttemplate(90,40,"landscape")
# [1] "A1_landscape"
# 
# > besttemplate(8,5,"landscape")
# [1] "A5_landscape"
