#' Export R graph to Microsoft Office / LibreOffice format
#' 
#' Export the currently showing R graph, a graphics object or a graph passed as
#' a function to Microsoft Office / LibreOffice format
#' 
#' 
#' @aliases graph2office graph2doc graph2ppt graph2bsdoc graph2html
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
#' @example examples/graph2office.R
#' @export graph2office
graph2office = function(file = "Rplot", obj = NULL, fun = NULL, type = "PPT", 
                        append = FALSE, scaling = 90, aspectr = NULL, width = NULL, 
                        height = NULL, vector.graphic = TRUE, font = "Arial", ...) {
    type = toupper(type)
    type = match.arg(type,c("PPT","PPTX","DOC","DOCX","HTML"))
    if (type == "PPT" | type == "PPTX") {
        ext = ".pptx"
        type = "PPT"
    }
    if (type == "DOC" | type == "DOCX") {
        ext = ".docx"
        type = "DOC"
    }
    if (type == "HTML") {
        ext = ".html"
        type = "HTML"
    }
    file = sub("^(.*)[.].*", "\\1", file)  # remove extension if given
    file = paste0(file, ext)  # add extension
    if (is.null(obj) & is.null(fun)) 
        p = captureplot() else p = obj
    if (inherits(p,"list") 
        stop("base R plots cannot be passed as objects, use ggplot2 or lattice plots instead")
    plotsize = dev.size()  # also works if no graphics device is open
    plotaspectr = plotsize[[1]]/plotsize[[2]]
    if (!is.null(aspectr)) 
        plotaspectr = aspectr
    if ((!is.null(aspectr))&(!is.null(width))) { height = width / aspectr; aspectr = NULL }
    if ((!is.null(aspectr))&(!is.null(height))) { width = height / aspectr; aspectr = NULL }  
    
    myplot = if (is.null(fun)) 
        function(pl = p) print(pl) else fun
    
    pagesize = c(width = 6, height = 6)
    if (type == "PPT") {
        if (append & file.exists(file)) 
            doc = pptx(template = file) else doc = pptx()
        doc = addSlide(doc, slide.layout = "Blank")
        pagesize = dim(doc)$slide.dim
    }
    if (type == "DOC") {
        # if (append&file.exists(file)) doc=docx(template=file) else doc=docx() 
        # as append currently didn't work in this way (office reports
        # errors in the document, and opens it only after fixing these errors) I've now disabled this option
        doc = docx()
        pagesize = dim(doc)$page - dim(doc)$margins[c(4, 3)]
    }
    pageaspectr = pagesize["width"]/pagesize["height"]
    if (pageaspectr >= plotaspectr) {
        xf = plotaspectr/pageaspectr
        yf = 1
    } else {
        xf = 1
        yf = pageaspectr/plotaspectr
    }
    w = (scaling/100) * pagesize["width"] * xf
    h = (scaling/100) * pagesize["height"] * yf
    
    if (type == "HTML") 
        {
            doc = bsdoc(title = "")
            w = (scaling/100) * 10
            h = (scaling/100) * 10/plotaspectr
        }  # for type='BSDOC' argument append and aspectr is currently ignored
           # for type='BSDOC' scaling=100 will result in width of 10 inches
           # scaling can be set > or < 100 to expand or shrink
    
    if (!is.null(width)) 
        w = width  # if width and height is given override other scaling params
    if (!is.null(height)) 
        h = height
    if (type == "PPT") {
        doc = addPlot(doc, fun = myplot, vector.graphic = vector.graphic, fontname = font, offx = (pagesize["width"] - 
            w)/2, offy = (pagesize["height"] - h)/2, width = w, height = h, ...)
    }
    if (type == "DOC") {
        doc = addPlot(doc, fun = myplot, vector.graphic = vector.graphic, fontname = font, width = w, height = h, 
            ...)
        # if (pagebreak) doc = addPageBreak( doc )
    }
    if (type == "HTML") {
        doc = addPlot(doc, fun = myplot, vector.graphic = vector.graphic, fontname = font, width = w, height = h, 
            ...)
    }
    writeDoc(doc, file)
}

#' @param \dots other options are passed on to graph2office.
#' @export graph2office
graph2ppt = function(...) graph2office(type = "PPT", ...)

#' @param \dots other options are passed on to graph2office.
#' @export graph2office
graph2doc = function(...) graph2office(type = "DOC", ...)

#' @param \dots other options are passed on to graph2office.
#' @export graph2office
graph2html = function(...) graph2office(type = "HTML", ...)
