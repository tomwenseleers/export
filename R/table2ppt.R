#' Export statistical output to a table in Microsoft Powerpoint / LibreOffice format
#' 
#' Export currently showing R stats object or stats object obj to a Microsoft
#' Powerpoint / LibreOffice table
#' 
#' @aliases table2ppt
#' @param obj given R stats object to export; if set to NULL the output of the 
#' previous R command will be exported.
#' @param file name of output file. The .pptx extension is added automatically.
#' @param append logical value - if TRUE and type="PPT"" or "DOC"" it will
#' append the graph to the given file, where file can also be a given corporate
#' template in case type="PPT".  If append=FALSE any existing file will be
#' overwritten. 
#' @param digits number of significant digits to show for all columns except
#' for the column with p values.
#' @param digitspvals number of significant digits to show for columns with p
#' values.
#' @param width desired width of table in inches.
#' @param height desired height of table in inches.
#' @param offx x offset in inches to specify horizontal location of table.
#' @param offy y offset in inches to specify vertical location of table.
#' @param font desired font to use for output table.
#' @param pointsize desired font point size.
#' @param add.rownames logical specifying whether or not to add row names.
#' @param \dots extra options are passed on to xtable.
#' @return FlexTable object
#' @note Columns corresponding to degrees of freedom (with header "Df" or "df")
#' are always given as integers.
#' @author Tom Wenseleers
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @example examples/table2ppt.R
#' @rdname table2ppt
#' @export
#' 
table2ppt = function(obj = NULL, file = "Rtable", append = FALSE, digits = 2, 
                     digitspvals = 2, width = NULL, height = NULL, offx = 1, offy = 1, 
                     font = "Calibri", pointsize = 12, 
                     add.rownames = FALSE, ...) {
  #zebra = FALSE, odd = "#D2DEEF", even = "#EAEFF7", header = "#5B9BD5",
  #headertext = ifelse(zebra,"white","black"),
  
  if (is.null(obj)) 
        outp = .Last.value else outp = obj  # capture previously shown output or use passed object
    if (is.null(outp)) 
        stop("no R stats object available to export")
    supobjects = c(as.character(gsub("xtable.", "", methods(xtable))), "xtabs", "ftable")  
    # suppported objects
    # 'anova' 'aov' 'aovlist' 'coxph' 'data.frame' 'glm' 'lm' 
    # 'matrix' 'prcomp' 'summary.aov' 'summary.aovlist' 'summary.glm'
    # 'summary.lm' 'summary.prcomp' 'table' 'ts' 'zoo'
    if (length(intersect(class(outp), supobjects)) == 0) 
        stop(paste0(class(outp), "is currently not supported by table2office"))
    
    # alternative way to get last executed command 
    # tempfile = tempfile(pattern='rhistory_', fileext='.txt') 
    # savehistory(tempfile)
    # myhistory = readLines(tempfile) 
    # h = tail(h, 5) # last 5 commands eval(parse(text=h[length(h)-1])) 
    # exec last command but 1 see
    # also http://stackoverflow.com/questions/20959561/accessing-r-history-from-r-code
    
    # helper function to show p values right aligned with digitspvals sign digits and 
    # degrees of freedom columns as right aligned integers
    xtable2 = function(x, ndigits = digits, ndigitspvals = digitspvals) {
        sm = xtable(x)
        ncol = ncol(sm)
        digs = rep(ndigits, ncol + 1)
        disp = rep("f", ncol + 1)
        whch = grep("\\QPr(\\E|\\Qp-value\\E|\\Qp value\\E|\\Qpadj\\E|^p$|^padj$", colnames(sm))
        if (length(whch) != 0) 
            digs[whch + 1] = ndigitspvals
        if (length(whch) != 0) 
            disp[whch + 1] = "g"
        whch = grep("^Df$|^df$", colnames(sm))
        if (length(whch) != 0) 
            digs[whch + 1] = 0
        if (length(whch) != 0) 
            disp[whch + 1] = "d"
        xtable(x, digits = digs, display = disp, ...)
    }
    
    # deal with specific classes of objects not supported by xtable
    if ("summary.merMod" %in% class(outp)) 
        outp = data.frame(coef(summary(outp)), check.names = F)
    if ("xtabs" %in% class(outp)) 
        outp = ftable(outp)
    supobjects = as.character(gsub("xtable.", "", methods(xtable)))
    if ("ftable" %in% class(outp)) 
        tab = ftable(outp) else {
        if (length(intersect(class(outp), supobjects)) >= 1) 
            tab = xtable2(outp, ...) else outp = data.frame(outp, check.names = F)
    }
    
    # do some formatting of the FlexTable
    # see https://davidgohel.github.io/ReporteRs/formatting_properties.html
    flextab = as.FlexTable(x = tab, add.rownames = add.rownames)
    #if (zebra) flextab=setZebraStyle( flextab, odd = odd, even = even)
    flextab[] = ReporteRs::textProperties(font.size=pointsize, font.family=font) 
    # flextab[1,, to="header"] = textProperties(color=headertext, 
    # font.size=pointsize, font.weight = "bold", font.family=font)
    flextab[1,, to="header"] = ReporteRs::textProperties(font.size=pointsize, 
                                              font.weight = "bold", font.family=font)
    # flextab[1,, to="header"] = parProperties(shading.color=header) 
    # change to setFlexTableBackgroundColors
    
    ext = ".pptx"
    file = sub("^(.*)[.].*", "\\1", file)  # remove extension if given
    file = paste0(file, ext)  # add extension
    
    if (append & file.exists(file)) { doc = ReporteRs::pptx(template = file) } else { doc = ReporteRs::pptx() }
    doc = ReporteRs::addSlide(doc, slide.layout = "Blank")
    pagesize = dim(doc)$slide.dim
    
    tblaspectr = flextab$numcol * 0.7/flextab$numrow  # guess table aspect ratio
    pageaspectr = pagesize["width"]/pagesize["height"]
    if (pageaspectr > tblaspectr) {
        xf = tblaspectr/pageaspectr
        yf = 1
    } else {
        xf = 1
        yf = pageaspectr/tblaspectr
    }
    w = pagesize["width"] * xf
    h = pagesize["height"] * yf
    
    if (!is.null(width)) 
        w = width  # if width and height is given override other scaling params
    if (!is.null(height)) 
        h = height
      
    doc = ReporteRs::addFlexTable(doc, flextab, offx = offx, offy = offy, width = w, height = h)
    
  ReporteRs::writeDoc(doc, file)
    flextab
}
