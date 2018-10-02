#' Export statistical output to a table in Microsoft Office / LibreOffice format
#' 
#' Export currently showing R stats object or stats object obj to a Microsoft
#' Office / LibreOffice table
#' 
#' @importFrom utils methods 
#' @import stats  
#' @importFrom xtable xtable
#' @importFrom broom tidy
#' @import officer
#' @import flextable
#' @aliases table2office table2doc table2ppt
#' @param x given R stats object to export; if set to \code{NULL} the output of the 
#' previous R command will be exported. 
#' @param file name of output file. The .pptx or .docx extension is added automatically.
#' @param type desired output type - \code{"PPT"} for PowerPoint and \code{"DOC"} for Word.
#' @param append logical value - if \code{TRUE} and \code{type="PPT"} or \code{"DOC"} it will
#' append the table to the given file, where file can also be a given corporate.  If 
#' \code{append=FALSE} any existing file will be overwritten. 
#' @param digits number of significant digits to show for all columns except
#' for the column with p values.
#' @param digitspvals number of significant digits to show for columns with p
#' values.
#' @param width desired width of table in inches. If the given width exceeds the page or slide 
#' width, the table width becomes the page/slide width.
#' @param height desired height of table in inches. If the given height exceeds the page or slide 
#' height, the table height becomes the page/slide height.
#' @param offx x offset in inches to specify horizontal location of table (only for \code{type=="PPT"}).
#' @param offy y offset in inches to specify vertical location of table (only for \code{type=="PPT"}).
#' @param font desired font to use for output table; defaults to \code{"Arial"} on Windows
#' systems and to \code{"Helvetica"} on other systems.
#' @param pointsize desired font point size.
#' @param add.rownames logical specifying whether or not to add row names.
#' @param \dots extra options are passed on to \code{\link[xtable]{xtable}} or \code{\link[broom]{tidy}}
#' @return \code{\link[flextable]{flextable}} object
#' @details Columns corresponding to degrees of freedom (with header "Df" or "df")
#' are always given as integers. Objects that can be exported with \code{\link{table2office}} are 
#' all those supported by \code{\link[xtable]{xtable}} and \code{\link[broom]{tidy}}. The function will
#' first use \code{\link[xtable]{xtable}} to format the data. If the data class is not supported by 
#' \code{\link[xtable]{xtable}} the function will then use \code{\link[broom]{tidy}}. 
#' The data classes suported by \code{\link[xtable]{xtable}} are: 
#' \itemize{
#'    \item \code{anova} 
#'    \item \code{aov} 
#'    \item \code{aovlist} 
#'    \item \code{data.frame} 
#'    \item \code{glm} 
#'    \item \code{gmsar} 
#'    \item \code{lagImpact} 
#'    \item \code{lm} 
#'    \item \code{matrix} 
#'    \item \code{prcomp} 
#'    \item \code{sarlm} 
#'    \item \code{sarlm.pred} 
#'    \item \code{spautolm} 
#'    \item \code{sphet} 
#'    \item \code{splm} 
#'    \item \code{stsls} 
#'    \item \code{summary.aov} 
#'    \item \code{summary.aovlist} 
#'    \item \code{summary.glm} 
#'    \item \code{summary.gmsar} 
#'    \item \code{summary.lm} 
#'    \item \code{summary.prcomp} 
#'    \item \code{summary.sarlm} 
#'    \item \code{summary.spautolm} 
#'    \item \code{summary.sphet} 
#'    \item \code{summary.splm} 
#'    \item \code{summary.stsls} 
#'    \item \code{table} 
#'    \item \code{ts} 
#'    \item \code{zoo}
#'    } 
#' The data classes suported by \code{\link[broom]{tidy}} are: 
#' \itemize{
#'    \item \code{aareg} 
#'    \item \code{acf} 
#'    \item \code{Arima} 
#'    \item \code{betareg} 
#'    \item \code{biglm} 
#'    \item \code{binDesign} 
#'    \item \code{binWidth} 
#'    \item \code{brmsfit} 
#'    \item \code{btergm} 
#'    \item \code{cch} 
#'    \item \code{character} 
#'    \item \code{cld} 
#'    \item \code{coeftest} 
#'    \item \code{confint.glht} 
#'    \item \code{cv.glmnet} 
#'    \item \code{default} 
#'    \item \code{density} 
#'    \item \code{dgCMatrix} 
#'    \item \code{dgTMatrix} 
#'    \item \code{dist} 
#'    \item \code{emmGrid} 
#'    \item \code{ergm} 
#'    \item \code{felm} 
#'    \item \code{fitdistr} 
#'    \item \code{ftable} 
#'    \item \code{gam} 
#'    \item \code{Gam} 
#'    \item \code{gamlss} 
#'    \item \code{geeglm} 
#'    \item \code{glht} 
#'    \item \code{glmnet} 
#'    \item \code{glmRob} 
#'    \item \code{gmm} 
#'    \item \code{htest} 
#'    \item \code{ivreg} 
#'    \item \code{kappa} 
#'    \item \code{kde} 
#'    \item \code{kmeans} 
#'    \item \code{Line} 
#'    \item \code{Lines} 
#'    \item \code{list} 
#'    \item \code{lme} 
#'    \item \code{lmodel2} 
#'    \item \code{lmRob} 
#'    \item \code{logical} 
#'    \item \code{lsmobj} 
#'    \item \code{manova} 
#'    \item \code{map} 
#'    \item \code{Mclust} 
#'    \item \code{merMod} 
#'    \item \code{mle2} 
#'    \item \code{muhaz} 
#'    \item \code{multinom} 
#'    \item \code{nlrq} 
#'    \item \code{nls} 
#'    \item \code{NULL} 
#'    \item \code{numeric} 
#'    \item \code{orcutt} 
#'    \item \code{pairwise.htest} 
#'    \item \code{plm} 
#'    \item \code{poLCA} 
#'    \item \code{Polygon} 
#'    \item \code{Polygons} 
#'    \item \code{power.htest} 
#'    \item \code{pyears} 
#'    \item \code{rcorr} 
#'    \item \code{ref.grid}
#'    \item \code{ridgelm} 
#'    \item \code{rjags} 
#'    \item \code{roc} 
#'    \item \code{rowwise_df} 
#'    \item \code{rq} 
#'    \item \code{rqs} 
#'    \item \code{sparseMatrix} 
#'    \item \code{SpatialLinesDataFrame} 
#'    \item \code{SpatialPolygons} 
#'    \item \code{SpatialPolygonsDataFrame} 
#'    \item \code{spec} 
#'    \item \code{speedlm} 
#'    \item \code{stanfit} 
#'    \item \code{stanreg} 
#'    \item \code{summary.glht} 
#'    \item \code{summaryDefault} 
#'    \item \code{survdiff} 
#'    \item \code{survexp} 
#'    \item \code{survfit} 
#'    \item \code{survreg} 
#'    \item \code{tbl_df} 
#'    \item \code{TukeyHSD}
#'    }
#' @author Tom Wenseleers, Christophe Vanderaa
#' @example examples/table2office.R
#' @seealso \code{\link{table2tex}}, \code{\link{table2html}}, \code{\link{table2spreadsheet}}
#' @export
#' 
table2office = function(x = NULL, file = "Rtable", type = c("PPT","DOC"), append = FALSE, digits = 2, 
                     digitspvals = 2, width = NULL, height = NULL, offx = 1, offy = 1, 
                     font = ifelse(Sys.info()["sysname"]=="Windows","Arial","Helvetica")[[1]], pointsize = 12, 
                     add.rownames = FALSE, ...) {
  
  obj=x
  if (is.null(obj)) {
    outp = .Last.value # capture previously shown output or use passed object
  } else {
    outp = obj
  }
  if (is.null(outp)) stop("no R stats object available to export")
  supobjects = unique(c(as.character(gsub("xtable.", "", methods(xtable))), 
                        as.character(gsub("tidy.", "", methods(tidy))),
                        "xtabs"))
  if (length(intersect(class(outp), supobjects)) == 0) stop(paste0(class(outp), " is currently not supported by table2office"))
  
  
  type=toupper(type)
  type=match.arg(type,c("PPT","DOC"))
  
  ext <- if(type=="PPT"){ 
    ".pptx"
  } else if(type == "DOC"){
    ".docx"
  } 
  file = sub("^(.*)[.].*", "\\1", file)  # remove extension if given
  file = paste0(file, ext)  # add extension
  
  if(type == "PPT"){
    if (append & file.exists(file)) { 
      doc = read_pptx(path = file) 
    } else { 
      doc = read_pptx() 
    }
    doc <- add_slide(doc, layout = "Blank", master = "Office Theme")
    pagesize <- get.slide.size(doc)
  } else if(type == "DOC"){
    if (append & file.exists(file)) { 
      doc = read_docx(path = file) 
      doc = body_add_break(doc, pos = "after")
    } else { 
      doc = read_docx() 
    }
    pagesize <- (doc$sect_dim$page - doc$sect_dim$margins[c(3,2)])/1440 # 1440 is a factor to convert to inches
  } 
  
  
  # deal with specific classes of objects 
  if (inherits(outp, "summary.merMod")) {
    outp <- data.frame(coef(summary(outp)), check.names = F)
  } else if(inherits(outp, "Matrix")) {
    outp <- as.data.frame(as.matrix(x))
  } else if (inherits(outp, c("xtabs", "ftable"))) {
    outp <- ftable(outp)
  } 
  
  # Depending on the data class, call xtable or tidy
  if (length(intersect(class(outp), as.character(gsub("xtable.", "", methods(xtable))))) >= 1) {
    tab <- xtable2(x=outp, ndigits = digits, ndigitspvals = digitspvals,...)
  } else if (length(intersect(class(outp), as.character(gsub("tidy.", "", methods(tidy))))) >= 1) {
    tab <- tidy2(x=outp, ndigits = digits, ndigitspvals = digitspvals,...)
  } else { # should not occur
    tab <- data.frame2(x=outp, ndigits = digits, ndigitspvals = digitspvals)
  }
  
  nc <- ncol(tab)
  nr <- nrow(tab)
  tblaspectr = nc / nr * 2  # guess table aspect ratio
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
  # if width and height is given override other scaling params
  if (!is.null(width)) w = width  
  if (!is.null(height)) h = height
  
  
  # Avoid bug in flextable: when one of the colnames = x, flextable returns an empty table
  x.col <- which(colnames(tab) == "x")
  if(length(x.col)>0) colnames(tab)[x.col]<- "x "
  
  # Issues
  # - Deal with ftable 
  # - use margins ?
  cell.height <- min(h, pagesize["height"] - offy)/(nr+1)
  cell.width <- min(w, pagesize["width"] - offx)/(nc+1)
  if(inherits(tab,"xtable")){
    tab <- xtable_to_flextable(tab, include.rownames = add.rownames, rowname_col = ".")
    tab <- width(tab, width=cell.width)
    tab <- height(tab, height=cell.height)
  } else {
    if(add.rownames) x <- cbind(" " = rownames(x), x)
    tab <- flextable(tab, cheight = cell.height, cwidth = cell.width)
  }
  tab <- bold(tab, part = "header") # bold header
  tab <- fontsize(tab, part = "all", size = pointsize) 
  tab <- font(tab, part = "all", fontname = font)
  
  
  if(type=="PPT"){
    doc <- ph_with_flextable_at(doc, value = tab , left=offx, top=offy)
  } else if(type == "DOC"){
    doc <- body_add_flextable(doc, value = tab)
  } 
  
  print(doc, target = file)
  message(paste0("Exported table as ",file))
  return(tab)
}

#' @describeIn table2office
#' Export statistical output to a table in a Microsoft Office PowerPoint/ LibreOffice Impress presentation
#' @export
table2ppt = function(...) table2office(type = "PPT", ...)

#' @describeIn table2office
#' Export statistical output to a table in a Microsoft Office Word/ LibreOffice Writer document
#' @export
table2doc = function(...) table2office(type = "DOC", ...)



