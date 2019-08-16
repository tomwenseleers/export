#' Export statistical output to a table in spreadsheet compatible format (.xlsx or .csv)
#' 
#' Export currently showing R stats object or stats object obj to a Microsoft Excel / 
#' LibreOffice Calc or comma-separated value file
#' 
#' @importFrom utils methods write.csv write.csv2
#' @import stats  
#' @import openxlsx
#' @importFrom xtable xtable
#' @aliases table2spreadsheet table2excel table2csv table2csv2
#' @param x given R stats object to export; if set to \code{NULL} the output of the 
#' previous R command will be exported.
#' @param file name of output file. The .xlsx or .csv extension is added automatically.
#' @param type desired output type - \code{"XLS"} for Excel and \code{"CSV"}/\code{"CSV2"} for CSV file. Note that 
#' \code{type="CSV2"} will generate a CSV file where the value separator is a semi-colon (";") and the decimal
#' separator is a comma (",")
#' @param append logical value - if \code{TRUE} and \code{type="XLS"} it will add a new woorksheet to the given 
#' file, where file can also be a given corporate.
#' \code{append=FALSE} any existing file will be overwritten. 
#' @param sheetName a string giving the name of the new sheet that is created (only for \code{type=="XLS"}). 
#' It must be unique (case insensitive) from any existing sheet name in the file. 
#' @param digits number of significant digits to show for all columns except
#' for the column with p values.
#' @param digitspvals number of significant digits to show for columns with p
#' values.
#' @param trim.pval a threshold below which the p-values are trimmed as 
#' "< \code{trim.pval}".
#' @param add.rownames logical specifying whether or not to add row names.
#' @param \dots extra options are passed on to \code{\link[openxlsx]{createStyle}} for the formatting of the woorksheet.
#' This is only applicable for \code{type=="XLS"}.
# #' @return \code{\link[flextable]{flextable}} object
#' @return A data frame 
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
#' @example examples/table2spreadsheet.R
#' @seealso \code{\link{table2tex}}, \code{\link{table2html}}, \code{\link{table2office}}
#' @export
#' 
table2spreadsheet = function(x = NULL, file = "Rtable", type = c("XLS","CSV","CSV2"), append = FALSE, sheetName="new sheet",
                        digits = 2, digitspvals = 2, trim.pval = 1E-16, add.rownames = FALSE, ...) {
 
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
  type=match.arg(type,c("XLS","CSV"))
  
  ext <- if(type=="XLS"){ 
    ".xlsx"
  } else if(type == "CSV" || type == "CSV2"){
    ".csv"
  } 
  file = sub("^(.*)[.].*", "\\1", file)  # remove extension if given
  file = paste0(file, ext)  # add extension
  
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
    tab <- xtable2(x=outp, ndigits = digits, ndigitspvals = digitspvals, trim.pval=trim.pval)
  } else if (length(intersect(class(outp), as.character(gsub("tidy.", "", methods(tidy))))) >= 1) {
    tab <- tidy2(x=outp, ndigits = digits, ndigitspvals = digitspvals, trim.pval=trim.pval)
  } else { # should not occur
    tab <- data.frame2(x=outp, ndigits = digits, ndigitspvals = digitspvals, trim.pval=trim.pval)
  }
  
  if(type=="XLS"){
    if (append & file.exists(file)){
      doc <- loadWorkbook(file=file)
    } else {
      doc <- createWorkbook()
    }
    addWorksheet(doc, sheetName = sheetName)
    writeData(doc, sheet = sheetName, x = tab, colNames = TRUE, rowNames = add.rownames, 
              headerStyle = createStyle(textDecoration="bold"), withFilter = FALSE)
    sheetStyle <- createStyle(...)
    addStyle(doc, sheet = sheetName, style = sheetStyle, 
             rows = rep(2:(nrow(tab)+1), ncol(tab)), 
             cols = rep((1+add.rownames):(ncol(tab)+add.rownames), each=nrow(tab)))
    saveWorkbook(doc, file, overwrite = TRUE) 
  } else if(type == "CSV"){
    write.csv(x=tab, file=file, quote=F, row.names = add.rownames)
  } else if(type == "CSV2"){
    write.csv2(x=tab, file=file, quote=F, row.names = add.rownames)
  }
  message(paste0("Exported table as ",file))
  return(as.data.frame(tab))
}

#' @describeIn table2spreadsheet
#' Export statistical output to a table in a Microsoft Office Excel/ LibreOffice Calc spreadsheet
#' @export
table2excel <- function(...) table2spreadsheet(..., type = "XLS")
  
#' @describeIn table2spreadsheet
#' Export statistical output to a table in a CSV format ("," for value separation and "." for decimal)
#' @export
table2csv <- function(...) table2spreadsheet(..., type = "CSV")
    
#' @describeIn table2spreadsheet
#' Export statistical output to a table in a CSV format (";" for value separation and "," for decimal)
#' @export
table2csv2 <- function(...) table2spreadsheet(..., type = "CSV2")
  