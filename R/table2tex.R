#' Export statistical output to HTML or Latex table
#' 
#' Export currently showing R stats object or stats object obj to a
#' HTML or Latex table
#' @import stats
#' @importFrom utils capture.output
#' @import stargazer
#' @aliases table2tex table2html 
#' @param x given R stats object or list of stats objects to export; if set to \code{NULL} 
#' the output of the previous R command will be exported.
#' @param file name of output file. The appropriate extension is added automatically.
#' @param type desired output type - \code{"TEX"} for Latex and \code{"HTML"} for HTML.
#' @param digits number of significant digits to show. A value of NA indicates that
#' no rounding should be done.
#' @param summary logical indicating whether or not to summarize data files.
#' @param standAlone logical indicating whether exported Latex code should be
#' standalone compilable, or whether it will be pasted into another document.
#' @param add.rownames logical indicating whether the names of the rows should be added
#' to the table (inserting a column before first column).
#' @param \dots extra options are passed on to stargazer.
#' @return \code{NULL}
#' @details Objects that can be exported are all those supported by \code{\link[stargazer]{stargazer}}:
#' \itemize{
#' 
#' \item \code{aftreg} (\code{eha})
#' \item \code{anova} (\code{stats})
#' \item \code{aov} (\code{stats})
#' \item \code{aovlist} (\code{stats})
#' \item \code{arima} (\code{stats}) 
#' \item \code{betareg} (\code{betareg})
#' \item \code{binaryChoice} (\code{sampleSelection})
#' \item \code{bj} (\code{rms})
#' \item \code{brglm} (\code{brglm}) 
#' \item \code{censReg} (\code{censReg})
#' \item \code{coeftest} (\code{lmtest})
#' \item \code{coxph} (\code{survival})
#' \item \code{coxreg} (\code{eha}) 
#' \item \code{clm} (\code{ordinal})
#' \item \code{clogit} (\code{survival})
#' \item \code{cph} (\code{rms})
#' \item \code{dynlm} (\code{dynlm})
#' \item \code{ergm} (\code{ergm})
#' \item \code{errorsarlm} (\code{spdev})
#' \item \code{felm} (\code{lfe})
#' \item \code{gam} (\code{mgcv})
#' \item \code{garchFit} (\code{fGarch})
#' \item \code{gee} (\code{gee})
#' \item \code{glm} (\code{stats})
#' \item \code{Glm} (\code{rms})
#' \item \code{glmer} (\code{lme4})
#' \item \code{glmrob} (\code{robustbase})
#' \item \code{gls} (\code{nlme})
#' \item \code{Gls} (\code{rms})
#' \item \code{gmm} (\code{gmm})
#' \item \code{heckit} (\code{sampleSelection})
#' \item \code{hetglm} (\code{glmx})
#' \item \code{hurdle} (\code{pscl})
#' \item \code{ivreg} (\code{AER})
#' \item \code{lagarlm} (\code{spdep})
#' \item \code{lm} (\code{stats})
#' \item \code{lme} (\code{nlme})
#' \item \code{lmer} (\code{lme4})
#' \item \code{lmrob} (\code{robustbase})
#' \item \code{lrm} (\code{rms})
#' \item \code{maBina} (\code{erer})
#' \item \code{mclogit} (\code{mclogit})
#' \item \code{mlogit} (\code{mlogit})
#' \item \code{mnlogit} (\code{mnlogit})
#' \item \code{mlreg} (\code{eha})
#' \item \code{multinom} (\code{nnet})
#' \item \code{nlme} (\code{nlme})
#' \item \code{nlmer} (\code{lme4})
#' \item \code{ols} (\code{rms})
#' \item \code{pgmm} (\code{plm})
#' \item \code{phreg} (\code{eha})
#' \item \code{plm} (\code{plm})
#' \item \code{pmg} (\code{plm})
#' \item \code{polr} (\code{MASS})
#' \item \code{psm} (\code{rms})
#' \item \code{rem.dyad} (\code{relevent})
#' \item \code{rlm} (\code{MASS})
#' \item \code{rq} (\code{quantreg})
#' \item \code{Rq} (\code{rms})
#' \item \code{selection} (\code{sampleSelection})
#' \item \code{svyglm} (\code{survey})
#' \item \code{survreg} (\code{survival})
#' \item \code{tobit} (\code{AER})
#' \item \code{weibreg} (\code{eha})
#' \item \code{zeroin} (\code{pscl})
#' \item \code{relogit} (\code{zelig})
#' \item \code{cloglog.net} (\code{zelig})
#' \item \code{gamma.net} (\code{zelig})
#' \item \code{probit.net} (\code{zelig}) 
#' \item \code{logit.net} (\code{zelig})
#' }
#' @author Tom Wenseleers, Christophe Vanderaa
#' @example examples/table2tex.R
#' @seealso \code{\link{table2office}} ,\code{\link{table2ppt}}, \code{\link{table2doc}}
#' @export
#' 

table2tex = function(x = NULL, file = "Rtable", type="TEX", digits = 2, summary=FALSE, standAlone=TRUE,
                     add.rownames = FALSE,...) {
  obj=x
  if (is.null(obj)) 
    outp = .Last.value else outp = obj  # capture previously shown output or use passed object
  if (is.null(outp)) 
    stop("no R stats object available to export")
  obj=outp

  type=toupper(type)
  type=match.arg(type,c("TEX","HTML"))
  
  if (type=="DOC") { ext=".doc" }
  if (type=="HTML") { ext=".html" }
  if (type=="TEX") { ext=".tex" }
  file = sub("^(.*)[.].*", "\\1", file)  # remove extension if given
  file = paste0(file, ext)  # add extension
  
  # define some additional methods for some objects
  # not supported by default by stargazer
  as.data.frame.summary.aov = function(x) {
    if(length(x) == 1) {
      as.data.frame(x[[1]])
    } else {
      lapply(unlist(x, FALSE), as.data.frame)
    }
  }
  
  if ("aov" %in% class(obj)) {obj=as.data.frame(anova(obj));summary=FALSE}
  if ("summary.aov" %in% class(obj)) {obj=as.data.frame(obj);summary=FALSE}
  
  # CHECK support for xtabs, ftable and CrossTable - contacted original author on this
  # if ("xtabs" %in% class(outp)) outp = ftable(outp)
  # if ("ftable" %in% class(outp)) XXX
  
  # object supported by stargazer?
  supported=!grepl("Unrecognized object type",paste(capture.output(stargazer(obj)),collapse=""))
  if (!supported) stop(paste0("Object of class ",class(obj)," is not supported by stargazer."))
  
  # objects supported by stargazer
  # aftreg (eha), anova (stats), aov (stats), aovlist (stats), arima (stats), 
  # betareg (betareg), binaryChoice (sampleSelection), bj (rms), brglm (brglm), 
  # censReg (censReg), coeftest (lmtest), coxph (survival), coxreg (eha), 
  # clm (ordinal), clogit (survival), cph (rms), dynlm (dynlm), ergm (ergm), 
  # errorsarlm (spdev), felm (lfe), gam (mgcv), garchFit (fGarch), gee (gee), 
  # glm (stats), Glm (rms), glmer (lme4), glmrob(robustbase), gls (nlme), 
  # Gls (rms), gmm (gmm), heckit (sampleSelection), hetglm (glmx), hurdle (pscl), 
  # ivreg (AER), lagarlm (spdep), lm(stats), lme (nlme), lmer (lme4), lmrob (robustbase), 
  # lrm (rms), maBina (erer), mclogit (mclogit), mlogit (mlogit), mnlogit (mnlogit), 
  # mlreg (eha), multinom (nnet), nlme (nlme), nlmer (lme4), ols (rms), pgmm (plm), 
  # phreg (eha), plm (plm), pmg (plm), polr (MASS), psm (rms), rem.dyad (relevent), 
  # rlm (MASS), rq (quantreg), Rq (rms), selection (sampleSelection), svyglm (survey), 
  # survreg (survival), tobit (AER), weibreg (eha), zeroin (pscl), relogit (zelig), 
  # cloglog.net (zelig), gamma.net (zelig), probit.net (zelig) and logit.net (zelig).
  # not prcomp, ts or zoo?
  # not ftable, xtab, CrossTable?
  
  # preview table in viewer or browser
  htmlout = capture.output(stargazer(obj, type="html", summary=summary, rownames = add.rownames, ...)) 
  texout = capture.output(stargazer(obj, type="latex", summary=summary, rownames = add.rownames, ...))
  outp = preview(htmlout)
    
  # export to HTML/Latex
  
  if (standAlone) {
    texheader=c("\\documentclass[10pt]{article}","\\begin{document}")
    texfooter=c("\\end{document}")
    texout=c(texout[1:3],texheader,texout[4:length(texout)],texfooter)
  }
  if (type=="HTML") { fileConn = file(file)
                      writeLines(htmlout,file)
                      close(fileConn) }
  
  if (type=="TEX") { fileConn = file(file)
                     writeLines(texout,file)
                     close(fileConn) }
  
  message(paste0("Exported table as ",file))
  
}

#' @describeIn table2tex
#' Export statistical output to HTML table
#' @export
table2html = function(...) table2tex(type = "HTML", ...)
