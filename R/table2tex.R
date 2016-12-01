#' Export statistical output to a Microsoft Word / LibreOffice, HTML or Latex table
#' 
#' Export currently showing R stats object or stats object obj to a
#' Microsoft Word / LibreOffice, HTML or Latex table
#' @import stats
#' @importFrom utils capture.output
#' @aliases table2tex table2tex2 table2html table2doc
#' @param x given R stats object or list of stats objects to export; if set to \code{NULL} 
#' the output of the previous R command will be exported.
#' @param file name of output file. The appropriate extension is added automatically.
#' @param type desired output type - \code{"TEX"} for Latex, \code{"DOC"} for Word and \code{"HTML"} for HTML.
#' @param digits number of significant digits to show. A value of NA indicates that
#' no rounding should be done.
#' @param summary logical indicating whether or not to summarize data files.
#' @param standAlone logical indicating whether exported Latex code should be
#' standalone compilable, or whether it will be pasted into another document.
#' @param \dots extra options are passed on to stargazer.
#' @return \code{NULL}
#' @details Objects that can be exported are all those supported by \code{\link[stargazer]{stargazer}},
#' namely \code{aftreg} (\code{eha}), \code{anova} (\code{stats}), \code{aov} (\code{stats}), \code{aovlist} (\code{stats}), \code{arima} (\code{stats}), 
#' \code{betareg} (\code{betareg}), \code{binaryChoice} (\code{sampleSelection}), \code{bj} (\code{rms}), \code{brglm} (\code{brglm}), 
#' \code{censReg} (\code{censReg}), \code{coeftest} (\code{lmtest}), \code{coxph} (\code{survival}), \code{coxreg} (\code{eha}), 
#' \code{clm} (\code{ordinal}), \code{clogit} (\code{survival}), \code{cph} (\code{rms}), \code{dynlm} (\code{dynlm}), \code{ergm} (\code{ergm}), 
#' \code{errorsarlm} (\code{spdev}), \code{felm} (\code{lfe}), \code{gam} (\code{mgcv}), \code{garchFit} (\code{fGarch}), \code{gee} (\code{gee}), 
#' \code{glm} (\code{stats}), \code{Glm} (\code{rms}), \code{glmer} (\code{lme4}), \code{glmrob} (\code{robustbase}), \code{gls} (\code{nlme}), 
#' \code{Gls} (\code{rms}), \code{gmm} (\code{gmm}), \code{heckit} (\code{sampleSelection}), \code{hetglm} (\code{glmx}), \code{hurdle} (\code{pscl}), 
#' \code{ivreg} (\code{AER}), \code{lagarlm} (\code{spdep}), \code{lm} (\code{stats}), \code{lme} (\code{nlme}), \code{lmer} (\code{lme4}), \code{lmrob} (\code{robustbase}), 
#' \code{lrm} (\code{rms}), \code{maBina} (\code{erer}), \code{mclogit} (\code{mclogit}), \code{mlogit} (\code{mlogit}), \code{mnlogit} (\code{mnlogit}), 
#' \code{mlreg} (\code{eha}), \code{multinom} (\code{nnet}), \code{nlme} (\code{nlme}), \code{nlmer} (\code{lme4}), \code{ols} (\code{rms}), \code{pgmm} (\code{plm}), 
#' \code{phreg} (\code{eha}), \code{plm} (\code{plm}), \code{pmg} (\code{plm}), \code{polr} (\code{MASS}), \code{psm} (\code{rms}), \code{rem.dyad} (\code{relevent}), 
#' \code{rlm} (\code{MASS}), \code{rq} (\code{quantreg}), \code{Rq} (\code{rms}), \code{selection} (\code{sampleSelection}), \code{svyglm} (\code{survey}), 
#' \code{survreg} (\code{survival}), \code{tobit} (\code{AER}), \code{weibreg} (\code{eha}), \code{zeroin} (\code{pscl}), \code{relogit} (\code{zelig}), 
#' \code{cloglog.net} (\code{zelig}), \code{gamma.net} (\code{zelig}), \code{probit.net} (\code{zelig}) and \code{logit.net} (\code{zelig}).
#' @author Tom Wenseleers
#' @example examples/table2tex.R
#' @seealso \code{\link{table2ppt}}
#' @export
#' 

table2tex = function(x = NULL, file = "Rtable", type="TEX", digits = 2, summary=FALSE, standAlone=TRUE, ...) {
  obj=x
  if (is.null(obj)) 
    outp = .Last.value else outp = obj  # capture previously shown output or use passed object
  if (is.null(outp)) 
    stop("no R stats object available to export")
  obj=outp

  type=toupper(type)
  type=match.arg(type,c("TEX","DOC","HTML"))
  
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
  htmlout = capture.output(stargazer(obj, type="html", summary=summary, ...)) 
  texout = capture.output(stargazer(obj, type="latex", summary=summary, ...))
  outp = preview(htmlout)
    
  # export to HTML/Word/Latex
  
  if (standAlone) {
    texheader=c("\\documentclass[10pt]{article}","\\begin{document}")
    texfooter=c("\\end{document}")
    texout=c(texout[1:3],texheader,texout[4:length(texout)],texfooter)
  }
  
  if (type=="DOC") { fileConn = file(file)
                     writeLines(htmlout,file)
                     close(fileConn) }
  
  if (type=="HTML") { fileConn = file(file)
                      writeLines(htmlout,file)
                      close(fileConn) }
  
  if (type=="TEX") { fileConn = file(file)
                     writeLines(texout,file)
                     close(fileConn) }
  
  message(paste0("Exported table as ",file))
  
}


#' @describeIn table2tex
#' @export
table2tex2 = function(...) table2tex(type = "TEX", standAlone = FALSE, ...)


#' @describeIn table2tex
#' @export
table2doc = function(...) table2tex(type = "DOC", ...)


#' @describeIn table2tex
#' @export
table2html = function(...) table2tex(type = "HTML", ...)
