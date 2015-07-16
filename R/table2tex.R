#' Export statistical output to a Microsoft Word / LibreOffice, HTML or Latex table
#' 
#' Export currently showing R stats object or stats object obj to a
#' Microsoft Word / LibreOffice, HTML or Latex table
#' 
#' @aliases table2tex table2tex2 table2html table2doc
#' @param file name of output file. The appropriate extension is added automatically.
#' @param obj given R stats object or list of stats objects to export; if set to NULL 
#' the output of the previous R command will be exported.
#' @param digits number of significant digits to show. A value of NA indicates that
#' no rounding should be done.
#' @param summary logical indicating whether or not to summarize data files.
#' @param \dots extra options are passed on to stargazer.
#' @return NULL
#' @note Objects that can be exported are all those supported by stargazer,
#' namely aftreg (eha), anova (stats), aov (stats), aovlist (stats), arima (stats), 
# betareg (betareg), binaryChoice (sampleSelection), bj (rms), brglm (brglm), 
# censReg (censReg), coeftest (lmtest), coxph (survival), coxreg (eha), 
# clm (ordinal), clogit (survival), cph (rms), dynlm (dynlm), ergm (ergm), 
# errorsarlm (spdev), felm (lfe), gam (mgcv), garchFit (fGarch), gee (gee), 
# glm (stats), Glm (rms), glmer (lme4), glmrob(robustbase), gls (nlme), 
# Gls (rms), gmm (gmm), heckit (sampleSelection), hetglm (glmx), hurdle (pscl), 
# ivreg (AER), lagarlm (spdep), lm (stats), lme (nlme), lmer (lme4), lmrob (robustbase), 
# lrm (rms), maBina (erer), mclogit (mclogit), mlogit (mlogit), mnlogit (mnlogit), 
# mlreg (eha), multinom (nnet), nlme (nlme), nlmer (lme4), ols (rms), pgmm (plm), 
# phreg (eha), plm (plm), pmg (plm), polr (MASS), psm (rms), rem.dyad (relevent), 
# rlm (MASS), rq (quantreg), Rq (rms), selection (sampleSelection), svyglm (survey), 
# survreg (survival), tobit (AER), weibreg (eha), zeroin (pscl), relogit (zelig), 
# cloglog.net (zelig), gamma.net (zelig), probit.net (zelig) and logit.net (zelig).
#' @author Tom Wenseleers
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @example examples/table2tex.R
#' @export
#' 

table2tex = function(file = "Rtable", obj = NULL, type="TEX", digits = 2, summary=FALSE, standAlone=TRUE, ...) {
  
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
  supported=!grepl("Unrecognized object type",paste(capture.output(stargazer::stargazer(obj)),collapse=""))
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
  htmlout = capture.output(stargazer::stargazer(obj, type="html", summary=summary, ...)) 
  texout = capture.output(stargazer::stargazer(obj, type="latex", summary=summary, ...))
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
