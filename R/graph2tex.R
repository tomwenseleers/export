#' Save currently active R graph to Latex format
#' 
#' Save the currently active R graph or a graph passed as an object or function 
#' to Latex format with sensible defaults
#' 
#' 
#' @aliases graph2tex graph2tex2
#' @param obj given \code{ggplot2} plot or \code{lattice} plot object to export; if
#' set to \code{NULL} the currently active R graph will be exported; not
#' supported for base R plots.
#' @param file name of output file; extension \code{".tex"} is added automatically.
#' If file already exists it is overwritten.
#' @param fun plot passed on as a function used to create it; useful especially
#' for base R plots.
#' @param aspectr desired width to height aspect ratio. If set to \code{NULL}, the
#' aspect ratio of the graphics device is used. Can also be combined with one
#' value for either the desired width or height of the graph.
#' @param width desired width in inches; can be combined with a desired
#' aspect ratio aspectr.
#' @param height desired height in inches; can be combined with a desired
#' aspect ratio aspectr.
#' @param scaling scale width & height by a certain percentage.
#' @param font default \code{"sans"} uses \code{sansmath} font for output; any other value
#' will result in the font of the Latex document being used.
#' @param bg desired background colour, e.g. \code{"white"} or \code{"transparent"}.
#' @param standAlone logical indicating whether output should be a full compilable
#' Latex document or whether only the graph output should be produced, for pasting
#' into another document. \code{\link{graph2tex}} default is \code{standAlone=TRUE}, whereas \code{\link{graph2tex2}}
#' has default \code{standAlone=FALSE}.
#' @param \dots any other options are passed on to \code{tikzDevice}'s \code{\link[tikzDevice]{tikz}} function.
#' @return \code{NULL}
#' @author Tom Wenseleers
#' @example examples/graph2tex.R
#' @seealso \code{\link{graph2office}}, \code{\link{graph2vector}}, \code{\link{graph2svg}}, \code{\link{graph2pdf}}, \code{\link{graph2eps}},
#' \code{\link{graph2bitmap}}, \code{\link{graph2png}}, \code{\link{graph2tif}}, \code{\link{graph2jpg}} 
#' @export
#' 
graph2tex = function(obj = NULL, file = "Rplot", fun = NULL,   
                        aspectr = NULL, width = NULL, height = NULL, 
                        scaling = 100, font = "sans", bg = "transparent", 
                        standAlone = TRUE, ...) {
  ext = ".tex"
  file = sub("^(.*)[.].*", "\\1", file)  # remove extension if given
  file = paste0(file, ext)  # add extension
  if (is.null(obj) & is.null(fun)) p = captureplot() else p = obj
  if (inherits(p,"list")) 
    stop("base R plots cannot be passed as objects, use ggplot2 or lattice plots instead")
  myplot = if (is.null(fun)) function(pl = p) print(pl) else fun
  
  if (!is.na(font)) {if (font=="sans-serif"|font=="Helvetica"|font=="Arial"|font=="Verdana"|font=="Tahoma") font="sans"}
  plotsize = dev.size()  # also works if no graphics device is open
  w = plotsize[[1]]
  h = plotsize[[2]]
  plotaspectr = plotsize[[1]]/plotsize[[2]]
  if ((!is.null(aspectr))&is.null(height)&is.null(width)) { plotaspectr = aspectr
                                                            if (plotaspectr >= 1) { 
                                                              h = w/plotaspectr } else { w = h*plotaspectr } 
  }
  if ((is.null(height))&(!is.null(width))) { w = width; h = w / plotaspectr }
  if ((is.null(width))&(!is.null(height))) { h = height; w = h / plotaspectr } 
  # if width and height is given override other scaling params
  if ((!is.null(width))&(!is.null(height))) { w = width; h = height }  
  w = w*scaling/100; h = h*scaling/100;
  
  tikzDevice::tikz(file = file, 
        height = h, 
        width = w,
        bg = bg,
       standAlone = standAlone,
        ... )
    myplot()
  dev.off()
  
  if (!is.null(font)) {   if ("sans" %in% font) {
                          out = readLines(file) 
                          # see http://tex.stackexchange.com/questions/4887/pgf-tikz-and-sans-serif-fonts
                          # see http://tex.stackexchange.com/questions/118678/change-font-family-in-pgfplots-followup
                          l1 = min(grep("usepackage\\{tikz\\}",out))
                          extra1="\\usepackage{helvet}"
                          extra2="\\usepackage[eulergreek]{sansmath}"
                          extra3 = paste0("[font=\\sffamily\\sansmath]")
                          l2 = min(grep("begin\\{tikzpicture\\}",out))
                          out2 = c(out[1:l1],extra1,extra2,out[(l1+1):l2],extra3,out[(l2+1):length(out)])
                          fileConn = file(file)
                          writeLines(out2,file)
                          close(fileConn) 
     }}
  
  message(paste0("Exported graph as ",file))
  
}

#' @describeIn graph2tex
#' @export
#' 
graph2tex2 = function(...) graph2tex(standAlone = FALSE, ...)

