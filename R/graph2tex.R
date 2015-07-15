#' Save currently active R graph to Latex format
#' 
#' Save the currently active R graph or a graph passed as an object or function 
#' to Latex format with sensible defaults
#' 
#' 
#' @aliases graph2tex graph2tex2
#' @param file name of output file; extension ".tex" is added automatically.
#' If file already exists it is overwritten.
#' @param obj given ggplot2 plot or lattice plot object to export; if
#' set to NULL the currently active R graph will be exported; not
#' supported for base R plots.
#' @param fun plot passed on as a function used to create it; useful especially
#' for base R plots.
#' @param aspectr desired width to height aspect ratio. If set to NULL, the
#' aspect ratio of the graphics device is used. Can also be combined with one
#' value for either the desired width or height of the graph.
#' @param width desired width in inches; can be combined with a desired
#' aspect ratio aspectr.
#' @param height desired height in inches; can be combined with a desired
#' aspect ratio aspectr.
#' @param scaling scale width & height by a certain percentage.
#' @param font default "sans" uses sansmath font for output; any other value
#' will result in the font of the Latex document being used.
#' @param bg desired background colour, e.g. "white" or "transparent".
#' @param standAlone logical indicating whether output should be a full compilable
#' Latex document or whether only the graph output should be produced, for pasting
#' into another document. graph2tex default is standAlone=TRUE, whereas graph2tex2
#' has default standAlone=FALSE.
#' @param \dots any other options are passed on to tikzDevice's tikz function.
#' @return NULL
#' @note %% ~~further notes~~
#' @author Tom Wenseleers
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @example examples/graph2tex.R
#' @export
#' 
graph2tex = function(file = "Rplot", obj = NULL, fun = NULL,   
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

