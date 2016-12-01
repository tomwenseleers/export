#' @importFrom grDevices recordPlot
#' @importFrom grDevices dev.cur
#' @importFrom utils browseURL

# function to preview HTML in RStudio viewer or web browser

preview = function(x){
    htmlFile = tempfile(fileext=".html")
    cat(x, file=htmlFile)
    viewer = getOption("viewer")
    if ((!is.null(viewer)) && is.function(viewer))
      viewer(htmlFile)
    else
      browseURL(htmlFile)
}


# function to capture currently active plot

captureplot = function() {
  current.device = dev.cur()
  nm = names(current.device)[1L]
  if(nm == "null device") stop("no device to print from")
  if (dev.cur() == 1) 
    stop("no active graphics device found")
  p = invisible(recordPlot())
  p
}


# function to clip a value between an allowed range; x can be a single value, a vector or a matrix

clip = function(x, a, b) {(a + (x-a > 0)*(x-a) - (x-b > 0)*(x-b))} 


# function that returns default ggplot colours for n groups

gg_color_hue <- function(n,l=65,c=100) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=l, c=c)[1:n]
}


# select rows from a dataframe mydata based on a list of selection conditions given 
# in a named list "selection"
# e.g. 
# mydata=data.frame(group1=c(rep("MALE",6),rep("FEMALE",6)),group2=c(rep("TREATED",3),rep("UNTREATED",3)))
# selection=list(group1="MALE",group2="TREATED")
# selrows(mydata,selection)
# 1 2 3
selrows=function(mydata,selection) {
  nms=names(selection)
  sel=data.frame(matrix(TRUE,nrow=nrow(mydata),ncol=length(nms)))
  for (i in 1:length(nms)) { sel[,i]=(mydata[,nms[[i]]]==selection[nms[[i]]][[1]]) }
  which(apply(sel*1,1,prod)==1)
}


