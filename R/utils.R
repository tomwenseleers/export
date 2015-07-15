# function to preview HTML in RStudio viewer or web browser

preview = function(x){
    htmlFile = tempfile(fileext=".html")
    cat(x, file=htmlFile)
    viewer = getOption("viewer")
    if (!is.null(viewer))
      viewer(htmlFile)
    else
      utils::browseURL(htmlFile)
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
