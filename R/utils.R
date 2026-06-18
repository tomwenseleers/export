#' @importFrom grDevices recordPlot dev.cur hcl dev.list dev.off
#' @importFrom utils browseURL
#' @import xml2

is_plot_object <- function(x) {
  inherits(x, c("ggplot", "ggplot2::ggplot", "trellis", "patchwork"))
}

resolve_plot <- function(x = NULL, fun = NULL) {
  if (!is.null(x) || !is.null(fun)) {
    return(x)
  }
  if (exists(".Last.value", envir = .GlobalEnv, inherits = FALSE)) {
    last.value <- get(".Last.value", envir = .GlobalEnv)
    if (is_plot_object(last.value)) {
      return(last.value)
    }
  }
  captureplot()
}

plot_dimensions <- function(width = NULL, height = NULL, aspectr = NULL,
                            scaling = 100, default = c(7, 5)) {
  if (!is.null(aspectr) && (!is.numeric(aspectr) || length(aspectr) != 1 || aspectr <= 0)) {
    stop("aspectr must be a single positive number")
  }
  if (!is.null(width) && (!is.numeric(width) || length(width) != 1 || width <= 0)) {
    stop("width must be a single positive number")
  }
  if (!is.null(height) && (!is.numeric(height) || length(height) != 1 || height <= 0)) {
    stop("height must be a single positive number")
  }
  if (!is.numeric(scaling) || length(scaling) != 1 || scaling <= 0) {
    stop("scaling must be a single positive number")
  }

  plotsize <- default
  if (!identical(getOption("device"), FALSE) && dev.cur() != 1) {
    plotsize <- dev.size()
  }
  plotaspectr <- if (is.null(aspectr)) plotsize[[1]] / plotsize[[2]] else aspectr

  if (is.null(width) && is.null(height)) {
    if (is.null(aspectr)) {
      w <- plotsize[[1]]
      h <- plotsize[[2]]
    } else if (plotaspectr >= 1) {
      w <- plotsize[[1]]
      h <- w / plotaspectr
    } else {
      h <- plotsize[[2]]
      w <- h * plotaspectr
    }
  } else if (is.null(height)) {
    w <- width
    h <- w / plotaspectr
  } else if (is.null(width)) {
    h <- height
    w <- h * plotaspectr
  } else {
    w <- width
    h <- height
    plotaspectr <- w / h
  }

  c(width = w * scaling / 100, height = h * scaling / 100, aspectr = plotaspectr)
}

showtext_auto_enabled <- function() {
  hooks <- c(getHook("plot.new"), getHook("grid.newpage"))
  any(vapply(hooks, inherits, logical(1), "showtext_hook"))
}

default_office_font <- function() {
  ifelse(Sys.info()["sysname"] == "Windows", "Arial", "Helvetica")[[1]]
}

element_text_family <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  family <- tryCatch(x@family, error = function(e) NULL)
  if (is.null(family)) {
    family <- tryCatch(x$family, error = function(e) NULL)
  }
  if (is.character(family) && length(family) == 1 && !is.na(family) && nzchar(family)) {
    family
  } else {
    NULL
  }
}

infer_plot_font <- function(x) {
  if (!is_plot_object(x)) {
    return(NULL)
  }

  theme_family <- tryCatch(element_text_family(x[["theme"]][["text"]]),
                           error = function(e) NULL)
  if (!is.null(theme_family)) {
    return(theme_family)
  }

  layers <- tryCatch(x[["layers"]], error = function(e) NULL)
  layer_families <- unlist(lapply(layers, function(layer) {
    tryCatch(layer[["aes_params"]][["family"]], error = function(e) NULL)
  }), use.names = FALSE)
  layer_families <- layer_families[!is.na(layer_families) & nzchar(layer_families)]
  if (length(layer_families) > 0) {
    return(layer_families[[1]])
  }

  NULL
}

resolve_office_font <- function(font = NULL, fonts = NULL, plot = NULL) {
  if (is.null(font)) {
    font <- infer_plot_font(plot)
  }
  if (is.null(font)) {
    font <- default_office_font()
  }
  if (is.null(fonts)) {
    fonts <- list(sans = font, serif = font, mono = font, symbol = font)
  }
  list(font = font, fonts = fonts)
}

splm_to_data_frame <- function(x) {
  sx <- if (inherits(x, "summary.splm")) {
    x
  } else {
    tryCatch(summary(x), error = function(e) x)
  }

  as_table <- function(component, mat) {
    mat <- as.matrix(mat)
    data.frame(component = component,
               term = rownames(mat),
               as.data.frame(mat, check.names = FALSE),
               row.names = NULL,
               check.names = FALSE)
  }

  tables <- list()
  if (!is.null(sx$ErrCompTable)) {
    tables[[length(tables) + 1]] <- as_table("error variance parameters", sx$ErrCompTable)
  }
  if (!is.null(sx$ARCoefTable)) {
    tables[[length(tables) + 1]] <- as_table("spatial autoregressive coefficient", sx$ARCoefTable)
  }
  if (!is.null(sx$CoefTable)) {
    tables[[length(tables) + 1]] <- as_table("coefficients", sx$CoefTable)
  }

  if (length(tables) > 0) {
    return(do.call(rbind, tables))
  }

  coefs <- tryCatch(stats::coef(x), error = function(e) NULL)
  if (is.null(coefs) && !is.null(x$coefficients)) {
    coefs <- x$coefficients
  }
  if (is.null(coefs)) {
    stop("could not extract coefficients from splm object")
  }
  data.frame(component = "coefficients",
             term = names(coefs),
             Estimate = unname(coefs),
             row.names = NULL,
             check.names = FALSE)
}

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

# helper function to show p values right aligned with digitspvals sign digits and 
# degrees of freedom columns as right aligned integers
xtable2 = function(x, ndigits = 2, ndigitspvals = 2, trim.pval = 1E-16, ...) {
  sm = xtable(x)
  ncol = ncol(sm)
  digs = rep(ndigits, ncol + 1)
  disp = rep("f", ncol + 1)
  whch = grep("\\QPr(\\E|\\Qp-value\\E|\\Qp value\\E|\\Qpadj\\E|^p$|^padj$|p[.]value", colnames(sm))
  
  if (length(whch) != 0) {
    for(j in whch){ # Format the pvalues in scientific format
      sm[,j] <- sapply(sm[,j],function(val){
        val <- ifelse(val < trim.pval, 
                      paste0("< ", formatC(trim.pval, format = "e", digits = 0)), 
                      formatC(val, format = "e", digits = ndigitspvals))
        return(val)
      })
    }
  }
  # whch = grep("^Df$|^df$", colnames(sm))
  # if (length(whch) != 0){
  #   digs[whch + 1] = 0
  #   disp[whch + 1] = "d"
  # }
  # digs[c(1,which(!sapply(sm,is.numeric))+1)] <- NA
  # disp[c(1,which(!sapply(sm,is.numeric))+1)] <- "s"
  # for(i in 2:length(digs)){
  #   if(disp[i]=="f") sm[,i-1] <- round(sm[,i-1], digits = digs[i])
  # }
  xtable(sm, digits = digs, display = disp,...)
}

tidy2 <- function(x, ndigits = 2, ndigitspvals = 2, trim.pval = T, ...) {
  x <- tidy(x)
  ncol = ncol(x)
  digs = rep(ndigits, ncol)
  whch = grep("\\QPr(\\E|\\Qp-value\\E|\\Qp value\\E|\\Qpadj\\E|^p$|^padj$|p[.]value", colnames(x))
  if (length(whch) != 0) { digs[whch] = "pval" }
  whch = grep("^Df$|^df$", colnames(x))
  if (length(whch) != 0){ digs[whch] = 0 }
  digs[!sapply(x,is.numeric)] <- NA
  for(i in 1:length(digs)){
    if(is.numeric(digs[i])) x[,i] <- round(x[,i], digits = digs[i])
    if(digs[i]=="pval"){ 
      x[,i] <- sapply(x[,i],function(val){
        ifelse(val < 10^-ndigitspvals & trim.pval,
               paste0("<", 10^-ndigitspvals),
               formatC(val, format = "f", digits = ndigitspvals))
      })
    }
  }
  return(x)
}

data.frame2<- function(x, ndigits = 2, ndigitspvals = 2, trim.pval = T,...) {
  x <- data.frame(x, check.names = F)
  ncol = ncol(x)
  digs = rep(ndigits, ncol)
  whch = grep("\\QPr(\\E|\\Qp-value\\E|\\Qp value\\E|\\Qpadj\\E|^p$|^padj$", colnames(x))
  if (length(whch) != 0) { digs[whch] = "pval" }
  whch = grep("^Df$|^df$", colnames(x))
  if (length(whch) != 0){ digs[whch] = 0 }
  for(i in 1:length(digs)){
    if(is.numeric(digs[i])) x[,i-1] <- round(x[,i-1], digits = digs[i])
    if(digs[i]=="pval"){ 
      x[,i-1] <- sapply(x[,i-1],function(val){
        ifelse(val < 10^-ndigitspvals & trim.pval,
               paste0("<", 10^-ndigitspvals),
               formatC(val, format = "f", digits = ndigitspvals))
      })
    }
  }
  return(x)
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


# Function to extract the slide size of a presentation from an XML (for PPT)
# This is needed when using officer package
get.slide.size <- function(doc, units="in"){
  doc.xml <- doc$presentation$get()
  nodes.xml<- xml_children(doc.xml)
  ind <- which(grepl(sapply(nodes.xml, as.character), pattern = "sldSz"))
  slide.size <- as.numeric(xml_attrs(nodes.xml[[ind]])[1:2])
  fac <- ifelse(units == "cm", 360000, 360000 * 2.54 ) # fac = number of pixels per inch or cm
  slide.size <- slide.size/fac
  names(slide.size) <- c("width", "height")
  return(slide.size)
}


# for a given graph width, height, page orientation and type
# returns the best suited template
# TO DO: maybe still add additional parameter "pageaspectr" to support A4, 16:9 or 4:3
besttemplate = function(w,h,margins = c(top=1,right=1,bottom=1,left=1),orient="auto",type="PPT") {
  # size of A5 to A1 landscape PPT/DOC templates
  landscA=list(c(8.27,5.83),c(11.7,8.27),c(16.5,11.7),c(23.4,16.5),c(33.1,23.4))
  if (type=="PPT" ) { 
    landscA = lapply(landscA,function(x){
      return(x-c(margins["left"]+margins["right"],margins["top"]+margins["bottom"]))
    }) 
  }
  if (type=="DOC") landscA=lapply(landscA,function(x) x-1)[1:3] # allow for 1 inch margins+DOC only supports A5, A4 and A3
  sizes=(5:1)[1:length(landscA)]
  portrA=lapply(landscA,rev) # size of A5 to A1 landscape PPT/DOC templates
  #w=8.9;h=6.7;
  if (orient=="auto") orient=ifelse(w>=h,"landscape","portrait")
  bestpagesize=suppressWarnings(ifelse( orient=="landscape", 
                                        sizes[max( min(which(w<=unlist(lapply(landscA,head,n=1)))), 
                                                                        min(which(h<=unlist(lapply(landscA,tail,n=1)))) )],
                                        sizes[max( min(which(w<=unlist(lapply(portrA,head,n=1)))),
                                                   min(which(h<=unlist(lapply(portrA,tail,n=1)))) )] ))
  if (is.na(bestpagesize)) bestpagesize=min(sizes)
  return(paste0("A",bestpagesize,"_",orient)) 
}


# Function that shuts down the newly opened devices to match a previous setup
# if dev.old is NULL, all devices are clsed
dev.reset <- function(dev.init = NULL) {
  i <- dev.list()[!dev.list() %in% dev.init]
  for (ii in i ) dev.off(ii)
  return(NULL)
}
