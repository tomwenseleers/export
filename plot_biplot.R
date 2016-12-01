#' Visually appealing 2D biplot
#  modified from function ggbiplot.r by Vincent Q. Vu
#
#' @importFrom rgl rgl.cur
#' @import ggplot2
#' @importFrom scales expand_range
#' @importFrom scales muted
#' @importFrom plyr ddply
#' @importFrom grid arrow
#' @importFrom grid gpar
#' @importFrom grid unit
#' @importFrom ggthemes theme_few 
#' @aliases Biplot
#' @param pcobj           an object returned by prcomp() or princomp()
#' @param choices         vector specifying which PC axes to plot. A 2 element
#' vector will result in a 2D ggplot2 biplot, whereas a 3 element vector will 
#' result in an interactive 3D rgl biplot.
#' @param scale           covariance biplot (scale = 1), form biplot (scale = 0). When scale = 1, the inner product between the variables approximates the covariance and the distance between the points approximates the Mahalanobis distance.
#' @param pc.biplot       for compatibility with biplot.princomp(); if true, use what Gabriel (1971) refers to as a "principal component biplot", with lambda = 1 and observations scaled up by sqrt(n) and variables scaled down by sqrt(n). Then inner products between variables approximate covariances and distances between observations approximate Mahalanobis distance.
#' @param equalize        should the basis vectors be rescaled to be comparable with the score vectors?
#' @param groups          optional factor variable indicating the groups that the observations belong to. If provided the points will be colored according to their group membership.
#' @param legend          logical specifying whether or not to include a legend
#' in case grouping variable groups is provided.
#' @param legend.pos      vector with x,y coordinates of upper left corner where 
#' legend should be placed
#' @param pointsize       size of factor score points
#' @param cols            if a grouping variable groups is provided an optional
#' vector of colours to use to plot the factor scores of observations corresponding
#' with the different groups (length should be equal to the number of groups); if
#' no grouping variable is provided it can be a single colour to use to plot the
#' factor scores; NULL results in default ggplot2 equally spaced HCL colours being used
#' @param l               luminance in the range of [0,100] of default HCL colours to use
#' @param c               chroma in the range of [0,100] of default HCL colours to use
#' @param alpha           alpha transparency of factor score points (0 = transparent, 1 = opaque)
#' @param highlight       with values "chull" or "ellipse" factor scores will be 
#' encircled with their convex hull or confidence ellipse; omitted if set to "none"
#' or if no grouping variable is provided
#' @param highl.alpha     alpha transparency to use to draw the filled convex hull
#' or confidence ellipses (0 = transparent, 1 = opaque)
#' @param ellipse.prob    size of the ellipse in terms of a multivariate t distribution
#' @param arrows          logical indicating whether or not to draw arrows for the variables
#' @param arrows.col      colour to use for the arrows
#' @param arrows.type     type of arrow to draw ("closed" or "open")
#' @param varnames        optional vector of variables names to use; if set to
#' NULL variable names will be extracted from the PCA pcobj object; of set to "nrs"
#' variable names will be substituted with numbers and the full names will be included
#' in a legend at the bottom of the plot
#' @param minlength        minimum Euclidian length of a given factor loading arrow
#' required to include it in the biplot, bounded in the range [0-1].
#' @param varname.size    size of the text for variable names
#' @param varname.adjust  adjustment factor the placement of the variable names, >= 1 means farther from the arrow
#' @param varname.abbrev  whether or not to abbreviate the variable names
#' @param varname.legend  logical indicating whether or not to include a legend
#' at the bottom with the numbers that are used for given variables if varnames="nrs"
#' @param varname.legend.par  grid gpar graphical parameter object giving styling
#' attributed for the variable name legend
#' @param varname.legend.pos  x,y placement of variable name legend as a fraction
#' of the x and y axis range
#' @param labels          optional vector of labels for the observations; if NULL
#' observations will be shown as points
#' @param label.size      size of the text to use for factor score labels
#' @param circle          draw a correlation circle? (only applies when prcomp was called with scale = TRUE and when scale=1)
#' @param circle.prob     radius of correlation circle is scaled so that it corresponds to 
#' a data ellipse for the standardized PC scores with probability circle.prob; larger values will
#' result in a larger circle compared to the observations
#' @param circle.col    colour to use for correlation circle 
#' @param circle.alpha    alpha transparency of colour to use for correlation circle 
#' (0 = transparent, 1 = opaque)  
#' @param lims            optional 2 element vector with limits to use for axes; always
#' taken identical for all PC axes; chosen automatically when set to NULL 
#' @param expand          additive expansion factor of axis ranges to use when automatic
#' limits are used (lims=NULL)
#' @return a ggplot2 or rgl plot, depending on whether 2 or 3 PC axes are chosen
#' @examples examples/plot_biplot.R
#' @export
#' 
Biplot = function (pcobj, choices = 1:2, scale = ifelse(inherits(pcobj, "lda"),0,1), pc.biplot = FALSE,
                   groups = NULL, 
                   legend = TRUE, legend.pos = c(0.12,0.9),
                   pointsize = 1, cols = NULL, l=65, c=100, alpha = 1, 
                   highlight = c("chull","ellipse","none"),
                   highl.alpha = 0.2, ellipse.prob = 0.68, 
                   arrows = TRUE, arrows.col = muted("red"), arrows.type=c("closed","open"),
                   varnames = NULL, minlength = 0, varname.size = 5, varname.adjust = 1, 
                   varname.abbrev = FALSE, varname.legend = TRUE, 
                   varname.legend.par = gpar(col="black", fontsize=12, fontface="italic"),
                   varname.legend.pos = c(1,0.95),
                   labels = NULL, label.size = 3, 
                   circle = FALSE, circle.prob = 0.69, circle.col = arrows.col, circle.alpha = 1,   
                   lims = NULL, expand = 0.1, 
                   ...) 
{
  stopifnot(length(choices) == 2|length(choices) == 3)
  if (length(choices) == 2) type="2d" else type="3d"
  highlight=match.arg(highlight)
  if (is.null(groups)) highlight="none"
  arrows.type=match.arg(arrows.type)
  
  pointsize=pointsize*3.2 # change pointsize scaling by factor so that pointsize=1 looks nice
  if (!is.null(groups)) {groups <- factor(groups,levels=sort(levels(as.factor(groups))))} # sort factor levels
  
  # recover the SVD
  if(inherits(pcobj, 'prcomp')){
    nobs.factor <- sqrt(nrow(pcobj$x) - 1)
    d <- pcobj$sdev
    u <- sweep(pcobj$x, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- pcobj$rotation
  } else if(inherits(pcobj, 'princomp')) {
    nobs.factor <- sqrt(pcobj$n.obs)
    d <- pcobj$sdev
    u <- sweep(pcobj$scores, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- pcobj$loadings
  } else if(inherits(pcobj, 'PCA')) {
    nobs.factor <- sqrt(nrow(pcobj$call$X))
    d <- unlist(sqrt(pcobj$eig)[1])
    u <- sweep(pcobj$ind$coord, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- sweep(pcobj$var$coord,2,sqrt(pcobj$eig[1:ncol(pcobj$var$coord),1]),FUN="/")
  } else if(inherits(pcobj, "lda")) {
    nobs.factor <- sqrt(pcobj$N)
    d <- pcobj$svd
    u <- predict(pcobj)$x # /nobs.factor
    v <- pcobj$scaling
    d.total <- sum(d^2)
  } else {
    stop('Expected a object of class prcomp, princomp, PCA, or lda')
  }
  
  # factor scores
  choices <- pmin(choices, ncol(u))
  df.u <- as.data.frame(sweep(u[, choices], 2, d[choices]^(1-scale), FUN = "*")) # sweep(fit$x, 2, model$svd^(-scale), FUN = "*")
  
  # factor loadings / directions
  v <- sweep(v, 2, d^scale, FUN = "*")  # sweep(model$scaling, 2, model$svd^scale, FUN = "*")
  df.v <- as.data.frame(v[, choices])
  if (is.null(varnames)) {
    varnames <- rownames(df.v)
  }
  
  
  veclen = function (x) as.vector(sqrt(x %*% x)) # Euclidian length of vector
  veclens = apply(df.v[,choices],1,veclen) # length of factor loading arrows
  # subset factor loadings based on minimum required Euclidian length
  if (!is.null(minlength)) selected=which(veclens>=minlength) else selected=1:length(veclens)
  df.v = df.v[selected,,drop=FALSE]
  if (length(selected)==0) arrows=FALSE
  
  names(df.u) <- c("xvar", "yvar")
  names(df.v) <- names(df.u)
  
  if (pc.biplot) {
    df.u <- df.u * nobs.factor
  }
  
  # scale radius of correlation circle so that it corresponds to 
  # a data ellipse for the standardized PC scores
  r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4)
  r <- 1
  
#   if (equalize) {
#     r <- sqrt( median(rowSums(df.u^2)) / max(colSums(df.v^2)) )
#   }
  
  # scale factor loadings / directions
  # v.scale <- r / sqrt(max(rowSums(v^2))) # rowSums(v^2)
  # df.v <- v.scale * df.v # r * df.v/sqrt(max(v.scale))
#   v.scale <- rowSums(v^2)
#   df.v <- r * df.v/sqrt(max(v.scale))
  
  v.scale <- r / sqrt(max(rowSums(v^2)))
  df.v <- v.scale * df.v
  
  # change labels for the axes
  if (scale == 1) {
    u.axis.labs <- paste("standardized PC", choices, sep = "")
  } else {
    u.axis.labs <- paste("PC", choices, sep = "")
  }
  if(inherits(pcobj, "lda")) u.axis.labs <- gsub("PC","LD",u.axis.labs)
  
  # include proportion of explained variance in labels
  u.axis.labs <- paste(u.axis.labs, sprintf("(%0.1f%%)", 
                                            100 * pcobj$sdev[choices]^2/sum(pcobj$sdev^2)))
  
  # factor loadings / variable names [ labels taken from rownames(pcobj$rotation) ]
  if (!is.null(varnames)) { if (varnames[[1]]=="nrs") { varnames <- as.character(1:nrow(df.v))
  legnd=paste0(c(rbind(varnames,"=",rownames(df.v),", ")),collapse="")
  legnd=substr(legnd, 1, nchar(legnd)-2) 
  message(legnd)
  } else { varnames <- varnames[selected]
           varname.legend <- FALSE } } else { varnames <- rownames(df.v)
  varname.legend <- FALSE }
  # labels taken from rownames(pcobj$rotation)
  if (varname.abbrev) { varnames <- abbreviate(varnames) } 
  df.v$varname <- varnames 
  
  # factor score labels
  if (!is.null(labels)) {
    if (labels[[1]]=="nrs") labels=rownames(df.u)
    df.u$labels <- labels
  }
  
  # grouping variable
  if (!is.null(groups)) {
    df.u$groups <- groups
    ngroups <- length(unique(groups))
    if (is.null(cols)) cols <- gg_color_hue(ngroups,l=l,c=c)
  } else { df.u$groups <- as.factor(1)
  ngroups = 1
  legend = FALSE
  if (is.null(cols)) cols <- gg_color_hue(ngroups,l=l,c=c) else cols <- cols[[1]] }
  
  if (type=="2d") {
    
    # base plot of factor scores
    g <- ggplot(data = df.u, aes(x = xvar, y = yvar, colour=groups)) + 
      xlab(u.axis.labs[1]) + 
      ylab(u.axis.labs[2]) + 
      coord_equal()
    
    # add confidence ellipses or convex hulls
    if (!is.null(df.u$groups) && highlight=="ellipse") { g <- g + 
      stat_ellipse(geom = "polygon", level=ellipse.prob, alpha = highl.alpha, 
                   aes(fill=groups, color=NA)) }
    
    # if (!is.null(df.u$groups) && highlight=="chull") {
    #   # get the convex hull of each unique point set
    #   df <- na.omit(df.u) # chull function does not work with missing data
    #   find_hull <- function(df) df[chull(df[,1], df[,2]), ]
    #   hulls <- ddply(df, "groups", find_hull)
    #   g <- g + geom_polygon(data = hulls, alpha = highl.alpha, aes(fill=groups, color=NA))
    # }  
    
    # draw either labels or points
    if (!is.null(df.u$labels)) {
      if (!is.null(df.u$groups)) {
        g <- g + geom_text(aes(label = labels, color = groups), 
                           size = label.size)
      }
      else {
        g <- g + geom_text(aes(label = labels), size = label.size)
      }
    } else {
      if (!is.null(df.u$groups)) {
        g <- g + geom_point(aes(color = groups), alpha = alpha, size=pointsize)
      } else {
        g <- g + geom_point(alpha = alpha, size=pointsize)
      }
    }
    
    if (arrows) { 
      # draw unit circle
      if (circle) { 
#         theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, 
#                                                   length = 50))
#         circle <- data.frame(xvar = r * cos(theta), yvar = r * 
#                                sin(theta))
#         g <- g + geom_path(data = circle, color = circle.col, 
#                            size = 1/2, alpha = circle.alpha)
        g <- g + annotate("path",
                          x=r*cos(seq(0,2*pi,length.out=100)),
                          y=r*sin(seq(0,2*pi,length.out=100)),color = circle.col, 
                          alpha = circle.alpha)
      }
      
      # draw factor loadings as arrows
      g <- g + geom_segment(data = df.v, aes(x = 0, y = 0, 
                                             xend = xvar, yend = yvar), 
                            arrow = arrow(angle = 15, length = unit(1,"picas"), 
                                          type=arrows.type), color = arrows.col)
      
      # add factor loadings labels
      maxlenlabels=max(nchar(varnames))
      varname.adjust=1+2*varname.adjust*exp(-0.2*maxlenlabels) 
      # adjustment factor to adjust whitespace to length of labels, hackish for the moment
      # TO DO: insert whitespace of one character
      
      df.v$angle <- with(df.v, (180/pi) * atan(yvar/xvar)) 
      df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar))/2) 
      
      g <- g + geom_text(data = df.v, aes(label = varnames, 
                                          x = xvar, y = yvar, angle = angle, hjust = hjust), 
                         color = arrows.col, size = varname.size) 
      # TO DO: once new ggplot2 version is out change to geom_label with fill="white" to 
      # add white box underneath labels to improve readability of labels
    }
    
    # add legend and change theme  
    g <- suppressMessages(g + scale_color_discrete(name = '') + 
                            scale_size(range=c(0, 1), guide=FALSE) +
                            theme_few(base_size=18) + 
                            scale_fill_manual(values=cols,guide=FALSE))
    # + 
    #                         # for nontransparent cols: put alpha=1 above and muted(cols,l=92,c=20)
    #                         scale_colour_manual(values=cols,
    #                                             guide = guide_legend(order=0,
    #                                                                  title=NULL,
    #                                             override.aes=list(fill=NA,
    #                                                               legend.background = 
    #                                                                 element_rect()),
    #                                             legend.background = element_rect())))
    
    if (legend) g = g + theme(legend.direction = 'vertical',legend.position = legend.pos, title=NULL, 
                              legend.background = element_rect(fill=adjustcolor("white",alpha.f=0.5),color=NA))
    else g = g + theme(legend.direction = 'vertical',legend.position = "none", title=NULL, 
                       legend.background = element_rect(fill=adjustcolor("white",alpha.f=0.5),color=NA))
    # or 'top'
    
    # adjust plot range
    currrng=range(unlist(ggplot_build(g)$panel$ranges[[1]][c("x.range","y.range")])) 
    currrng=c(-max(abs(currrng)),max(abs(currrng))) # make axis ranges symmetric
    if (is.null(lims)) lims=expand_range(currrng,add=expand)
    g <- g + xlim(lims[[1]],lims[[2]]) + ylim(lims[[1]],lims[[2]])  
    
    # add variable name legend if desired
    if (varname.legend)  { txt = splitTextGrob(legnd,gp=varname.legend.par)
    g <- g + annotation_custom(grob = txt,  
                               xmin = varname.legend.pos[[1]]*min(lims), 
                               xmax = varname.legend.pos[[1]]*max(lims), 
                               ymin = varname.legend.pos[[2]]*min(lims), 
                               ymax = varname.legend.pos[[2]]*min(lims)) }
    
    return(g) }
  
  # TO DO: maybe also support 
  # nipals, principal, factanal, factor.pa, PLS, pcaridge, prm, pcaRes, bfa, rda, MDS, LRA, MCA, CCA, CVA
  # TO DO: add separate axes for unit circle? (currently difficult in both ggplot2 and rgl)
}


# library(rgl)
# plot3d(pc$scores[,1:3], col=c("red","blue","green")[iris$Species])
# text3d(pc$scores[,1:3],texts=rownames(iris))
# text3d(pc$loadings[,1:3], texts=rownames(pc$loadings), col="red")
# coords <- NULL
# for (i in 1:nrow(pc$loadings)) {
#   coords <- rbind(coords, rbind(c(0,0,0),pc$loadings[i,1:3]))
# }
# lines3d(coords, col="red", lwd=4)
# 
# 
# 
# 
# data(iris)
# #iris$Species <- factor(iris$Species)
# pc <- princomp(iris[,1:4], cor=TRUE, scores=TRUE)
# 
# library(rgl)
# open3d(zoom=0.8,userMatrix=matrix(c(0.80,-0.60,0.022,0,0.23,0.34,0.91,0,-0.55,-0.72,0.41,0,0,0,0,1),
#                                   ncol=4,byrow=T),windowRect=c(0,29,1920,1032),antialias=8)
# 
# cols=c("red","blue","green")
# plot3d(pc$scores[,1:3], col=cols[iris$Species],type="s",lit=TRUE, size=0.5,aspect=c(1,1,0.75))
# 
# for (i in 1:length(unique(iris$Species))) {
# sp=unique(iris$Species)[[i]]
# col=cols[[i]]
# # Find the centre of the ellipsoid
# mean.vec <- c(mean(pc$scores[,1][iris$Species==sp]), mean(pc$scores[,2][iris$Species==sp]), mean(pc$scores[,3][iris$Species==sp])) 
# 
# # Define covariance matrix. We are only using the first 3 columns of mtcars.
# sigma <- cov(pc$scores[,1:3][iris$Species==sp,])
#  
# # Add confidence ellipsoid to plot.
# plot3d( ellipse3d(x = sigma, centre=mean.vec, level=0.95, subdivide=5, smooth=TRUE),
#         col=col, alpha=0.3, add = TRUE, lit=TRUE,
#         point_antialias=TRUE,
#         line_antialias=TRUE,shininess=50)
# }
# 
# 
# library(pca3d)
# pca2d(pc,biplot=TRUE,group=iris$Species,shape="sphere",show.ellipses=TRUE)
# pca3d(pc,biplot=TRUE,group=iris$Species,shape="sphere",show.ellipses=TRUE) # quite nice but could use some tweaking
# 
# 
# 
# #### NICE BIPLOT, see https://github.com/vqv/ggbiplot/blob/master/R/ggbiplot.r

