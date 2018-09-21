#' Plot predictions of a (general) linear model in 2D or 3D
#' 
#' Plot the predictions of a [general(ized)] linear model as a colour-coded 
#' 2D contour plot or 3D surface, if desired together with the raw data
#' 
#' 
#' @importFrom grDevices dev.size
#' @importFrom grDevices png
#' @importFrom grDevices dev.off
#' @importFrom grDevices tiff
#' @importFrom grDevices jpeg
#' @aliases plotfit2d plotfit3d
#' @param x given \code{ggplot2} plot or \code{lattice} plot object to export; if
#' set to \code{NULL} the currently active R graph will be exported; not
#' supported for base R plots.
#' @param file name of output file. Any extension is ignored and added
#' according to the requested output type. If file already exists it is overwritten.
#' @param fun plot passed on as a function used to create it; useful especially
#' for base R plots.
#' @param type desired output type - \code{PNG}, \code{TIF} or \code{JPG} are currently supported.
#' \code{PNG} is the preferred format, as it is a lossless format, and compresses better
#' than \code{TIF}.
#' @param aspectr desired width to height aspect ratio. If set to \code{NULL}, the
#' aspect ratio of the graphics device is used. Can also be combined with one
#' value for either the desired width or height of the graph.
#' @param width desired width in inches; can be combined with a desired
#' aspect ratio aspectr.
#' @param height desired height in inches; can be combined with a desired
#' aspect ratio aspectr.
#' @param dpi desired output in dpi; defaults to 600 dpi.
#' @param scaling scale width & height by a certain percentage.
#' @param font desired font to use for labels in PNG and TIFF output; defaults to 
#' \code{"Arial"} on Windows systems and to \code{"Helvetica"} on other systems.
#' @param bg desired background colour, e.g. \code{"white"} or \code{"transparent"}.
#' @param cairo logical, specifying whether or not to use \code{Cairographics} for export.
#' @param tiffcompression compression to use for \code{TIF} files.
#' @param jpegquality quality of \code{JPEG} compression.
#' @param \dots any other options are passed on to \code{ReporteRs}'s \code{\link[ReporteRs]{addPlot}} function.
#' @return \code{NULL}
#' @author Tom Wenseleers
#' @example examples/graph2bitmap.R
#' @seealso \code{\link{graph2office}}, \code{\link{graph2vector}}, \code{\link{graph2svg}}, \code{\link{graph2pdf}},
#' \code{\link{graph2eps}}, \code{\link{graph2tex}} 
#' @export
#' 

# load required packages
reqpackagesCRAN=c("rockchalk","aqfig","colorRamps","rgl","car","psych","lmtest","colorspace")
instpackages=installed.packages()[,1]
packagestoinstall=setdiff(reqpackagesCRAN,instpackages)
if (length(packagestoinstall)>0) install.packages(packagestoinstall,dependencies=TRUE,repos = "http://cran.us.r-project.org") 
invisible(lapply(reqpackagesCRAN, require, character.only=T))


# plot predictions of a (general) linear model as a function of two explanatory variables as an image / contour plot
# together with the actual data points
# mean value or mode is used for any other variables in the model
plotfit2d = function(model=NULL, plotx=NULL, ploty=NULL, points=TRUE,
                   contours=TRUE, legend=FALSE, npp=1000, xlab=NULL, ylab=NULL,zlab=NULL,
                   xlim=NULL, ylim=NULL, pch=16, cex=1.2, lwd=0.1,
                   col.palette = rev(rainbow_hcl(1000,c=100,l=60))) {
  #n=npp
  require(rockchalk)
  require(aqfig)
  require(colorRamps)
  require(colorspace)
  require(MASS)
  mf=model.frame(model);emf=rockchalk::model.data(model)
  if (is.null(xlab)) xlab=plotx
  if (is.null(ylab)) ylab=ploty
  if (is.null(zlab)) zlab=names(mf)[[1]]
  x=emf[,plotx];y=emf[,ploty];z=mf[,1]
  if (is.null(xlim)) xlim=c(min(x)*0.95,max(x)*1.05)
  if (is.null(ylim)) ylim=c(min(y)*0.95,max(y)*1.05)
  preds=predictOMatic(model,predVals=c(plotx,ploty),n=npp,divider="seq")
  zpred=matrix(preds[,"fit"],npp,npp)
  zlim=c(min(c(preds$fit,z)),max(c(preds$fit,z)))
  par(mai=c(1.2,1.2,0.5,1.2),fin=c(6.5,6))
  cols=col.palette[(zpred-zlim[1])*999/diff(zlim)+1]
  graphics::image(x=seq(xlim[1],xlim[2],len=npp),y=seq(ylim[1],ylim[2],len=npp),z=zpred,xlab=xlab,ylab=ylab,col=cols,useRaster=T,xaxs="i",yaxs="i")
  if (contours) graphics::contour(x=seq(xlim[1],xlim[2],len=npp),y=seq(ylim[1],ylim[2],len=npp),z=zpred,xlab=xlab,ylab=ylab,add=T,method="edge")
  if (points) {cols1=col.palette[(z-zlim[1])*999/diff(zlim)+1]
                   pch1=rep(pch,length(n))
                   cols2=adjustcolor(cols1,offset=c(-0.3,-0.3,-0.3,1))
                   pch2=pch-15
                   points(c(rbind(x,x)),c(rbind(y,y)), cex=cex,col=c(rbind(cols1,cols2)),pch=c(rbind(pch1,pch2)),lwd=lwd) }
  box()
  if (legend) vertical.image.legend(zlim=zlim,col=col.palette) 
  # TO DO: add z axis label, maybe make legend a bit smaller?
}



# plot predictions of a (general) linear model as a function of two explanatory variables as an interactive 3D plot
# mean value or mode is used for any other variables in the model
plotfit3d=function(model=NULL,plotx1=NULL,plotx2=NULL,residuals=TRUE,
                        droplines=TRUE,selection=NULL, npp=50, lwddrop=2,
                        x1lab=NULL,x2lab=NULL,ylab=NULL,x1lim=NULL,x2lim=NULL,ylim=NULL,
                        cex=1.5,col.palette=rev(rainbow_hcl(1000,c=100,l=65)),
                        segcol="black",segalpha=0.5,interval="none",
                        level=0.95, confcol="lightgrey",confalpha=0.4,
                        pointsalpha=1,lit=TRUE,
                        aspectr=c(1,1,0.71),zoom=0.8,
                        userMatrix=matrix(c(0.80,-0.60,0.022,0,0.23,0.34,0.91,0,-0.55,-0.72,0.41,0,0,0,0,1),
                        ncol=4,byrow=T),
                        windowRect=c(0,29,1920,1032)) { 
  require(rockchalk)
  require(rgl)
  require(colorRamps)
  require(colorspace)
  require(MASS)
  require(pbkrtest)
    
  # raw data
  mf=model.frame(model);
  emf=rockchalk::model.data(model)
  
  if (!is.null(selection)) selected=selrows(emf,selection) else selected=1:nrow(emf)
  
  x1=mf[,which(plotx1==names(emf))] 
  x2=mf[,which(plotx2==names(emf))] 
  y=emf[,1]
  
  if (is.null(x1lab)) x1lab=plotx1
  if (is.null(x2lab)) x2lab=plotx2
  if (is.null(ylab)) ylab=names(mf)[[1]]
  if (is.null(x1lim)) x1lim=range(x1)
  if (is.null(x2lim)) x2lim=range(x2)
  if (is.null(ylim)) zlim=range(y) else zlim=ylim
    
  
  xlvls=setNames(list(x1=seq(from=x1lim[[1]],to=x1lim[[2]],length.out=npp),
                      x2=seq(from=x2lim[[1]],to=x2lim[[2]],length.out=npp)),
                 c(plotx1,plotx2))
  if (!is.null(selection)) xlvls=c(xlvls,selection)
  
  preds=predictOMatic(model,predVals=xlvls,n=npp,divider="seq",interval=interval,level=level,type="response")

  x1=x1[selected]
  x2=x2[selected]
  y=y[selected]
  #resid=resid[selected]
  
  resid=resid(model)[selected] # eff$partial.residuals.raw[selected]

  ylim=c(min(c(preds$fit,y)),max(c(preds$fit,y)))
  #aspectr=aspectr*c(1,1,diff(range(y))/diff(range(ylim)))
  
  x1lim=extendrange(x1lim,f=0.01)
  x2lim=extendrange(x2lim,f=0.01)
  zlim=extendrange(zlim,f=0.1)

  open3d(zoom=zoom,userMatrix=userMatrix,windowRect=windowRect,antialias=8)
  
  if (residuals) { plot3d(x=x1,y=x2,z=y,type="s", # or z=y+resid
                          col=col.palette[clip((y-min(ylim))*(length(col.palette)-1)/diff(range(ylim))+1,1,length(col.palette))],
                          size=cex,aspect=aspectr,xlim=x1lim,ylim=x2lim,zlim=zlim,forceClipregion=TRUE,
                          xlab=x1lab,ylab=x2lab,zlab=ylab,lit=lit,
                          alpha=pointsalpha,point_antialias=TRUE,
                          line_antialias=TRUE,shininess=75) } else { 
                   plot3d(x=x1,y=x2,z=y,type="n", # z=y
                          col=col.palette[clip((y-min(ylim))*(length(col.palette)-1)/diff(range(ylim))+1,1,length(col.palette))],
                          size=cex,aspect=aspectr,xlim=x1lim,ylim=x2lim,zlim=zlim,forceClipregion=TRUE,
                          xlab=x1lab,ylab=x2lab,zlab=ylab) }
  
  ypred=matrix(preds[,"fit"],npp,npp)
  
  cols=col.palette[clip((ypred-min(ylim))*(length(col.palette)-1)/diff(range(ylim))+1,1,length(col.palette))]
  
  persp3d(x=unique(preds[,plotx1]),y=unique(preds[,plotx2]),z=ypred,color=cols, alpha=0.6, xlim=x1lim,ylim=x2lim,zlim=zlim, lit=lit, back="lines",add=TRUE)

  if ((interval!="none")&("upr" %in% names(preds))) { persp3d(x=unique(preds[,plotx1]),
                                                              y=unique(preds[,plotx2]),
                                  z=matrix(preds[,"lwr"],npp,npp),color=confcol,xlim=x1lim,ylim=x2lim,zlim=zlim, 
                                  alpha=confalpha, lit=lit, back="lines",add=TRUE)
                                                     persp3d(x=unique(preds[,plotx1]),
                                                             y=unique(preds[,plotx2]),
                                  z=matrix(preds[,"upr"],npp,npp),color=confcol,xlim=x1lim,ylim=x2lim,zlim=zlim,
                                  alpha=confalpha, lit=lit, back="lines",add=TRUE) }
  
  if (droplines) segments3d(x=rep(x1,each=2),y=rep(x2,each=2),
                            z=matrix(t(cbind(y,fitted(model)[selected])),nc=1),col=segcol,  
                            xlim=x1lim,ylim=x2lim,zlim=zlim,lty=2,lwd=lwddrop,alpha=segalpha)
  # TO DO: maybe plot residuals rather than raw data points?
  }






# n=10000
# age=rnorm(n,mean=40,sd=5)
# height=rnorm(n,mean=180,sd=7)
# sex=as.factor(c(rep("FEMALE",n/2),rep("MALE",n/2)))
# weight=-95+0.8*age+0.004*height^2+(as.numeric(sex)-1)*20+rnorm(n,mean=0,sd=7)
# bmi=weight/((height/100)^2)
# sbp=30+1.8*age+2.1*bmi-0.035*age*bmi+(as.numeric(sex)-1)*10+rnorm(n,mean=0,sd=5)
# mydata=data.frame(age,height,weight,bmi,sbp,sex)
# 
# fit1=lm(log(sbp)~log(age)*bmi,data=mydata)  # FIX
# plotfit3d(fit1, plotx1 = "age", plotx2 = "bmi",cex=0.6)
# plotfit3d(fit1, plotx1 = "age", plotx2 = "bmi",cex=0.5,interval="confidence")
# plotfit3d(fit1, plotx1 = "age", plotx2 = "bmi",cex=0.5,interval="prediction")
# 
# fit1=lm(sbp~age*bmi+sex,data=mydata)
# plotfit3d(fit1, plotx1 = "age", plotx2 = "bmi",cex=0.6, selection=list(sex="MALE"))
# plotfit3d(fit1, plotx1 = "age", plotx2 = "bmi",cex=0.6, selection=list(sex="FEMALE"))



# mydata2=
# # standard rockchalk plotPlane function
# plotPlane(fit1, plotx1 = "age", plotx2 = "bmi", theta=-45, phi=20, drawArrows = T, npp = 5, ticktype = "detailed",pch=16,pcex=0.5,pcol=adjustcolor("blue",alpha.f=0.4))
# # EXAMPLES OF ALTERNATIVE DISPLAY METHODS
# plotfit3d(fit1, plotx1 = "age", plotx2 = "bmi",cex=0.6)
# plotfit3d(fit1, plotx1 = "age", plotx2 = "bmi",cex=0.5,interval="confidence")
# plotfit3d(fit1, plotx1 = "age", plotx2 = "bmi",cex=0.5,interval="prediction",segcol="black")
# plotfit2d(fit1,plotx="age",ploty="bmi")
# plotfit2d(fit1,plotx="age",ploty="bmi",contours=F,legend=T)
# 
# 
# data=data.frame(age=c(530.6071,408.6071,108.0357,106.6071,669.4643,556.4643,665.4643,508.4643,106.0357),
#                 hrs=c(792.10,489.70,463.00,404.15,384.65,358.15,343.85,342.35,335.25),
#                 charges=c(3474.60,1247.06,1697.07,1676.33,1701.13,1630.30,2468.83,3366.44,2876.82))
# library(MASS)
# fit1=rlm( charges~age+hrs+age*hrs, data);
# plotPlane(fit1, plotx1 = "age", plotx2 = "hrs", theta=-45, phi=20, drawArrows = T, npp = 5, ticktype = "detailed",pch=16,pcex=0.5,pcol=adjustcolor("blue",alpha.f=0.4))
# plotfit3d(fit1, plotx1 = "age", plotx2 = "hrs")
# plotfit3d(fit1, plotx1 = "age", plotx2 = "hrs",interval="confidence")
# plotfit3d(fit1, plotx1 = "age", plotx2 = "hrs",interval="prediction")
# plotfit2d(fit1,plotx="age",ploty="hrs",contours=T,legend=T)