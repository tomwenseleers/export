# export of ggplot2 plot
library(ggplot2)
x=qplot(Sepal.Length, Petal.Length, data = iris, color = Species, 
      size = Petal.Width, alpha = I(0.7))
x
graph2ppt(x=x) # export graph with current graphics window width & height
graph2ppt(x=x,width=9,aspectr=sqrt(2)) # export graph with A4 aspect ratio
graph2ppt(x=x,vector.graphic=FALSE,width=9,aspectr=sqrt(2)) # export as 300 dpi PNG bitmap

\dontrun{
# use active graph instead of passing plot as object
graph2ppt(file="ggplot2_plot.pptx", aspectr=1.7)
# add 2nd slide with same graph in A4 aspect ratio
graph2ppt(file="ggplot2_plot.pptx", aspectr=sqrt(2), append=TRUE) 
# add 3d slide with same graph in A4 aspect ratio with a width of 9 inches
graph2ppt(file="ggplot2_plot.pptx", width=10, aspectr=sqrt(2), append=TRUE) 
# add 4th slide with same graph with fixed width & height
graph2ppt(file="ggplot2_plot.pptx", width=6, height=5, append=TRUE)
# add 5th slide with page-filled version of same graph
graph2ppt(file="ggplot2_plot.pptx", margins=0, upscale=TRUE, append=TRUE) 

# export to Word
graph2doc(aspectr=1.7) 
graph2doc(paper="A3", orient="portrait", aspectr=1.7) 

# export of lattice plot
library(lattice)
#library(effects)
fit <- lm(prestige ~ type + income*education, data=Prestige)
plot(effects::Effect(c("income", "education"), fit),multiline=TRUE, 
     span=1, show.fitted=TRUE, ci.style="bands")
graph2ppt(file="effect_plot.pptx")

# pass plot as object
x=plot(effects::Effect(c("income", "education"), fit),multiline=TRUE, 
     span=1, show.fitted=TRUE, ci.style="bands")
graph2ppt(x=x,file="effect_plot.pptx", append=TRUE)


# example export of base R plot
boxplot(mpg~cyl,data=mtcars,col="cyan2")
graph2ppt(file="boxplot.pptx")

# passing it as an object does not work
# p=boxplot(mpg~cyl,data=mtcars,col="cyan2")
# graph2ppt(obj=p,file="boxplot.pptx") # this does not work

# passing it as a function does work
f=function() boxplot(mpg~cyl,data=mtcars,col="cyan2")
graph2ppt(fun=f, file="boxplot.pptx", aspectr=1.3, append=TRUE)


# heatmap example
heatmap(as.matrix(eurodist))
graph2ppt(file="heatmap.pptx")

}
