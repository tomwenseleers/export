# export of ggplot2 plot
library(ggplot2)
qplot(Sepal.Length, Petal.Length, data = iris, color = Species, 
      size = Petal.Width, alpha = I(0.7))
graph2ppt(file="ggplot2 plot.pptx", aspectr=1.7)
# add 2nd slide with same graph in different aspect ratio
graph2ppt(file="ggplot2 plot.pptx", aspectr=1.3, append=T) 
# add 3d slide with same graph with fixed width & height
graph2ppt(file="ggplot2 plot.pptx", width=6, height=5, append=T) 

graph2doc(file="ggplot2 plot.docx", aspectr=1.7) 

graph2html(file="ggplot2 plot.html", aspectr=1.7) 

# pass plot as a ggplot2 object
p=qplot(Sepal.Length, Petal.Length, data = iris, color = Species, 
      size = Petal.Width, alpha = I(0.7))
graph2ppt(obj=p,file="ggplot2 plot.pptx", aspectr=1.7) # works OK


# export of lattice plot
library(lattice)
library(effects)
fit=lm(prestige ~ type + income*education, data=Prestige)
plot(Effect(c("income", "education"), fit, partial.residuals=TRUE),multiline=T, 
     span=1, show.fitted=TRUE, ci.style="bands")
graph2ppt(file="effect plot.pptx")

# pass plot as object
p=plot(Effect(c("income", "education"), fit, partial.residuals=TRUE),multiline=T, 
     span=1, show.fitted=TRUE, ci.style="bands")
graph2ppt(obj=p,file="boxplot.pptx") # works OK


# example export of base R plot
boxplot(mpg~cyl,data=mtcars, main="Car Milage Data",xlab="Number of Cylinders", 
ylab="Miles Per Gallon",col="cyan2")
graph2ppt(file="boxplot.pptx")

# passing it as an object does not work
p=boxplot(mpg~cyl,data=mtcars, main="Car Milage Data",xlab="Number of Cylinders", 
ylab="Miles Per Gallon",col="cyan2")
graph2ppt(obj=p,file="boxplot.pptx") # this does not work

# passing it as a function does work
f=function() boxplot(mpg~cyl,data=mtcars, main="Car Milage Data",col="cyan2")
graph2ppt(fun=f, file="boxplot.pptx", aspectr=1.3) # this does work


# heatmap example
heatmap(as.matrix(eurodist))
graph2ppt(file="heatmap.pptx")
