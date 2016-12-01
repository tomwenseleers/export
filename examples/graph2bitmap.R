library(ggplot2)
x=qplot(Sepal.Length, Petal.Length, data = iris, 
        color = Species, size = Petal.Width, alpha = I(0.7))
x
graph2png(x=x,dpi=400)
graph2tif(x=x)
graph2jpg(x=x)

\dontrun{
# use active graph instead of passing plot as object
graph2png(aspectr=1.7)
}
