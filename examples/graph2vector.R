library(ggplot2)
x=qplot(Sepal.Length, Petal.Length, data = iris, color = Species, 
      size = Petal.Width, alpha = I(0.7))
x
graph2svg(x=x,file="ggplot2_plot.svg")
graph2pdf(x=x,file="ggplot2_plot.pdf")
graph2eps(x=x,file="ggplot2_plot.eps")
\dontrun{
  # use active graph instead of passing plot as object
  graph2pdf(aspectr=1.7)
}
