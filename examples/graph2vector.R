library(ggplot2)
qplot(Sepal.Length, Petal.Length, data = iris, color = Species, 
      size = Petal.Width, alpha = I(0.7))
graph2svg(file="ggplot2_plot.svg")
graph2pdf(file="ggplot2_plot.pdf")
graph2eps(file="ggplot2_plot.eps")
# pass plot as object
x=qplot(Sepal.Length, Petal.Length, data = iris, color = Species, 
        size = Petal.Width, alpha = I(0.7))
graph2pdf(x=x, file="ggplot2_plot.pdf")

