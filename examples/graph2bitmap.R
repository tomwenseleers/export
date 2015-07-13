library(ggplot2)

qplot(Sepal.Length, Petal.Length, data = iris, color = Species, size = Petal.Width, alpha = I(0.7))
graph2png()
graph2tif()
graph2jpg()
