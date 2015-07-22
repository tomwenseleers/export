\dontrun{
library(ggplot2)
qplot(Sepal.Length, Petal.Length, data = iris, color = Species, 
      size = Petal.Width, alpha = I(0.7))
# this results in a compilable Latex file with sans font
graph2tex(file="ggplot2_plot1.tex")
# this results in a compilable Latex file with default document font
graph2tex(file="ggplot2_plot2.tex",font=NA)
# this results in Latex output that has to be pasted into another document
graph2tex2(file="ggplot2_plot3.tex",font=NA)
# pass plot as object
x=qplot(Sepal.Length, Petal.Length, data = iris, color = Species, 
      size = Petal.Width, alpha = I(0.7))
graph2tex(x=x,file="ggplot2_plot1.tex")
}
