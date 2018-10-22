# Create a file name
filen <- tempfile(pattern = "ggplot") # or 
# filen <- paste("YOUR_DIR/ggplot")

# Generate graphical output
library(ggplot2)
library(datasets)
x=qplot(Sepal.Length, Petal.Length, data = iris, 
        color = Species, size = Petal.Width, alpha = I(0.7))
plot.fun <- function(){
  print(qplot(Sepal.Length, Petal.Length, data = iris, 
              color = Species, size = Petal.Width, alpha = 0.7))
}

# There are 3 ways to use graph2vector():
### 1. Pass the plot as an object
graph2svg(x=x, file=filen, aspectr=2, font = "Times New Roman", 
          height = 5, bg = "white")
graph2pdf(x=x, file=filen, aspectr=2, font = "Arial",   
          height = 5, bg = "transparent")
graph2eps(x=x, file=filen, aspectr=2, font = "Arial",   
          height = 5, bg = "transparent")
### 2. Get the plot from current screen device
\donttest{ # Because the example uses screen devices 
  x
  graph2svg(file=filen, aspectr=2, font = "Arial",  
            height = 5, bg = "transparent")
  graph2pdf(file=filen, aspectr=2, font = "Times New Roman",   
            height = 5, bg = "white")
  graph2eps(file=filen, aspectr=2, font = "Times New Roman",   
            height = 5, bg = "white")
}
### 3. Pass the plot as a function
\donttest{ # Because the example uses screen devices 
  graph2svg(file=filen, fun = plot.fun, aspectr=2, font = "Arial",  
            height = 5, bg = "transparent")
  graph2pdf(file=filen, fun=plot.fun, aspectr=2, font = "Arial",   
            height = 5, bg = "transparent")
  graph2eps(file=filen, fun=plot.fun, aspectr=2, font = "Arial",   
            height = 5, bg = "transparent")
} 

