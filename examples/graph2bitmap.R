# Create a file name
dir <- tempdir()
filen <- paste0(dir,"ggplot")

# Generate graphical output
library(ggplot2)
library(datasets)
x=qplot(Sepal.Length, Petal.Length, data = iris, 
        color = Species, size = Petal.Width, alpha = I(0.7))
plot.fun <- function(){
  print(qplot(Sepal.Length, Petal.Length, data = iris, 
              color = Species, size = Petal.Width, alpha = 0.7))
}

# There are 3 ways to use graph2bitmap():
### 1. Pass the plot as an object
graph2png(x=x, file=filen, dpi=400, height = 5, aspectr=4)
graph2tif(x=x, file=filen, dpi=400, height = 5, aspectr=4)
graph2jpg(x=x, file=filen, dpi=400, height = 5, aspectr=4) 
### 2. Get the plot from current screen device
\donttest{ # Because the example uses screen devices 
  x
  graph2png(file=filen, dpi=400, height = 5, aspectr=4)
  graph2tif(file=filen, dpi=400, height = 5, aspectr=4)
  graph2jpg(file=filen, dpi=400, height = 5, aspectr=4) 
}
### 3. Pass the plot as a function
\donttest{ # Because the example uses screen devices 
  graph2png(file=filen, fun=plot.fun, dpi=400, height = 5, aspectr=4)
  graph2tif(file=filen, fun=plot.fun, dpi=400, height = 5, aspectr=4)
  graph2jpg(file=filen, fun=plot.fun, dpi=400, height = 5, aspectr=4)
} 
