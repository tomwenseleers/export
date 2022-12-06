# Create a file name
filen <- tempfile(pattern = "ggplot") # or 
# filen <- paste("YOUR_DIR/ggplot")

# Generate graphical output
library(ggplot2)
library(datasets)
x=qplot(Sepal.Length, Petal.Length, data = iris, 
        color = Species, size = Petal.Width, alpha = I(0.7))
plot.fun = function(){
  print(qplot(Sepal.Length, Petal.Length, data = iris, 
              color = Species, size = Petal.Width, alpha = I(0.7)))
}

# There are 3 ways to use graph2office():
### 1. Pass the plot as an object
graph2ppt(x=x, file=filen) 
graph2doc(x=x, file=filen, aspectr=0.5) 
### 2. Get the plot from current screen device
if (interactive()) { 
  x
  graph2ppt(file=filen, width=9, aspectr=2, append = TRUE)
  graph2doc(file=filen, aspectr=1.7, append =TRUE) 
  # Note this requires a graphical device
}
### 3. Pass the plot as a function
if (interactive()) { 
  graph2ppt(fun=plot.fun, file=filen, aspectr=0.5, append = TRUE)
  graph2doc(fun=plot.fun, file=filen, aspectr=0.5, append = TRUE)
  # Note this requires a graphical device
} 

### Formatting options:
# Disable vectorized image export (export as a bitmap)
graph2ppt(x=x, file=filen, vector.graphic=FALSE, width=9, 
          aspectr=sqrt(2), append = TRUE) 
# Fill the slide with graph
graph2ppt(x=x, file=filen, margins=0, upscale=TRUE, append=TRUE) 
# etc...
