
# Create a file name
dir <- tempdir()
filen <- paste0(dir,"\\ggplot")

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
\donttest{ # Because the example uses screen devices 
  x
  graph2ppt(file=filen, width=9, aspectr=2, append = TRUE)
  graph2doc(file=filen, aspectr=1.7, append =TRUE) 
}
### 3. Pass the plot as a function
\donttest{ # Because the example uses screen devices 
  graph2ppt(fun=plot.fun, file=filen, aspectr=0.5, append = TRUE)
  graph2doc(fun=plot.fun, file=filen, aspectr=0.5, append = TRUE)
} 

### Formatting options:
# Disable vectorized image export (export as a bitmap)
graph2ppt(x=x, file=filen, vector.graphic=FALSE, width=9, 
          aspectr=sqrt(2), append = TRUE) 
# Fill the slide with graph
graph2ppt(x=x, file=filen, margins=0, upscale=TRUE, append=TRUE) 
# etc...
