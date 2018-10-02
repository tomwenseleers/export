# Create a file name
dir <- tempdir()
filen <- paste0(dir,"\\rgl")

# Generate a 3D plot using 'rgl'
x = y = seq(-10, 10, length = 20)
z = outer(x, y, function(x, y) x^2 + y^2)
rgl::persp3d(x, y, z, col = 'lightblue')

# Save the plot as a png
rgl2png(file = filen)
# Note that omitting 'file' will save in current directory
