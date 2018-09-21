x = y = seq(-10, 10, length = 20)
z = outer(x, y, function(x, y) x^2 + y^2)
rgl::persp3d(x, y, z, col = 'lightblue')
rgl2png()
