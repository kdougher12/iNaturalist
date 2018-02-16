library(spatstat)

############Setting up the study window##############
#Contains x + y
load("/Users/kdougherty8/Box/Github/iNaturalist/iNaturalist_Github/Example_PPM/Eucalyptus_sparsifolia_Atlas_2012.RData")

#Contains quad
load("/Users/kdougherty8/Box/Github/iNaturalist/iNaturalist_Github/Example_PPM/Quad100m.RData")

#Sorts quad x + y values low to high
ux <- sort(unique(quad$X))
uy <- sort(unique(quad$Y))

#Gives the length of the vector containing quads
nx <- length(ux)
ny <- length(uy)

#Matches quads to extent of study area
col.ref <-  match(quad$X, ux)
row.ref = match(quad$Y, uy)

#replicates the values in the object
all.vec = rep(NA, max(row.ref)*max(col.ref))

vec.ref = (col.ref - 1)*max(row.ref) + row.ref

all.vec[vec.ref] = 1

Sydney.mask = matrix(all.vec, max(row.ref), max(col.ref),
                     dimnames = list(uy, ux))

Sydney.win = as.owin(im(Sydney.mask, xcol = ux, yrow = uy))



########## Make point pattern and quadrature scheme ##########

#'ppp creates an object of class "ppp" representing a point pattern dataset in the two-dimenstional plane.
#'Creates points at coordinate x, y, in window "Sydney.win, Check ensures that all points are within the window
#'which is unneccessary in this case but should normally be done. 
ppp.dat = ppp(X, Y, window = Sydney.win, check = FALSE)

#Does the same as above, except for quadrature points
quads = ppp(quad$X, quad$Y, window = Sydney.win)

#'Generates a quadrature scheme (object of class "quad") from point patters of data and dummy points
#'data: the observed data point pattern (must be object of class "ppp")
#'dummy: the pattern of dummy points for the quadrature (must be object of class "ppp)
#'method: the name of the method for calculating quadrature wights: either "grid" or "dirichlet"
#'        followed by parameters of the weighting method. In this case, we selected "grid" followed 
#'        commands that divide the quadrature region into an ntile[1]byntile[2] grid of rectangular tiles
#'        the weight for each point is then the area of a tile divided by the number of quadrature points 
#'        in that tile
Q = quadscheme(data = ppp.dat, dummy = quads, method = "grid",
               ntile = c(nx, ny), npix = c(nx, ny))
Q

