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
row.ref <-  match(quad$Y, uy)

#replicates the values in the object
all.vec <-  rep(NA, max(row.ref)*max(col.ref))

vec.ref <-  (col.ref - 1)*max(row.ref) + row.ref

all.vec[vec.ref] = 1

Sydney.mask <- matrix(all.vec, max(row.ref), max(col.ref),
                     dimnames = list(uy, ux))

Sydney.win <-  as.owin(im(Sydney.mask, xcol = ux, yrow = uy))



########## Make point pattern and quadrature scheme ##########

#'ppp creates an object of class "ppp" representing a point pattern dataset in the two-dimenstional plane.
#'Creates points at coordinate x, y, in window "Sydney.win, Check ensures that all points are within the window
#'which is unneccessary in this case but should normally be done. 
ppp.dat <-  ppp(X, Y, window = Sydney.win, check = FALSE)

#Does the same as above, except for quadrature points
quads <-  ppp(quad$X, quad$Y, window = Sydney.win)

#'Generates a quadrature scheme (object of class "quad") from point patters of data and dummy points
#'data: the observed data point pattern (must be object of class "ppp")
#'dummy: the pattern of dummy points for the quadrature (must be object of class "ppp)
#'method: the name of the method for calculating quadrature wights: either "grid" or "dirichlet"
#'        followed by parameters of the weighting method. In this case, we selected "grid" followed 
#'        commands that divide the quadrature region into an ntile[1]byntile[2] grid of rectangular tiles
#'        the weight for each point is then the area of a tile divided by the number of quadrature points 
#'        in that tile
Q <- quadscheme(data = ppp.dat, dummy = quads, method = "grid",
               ntile = c(nx, ny), npix = c(nx, ny))



########## Setting up covariate lists ##########
#' cbind takes a sequence of arguments and combines them by columns or rows
#' poly returns polynomials of degree 1 to degree over the specified set of points
#' a second degree polynomial is known as a quadratic polynomial
#' raw: if true, use raw and not orthogonal polynomials
X.des <-  cbind(poly(quad$FC, quad$MNT, quad$MXT, quad$Rain, degree = 2,
                    raw = TRUE), poly(sqrt(quad$D.Main), sqrt(quad$D.Urb), degree = 2,
                    raw = TRUE), quad$soil)

#Creates an empty object, will populate in the next steps.
int.list <- list()

#'for(variable in vector) starts a loop that will run through for each iteration
#'             all.vec = rep(NA, max(row.ref)*max(col.ref))
#'             vec.ref = (col.ref - 1)*max(row.ref) + row.ref
#'             all.vec[vec.ref] = X.des[,i]  
#'             int.list[[i]] = im(matrix(all.vec, max(row.ref), max(col.ref),
#'             dimnames = list(uy, ux)), xcol = ux, yrow = uy)

#'dim: retrieves or sets the dimension of an object
#'
#'Change a few components of code tomorrow to try to understand better
for (i in 1:dim(X.des)[2])
{
all.vec = rep(NA, max(row.ref)*max(col.ref))
vec.ref = (col.ref - 1)*max(row.ref) + row.ref
all.vec[vec.ref] = X.des[,i]
int.list[[i]] = im(matrix(all.vec, max(row.ref), max(col.ref),
dimnames = list(uy, ux)), xcol = ux, yrow = uy)
}

#'Names: sets the names of an object
#'Paste: concatenates vectors after converting to character
#'      sep= a character string to separate the terms
names(int.list) <- paste("V", 1:dim(X.des)[2], sep = "")

#'Creates object pred.list with distance based covariates set to zero for prediction of intensity
pred.list <- int.list

#'Will be used in the loop to tell which columns in pred.list to set to 0
set.0 <- 15:19

#'Lopp that sets 15:19 to 0 in pred.list
for (v in set.0)
{
pred.list[[v]]$v = 0*pred.list[[v]]$v
}



########## Fit Poisson point process model ##########

#'as.formula creates the formula for the IPPM
#'          ~ is the response 
#'          paste(names(int.list)) lets all combinations run 
int.form = as.formula(paste("~", paste(names(int.list), collapse = "+")))

#' ppm fits a point process model to an observed point pattern
#'        Q= describes the spatial model to be fitted
#'        trend=describes the spatial trend of the model
ft.int = ppm(Q, trend = as.formula(int.form), covariates = int.list)


