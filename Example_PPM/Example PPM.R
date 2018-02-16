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

