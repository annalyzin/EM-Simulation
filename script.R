library(deldir)

# set graphical parameters
default.par <- par(mar = c(5, 4, 4, 2) + 0.1, ask=FALSE)
par(mar=rep(0,4))
on.exit(par(default.par))


########################################
# Setup
########################################
set.seed(55)

# number of clusters
nc = 2

# set true cluster centers
xc <- sample(seq(0.2, 0.8, by=0.1), 2)
yc <- sample(seq(0.2, 0.8, by=0.1), 2)

# generate points based on true centers
np = 30 # no of points
xp <- NULL
yp <- NULL
for (i in 1:nc) {
  xp <- c(xp, rnorm(np, mean = xc[i], sd = 0.15))
  yp <- c(yp, rnorm(np, mean = yc[i], sd = 0.15))
}
# remove points outside plotting region
xp[xp < 0.05 | xp > 0.95] = NA
yp[yp < 0.05 | yp > 0.95] = NA



########################################
# Iteration
########################################
set.seed(100)

# set random pseudo-center
xpc <- sample(seq(0.2, 0.8, by=0.1), 2)
ypc <- sample(seq(0.2, 0.8, by=0.1), 2)

# plot points and intial pseudo-centers
dummy = deldir(0,0, rw=c(0,1,0,1))
plot(tile.list(dummy), pch=NA, close = TRUE)  # voronoi boundaries
points(xp,yp, pch=4, cex=2) # points
points(xpc,ypc, pch=16, asp=1, cex=2) # centers


par(ask=TRUE) # pause each time plot is generated
# start iteration
for (i in 1:5)  {

  # assign points to clusters
  z <- deldir(xpc, ypc, rw=c(0,1,0,1))
  w <- tile.list(z)


  # plot pseudo-centers and points with boundaries colored
  plot(w, pch=NA, fillcol = c("#33ccff", "#ff9999"), close = TRUE)  # voronoi boundaries
  points(xpc,ypc, pch=16, cex=2) # centers
  points(xp,yp, pch=4, cex=2) # points


  # re-assign cluster members
  cluster <-  NA
  boundary <-  (z$dirsgs$y2 - z$dirsgs$y1) * xp + z$dirsgs$y1
  cluster[yp > boundary] = 2
  cluster[yp <= boundary] = 1

  # re-locate cluster center
  xpcnew <- xpc
  ypcnew <- ypc

  for (i in 1:2)  {
    xpcnew[i] <- mean(xp[cluster==i], na.rm = T)
    ypcnew[i] <- mean(yp[cluster==i], na.rm = T)
  }

  # plot new cluster center with old boundaries
  plot(w, pch=NA, fillcol = c("#33ccff", "#ff9999"), close = TRUE)  # voronoi boundaries
  points(xpcnew,ypcnew, pch=16, cex=2) # centers
  points(xp,yp, pch=4, cex=2) # points

  # stop the iteration if cluster centers have stabilized
  if (all(xpcnew != xpc) & all(xpcnew != xpc))  {
    xpc <- xpcnew
    ypc <- ypcnew
  } else break;

}
