##############################################
#### Read in the data and exploratory analysis
##############################################
## Read in and compute the 
observed <- read.csv(file="coloncancer observed.csv", row.names=1)
expected <- read.csv(file="coloncancer expected.csv", row.names=1)
Y <- as.matrix(observed[ ,-1])
E <- as.matrix(expected)
K <- nrow(Y)
N <- ncol(Y)
SMR <- data.frame(Y / E, PHD=observed$PHD)
colnames(SMR) <- c("SMR2001","SMR2002", "SMR2003", "SMR2004", "SMR2005", "SMR2006", "SMR2007", "SMR2008", "SMR2009","SMR2010", "PHD")

## Read in the shapefile
shp <- read.shp(shp.name="County2008.shp")
dbf <- read.dbf(dbf.name="County2008.dbf")
dbf2 <- dbf
dbf2$dbf <- dbf$dbf[ ,c(5, 1:4, 6:18)]

## Re-order the data and shapefile to match
reorder <- rep(NA, K)
for(r in 1:K)
{
  temp1 <- dbf2$dbf[r,1]
  reorder[r] <- which(row.names(observed)==temp1)
}
SMR2 <- SMR[reorder, ]
Y2 <- Y[reorder, ]
E2 <- E[reorder, ]
Y <- Y2
E <- cbind(E2[ ,1], E2[ ,1], E2[ ,1], E2[ ,1], E2[ ,1], E2[ ,1], E2[ ,1], E2[ ,1], E2[ ,1], E2[ ,1])

## Create the combined spatial data object
SMRspatial <- combine.data.shapefile(SMR2, shp, dbf2)
coords <- coordinates(SMRspatial)


## Create the public health regions
shp.ph <- read.shp(shp.name="PH2008_alpha_ordered2.shp")
dbf.ph <- read.dbf(dbf.name="PH2008_alpha_ordered2.dbf")
n.ph <- length(shp.ph$shp)
ph.boundary <- shp.ph$shp[[1]]$points
for(j in 2:n.ph)
{
  ph.boundary <- rbind(ph.boundary, shp.ph$shp[[j]]$points)     
}
ph.spatial <- SpatialPoints(ph.boundary)


## Assess the relationships between PHD and SIR
SMR.vec <- as.numeric(as.matrix(SMR2[ ,1:10]))
PHD.all <- rep(SMR2$PHD, 10)
year <- kronecker(1:10, rep(1,159))
model <- lm(SMR.vec~factor(PHD.all) + factor(year))
summary(model)
anova(model)
boxplot(SMR.vec~PHD.all, xlab="PHD", ylab="SIR")
round(tapply(SMR.vec,PHD.all,median),3)



## Plot the SMR maps
range <- c(0, 0.5, 1, 1.5, 2, 6.6)
n.col <- length(range)-1
boundary <- list("sp.points", ph.spatial, col="white", pch=19, cex=0.2)
spplot(SMRspatial, zcol=c("SMR2001", "SMR2010"), sp.layout=list(boundary),
       scales=list(draw=TRUE), names.attr=c("SIR 2001", "SIR 2010"), 
       xlab="Longitude", ylab="Latitude", at=range, 
       col.regions=grey(seq(0.7, 0, length.out=n.col)), col="transparent")



## Create the neighbourhood matrix
W.nb <- poly2nb(SMRspatial, row.names = rownames(SMR2))
W.list <- nb2listw(W.nb, style="B")
W.mat <- nb2mat(W.nb, style="B")
