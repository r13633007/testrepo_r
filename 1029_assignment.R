##########################
####### Mapping_1029 ####
##########################

### 1.0 Spatial data

# 1.1 Vector data (x,y)
# 1.2 Raster data (raw,column)

# 1.3 Simple representation of spatial data
#(1)
name <- LETTERS[1:10]
longitude <- c(-116.7, -120.4, -116.7, -113.5, -115.5,
               -120.8, -119.5, -113.7, -113.7, -110.7)
latitude <- c(45.3, 42.6, 38.9, 42.1, 35.7, 38.9,
              36.2, 39, 41.6, 36.9)
stations <- cbind(longitude, latitude)
stations
# Simulated rainfall data
set.seed(35) #random
precip <- round((runif(length(latitude))*10)^3)
precip

#(2)
psize <- 1 + precip/500
plot(stations, cex=psize, pch=20, col='red', main='Precipitation')
text(stations, name, pos=4) # add names to plot
breaks <- c(100, 250, 500, 1000)
legend.psize <- 1+breaks/500 # add a legend
legend("topright", legend=breaks, pch=20, pt.cex=legend.psize, col='red', bg='gray')

#(3) [polygon] closed gram
lon <- c(-116.8, -114.2, -112.9, -111.9, -114.2, -115.4, -117.7)
lat <- c(41.3, 42.9, 42.4, 39.8, 37.6, 38.3, 37.6)
x <- cbind(lon, lat)
plot(stations, main='Precipitation')
polygon(x, col='blue', border='light blue') #
lines(stations, lwd=3, col='red')
points(x, cex=2, pch=20)
points(stations, cex=psize, pch=20, col='red', main='Precipitation')

#(4) [data.frame] 
wst <- data.frame(longitude, latitude, name, precip)
wst


### 2.0 Reading and writing spatial data

# 2.1 Vector files
#(1) reading
install.packages("terra") #can read and write several raster file formats.
library(terra)
filename1 <- system.file("ex/lux.shp", package="terra")
basename(filename1)

# example using .shp file
s1 <- vect(filename1)
s1

#(2) Writing
library (geodata)
#TWN <- gadm(country="TWN", level=1, path=tempdir())
#TWN <- gadm(country="TWN", level=1, path="./data"
TWN <- vect('data/gadm/gadm41_TWN_1_pk.rds')
TWN
# check for ?gadm
# The RDS format is a binary file format, native to R. It has been part of R for many years, and provides a convenient method for saving R objects, including data sets. R also has two native data formats—Rdata (sometimes shortened to Rda) and Rds. These formats are used when R objects are saved for later use. Rdata is used to save multiple R objects, while Rds is used to save a single R object.

TWN <- vect('data/gadm/gadm41_TWN_1_pk.rds') # overwrite
outfile1 <- "data/shp_TWN.shp"
writeVector(TWN, outfile1, overwrite=TRUE)

# 2.2 Raster files
#(1) Reading raster data
f <- system.file("ex/logo.tif", package="terra")
basename(f)
r <- rast(f)
r
plot(r)
r2 <- r[[2]] #[[2]]select second layer
r2
ele <-elevation_30s("FR", path=tempdir())
ele
plot(ele)

#(2) Writing raster data
x <- writeRaster(ele, "data/ele.tif", overwrite=TRUE)
x

### 3.0 Coordinate Reference Systems

# 3.1 Coordinate Reference Systems (CRS)
#(1) Angular coordinates
#(2) Projections
#(3) Notation
s1
crs(s1)

# 3.2 Assigning CRS
ss <- s1
crs(ss) <- ""
crs(ss)
crs(ss) <- "+proj=longlat +datum=WGS84"
crs(ss)

# 3.3 Transforming vector data
newcrs <- "+proj=robin +datum=WGS84" #[proj]
rob <- terra::project(s1, newcrs)
rob
p2 <- terra::project(rob, "+proj=longlat +datum=WGS84")

# 3.4 Transforming raster data
#(1) 
r <- rast(xmin=-110, xmax=-90, ymin=40, ymax=60, ncols=40, nrows=40) 
values(r) <- 1:ncell(r)
r
plot (r)
newcrs 
#(2)
pr1 <- terra::project(r, newcrs) 
crs(pr1)
plot(pr1)
#(3)
x <- rast(pr1) 
res(x) <- 200000 # Set the cell size
pr3 <- terra::project(r, x)
pr3
plot(pr3)

### 4.1 Vector data manipulation
#1
TWN <- vect('data/gadm/gadm41_TWN_1_pk.rds')
plot(TWN, "NAME_1")
#2
url <- 'https://data.moi.gov.tw/MoiOD/System/DownloadFile.aspx?DATA=72874C55-884D-4CEA-B7D6-F60B0BE85AB0' #government open data website
path1 <- tempfile(fileext = ".zip")
if (file.exists(path1))  'file alredy exists' else download.file(url, path1, mode="wb")
zip::unzip(zipfile = path1,exdir = 'data')

Taiwan <- "data/COUNTY_MOI_1130718.shp" # Make SpatialVector
Taiwan <-vect(Taiwan)
Taiwan
plot(Taiwan, "COUNTYENG") # Make the new plot

sessionInfo() #traditional Chinese characters 
Sys.setlocale(category = "LC_ALL", "Chinese (Traditional)_Taiwan.950") #[Sys.setlocale]("LC_TIME", "English")

# 4.2 Basics
#(1) Geometry and attributes
d <- as.data.frame(Taiwan) #[as.data.frame]
head(d) 
g <- geom(Taiwan) #x,y
head(g)
g <- geom(Taiwan, wkt=TRUE) #text
substr(g, 1, 50)

#(2) Variables
Taiwan$COUNTYENG #list
Taiwan[,"COUNTYENG"] #vector

set.seed(0)
Taiwan$lets <- sample(letters, nrow(Taiwan)) #[letters]/[LETTERS]
head(Taiwan)

Taiwan$lets <- NULL #get rid of a variable
head(Taiwan)

#(3) Merge [merge]
dfr <- data.frame(County=Taiwan$COUNTYENG, Value=round(runif(length(Taiwan), 100, 1000)))
dfr <- dfr[order(dfr$County), ]
pm <- merge(Taiwan, dfr, by.x="COUNTYENG",by.y="County") #[merge]/[cbind]:know sequence
head(pm)

#(4) Records
i <- which(Taiwan$COUNTYENG == 'Taipei City') #Selecting rows 
g <- Taiwan[i,]
g
head(g)

# 4.3 Append and aggregate 
#(1) Append
z <- rast(Taiwan)
dim(z) <- c(2,2)
values(z) <- 1:4
names(z) <- 'Zone'
z <- as.polygons(z)
z

z1 <- z[1,]
z2 <- z[2,]
z3 <- z[3,]
z4 <- z[4,]
plot(Taiwan)
plot(z, add=TRUE, border='blue', lwd=5)
plot(z2, add=TRUE, border='red', lwd=2, col='red')

b <- rbind(Taiwan, z) #[rbind]
head(b)
tail(b)

#(2) Aggregate 
#1
Taiwan$region<-c(rep("Others",6), rep("North",3), rep("Others",2), "North", rep("Others",10))
pa <- aggregate(Taiwan, by='region')
za <- aggregate(z)
plot(za, col='light gray', border='light gray', lwd=5)
plot(pa, add=TRUE, col=rainbow(2), lwd=3, border='white')
#2 [dissolve]
Taiwan$region<-c(rep("Others",6), rep("North",3), rep("Others",2), "North", rep("Others",10))
pa <- aggregate(Taiwan, by='region',dissolve=FALSE) #
za <- aggregate(z, dissolve = FALSE) #
plot(za, col='light gray', border='dark gray', lwd=3)
plot(pa, add=TRUE, col=rainbow(2), lwd=2, border='white')
#3
zd <- disagg(pa)
zd

#(3) Overlay
#1
e1 <- erase(Taiwan,z1 )
e2 <- erase(e1,z3 )
e3 <- erase(e2,z4 )
plot(e3)
#2
i <- terra::intersect(Taiwan, z3)
plot(i)
#3
e <- ext(119, 123, 21, 26)
te <- crop(Taiwan, e)
plot(Taiwan)
plot(e, add=TRUE, lwd=3, col="red")
plot(te, col='light blue', add=TRUE)
plot(e, add=TRUE, lwd=3, border="blue")
#4 [union]
u <- terra::union(Taiwan, z)
set.seed(5)
plot(u, col=sample(rainbow(length(u))))
#5 [cover]
cov <- cover(Taiwan, z[c(1,3),])
plot(cov)
#6 [symdif] different
dif <- symdif(z,Taiwan)
plot(dif, col=rainbow(length(dif)))

#(4) Spatial queries

### 5.1 Raster data manipulation

### 6.1 Maps

# 6.2 SpatVector
n <- nrow(TWN)
plot(TWN, col=rainbow(n))


#############################
##### Practice 5.1 ##########
#############################

