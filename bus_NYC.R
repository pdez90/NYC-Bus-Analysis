require(rgdal)
require(geosphere)
require(sp)
require(rgeos)

#15167 stops
busstops_nyc<-readOGR("/Users/priyankadesouza/Downloads/stops_bus_nyc_jan2017/", "stops_bus_nyc_jan2017")

#499 buses
busroutes_nyc<- readOGR("/Users/priyankadesouza/Downloads/routes_bus_nyc_jan2017/", "routes_bus_nyc_jan2017")

busstops_nyc<-spTransform(busstops_nyc, CRS("+init=epsg:4326"))
busroutes_nyc<-spTransform(busroutes_nyc, CRS("+init=epsg:4326"))

m1 <- gDistance(busstops_nyc, busroutes_nyc, byid=TRUE)
m.df<-as.data.frame(m1)
colnames(m.df)<-busstops_nyc$stop_name
rownames(m.df)<-busroutes_nyc$route_dir

#6371 km is the radius of the globe
m.df <-m.df*6371
#The next step has to be verified
m.df1<-ifelse(m.df<0.5,1,0)
m.df2<-ifelse(m.df<0.5, 0, 0)

Bustopperbus <- rowSums(m.df1)
BSB <- as.data.frame(Bustopsperbus)

Busesperbustop <-colSums(m.df1)
BBS <-as.data.frame(Busesperbustop)
BBS$stops<-colnames(m.df1)

#Indices of sorted array from lowest to highest
sortedBBS<-sort(BBS$Busesperbustop, index.return=TRUE)$ix
sortedBBS<-as.data.frame(sortedBBS)

#Adding the bus stop name to the datafrmae
for (i in 1:15167){
  sortedBBS[i,2]=BBS[sortedBBS[i,1], 2]
}

#Adding a coil to bus stops starting with the stops that have the highest number of buses
j=15167
sums =rowSums(m.df2)
sums1 <-as.data.frame(sums)
while (0 %in% sums1$sum && j>1) {
  m.df2[,sortedBBS[j,1]] = m.df1[,sortedBBS[j,1]]
  j=j-1
  sums=rowSums(m.df2)
  sums1 <-as.data.frame(sums)
  }

number_bs=colSums(m.df2)
number_bs<-as.data.frame(number_bs)
#Calculating the total no of bus stops that will be electrified
total=sum(number_bs>0)



