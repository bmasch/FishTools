library(httr)
library(rjson)

time1=proc.time()[3]
warn=""

#needed for logging
getBrowserIndex = function(){
	nm = names(SERVER$headers_in)
	index=0
	for(i in 1:length(nm)){
		if(nm[i]=="User-Agent")index=i
	}
	return(index)
}

doGet = function(){
url1 <- "http://waterservices.usgs.gov/nwis/iv/?format=json,1.1&sites=12456500,13340000,12452990,12447200,12439500,12462500,13310700,13341050,13342500,13340600,13338500,12413500,14105700,12399500,12436500,12472800,12462600,12453700,12409000,14092500,14103000,13333000,13292000,14048000,14046500,13305000,13316500,13337000,13330000,12449950,12448500,12445000,13345000,13239000,13317000,13302500,13296500,13309220,13313000,13336500,13334300,13290450,13269000,12422500,12414500,14020000,14033500,14020850,12459000,12457000,14174000,14166000,12502500,12510500,12484500&parameterCd=00060"
wdata <- GET(url1)
wdata1 <- content(wdata)
wdata2 <- wdata1$value
#a list with data from each site
timeSeries <- wdata2$timeSeries
mdat <- getMean(currentMonth(),currentDay())
nd <- length(timeSeries)
cat('"id"|"name"|"lat"|"lng"|"flow"|"dateTime"|"mflow"|"mtemp"\n')
for(i in 1:nd){
cat(timeSeries[[i]]$sourceInfo$siteCode[[1]]$value)
cat("|")
cat('"')
cat(timeSeries[[i]]$sourceInfo$siteName)
cat('"|')
cat(timeSeries[[i]]$sourceInfo$geoLocation$geogLocation$latitude)
cat('|')
cat(timeSeries[[i]]$sourceInfo$geoLocation$geogLocation$longitude)
cat('|')
cat(timeSeries[[i]][[3]][[1]]$value[[1]]$value)
cat('|"')
cat(timeSeries[[i]][[3]][[1]]$value[[1]]$dateTime)
cat('"|')
cat(mdat[mdat$id==timeSeries[[i]]$sourceInfo$siteCode[[1]]$value,]$flow)
cat('|')
cat(mdat[mdat$id==timeSeries[[i]]$sourceInfo$siteCode[[1]]$value,]$temperature)
cat('\n')
}

}

doGet1 = function(){
url1 <- "http://waterservices.usgs.gov/nwis/iv/?format=json,1.1&sites=12456500,13340000,12436500,12462600,12453700,12452990,14048000,12447200,12439500,12462500,13310700,13341050,13342500,13340600,13338500,12413500,14105700,12399500,12436500,12472800,12462600,12453700,12409000,14092500,14103000,13333000,13292000,14048000,14046500,13305000,13316500,13337000,13330000,12449950,12448500,12445000,13345000,13239000,13317000,13302500,13296500,13309220,13313000,13336500,13334300,13290450,13269000,12422500,12414500,14020000,14033500,14020850,12459000,12457000,14174000,14166000,12502500,12510500,12484500&parameterCd=00060"
wdata <- GET(url1)
wdata1 <- content(wdata)
wdata2 <- wdata1$value
#a list with data from each site
timeSeries <- wdata2$timeSeries
nd <- length(timeSeries)
name <- rep("",nd)
id <- rep(NA,nd)
lat <- rep(0,nd)
lng <- rep(0,nd)
flow <- rep(99999,nd)
dateTime <- rep("",nd)
for(i in 1:nd){
name[i] <- timeSeries[[i]]$sourceInfo$siteName
id[i] <- timeSeries[[i]]$sourceInfo$siteCode[[1]]$value
lat[i] <- timeSeries[[i]]$sourceInfo$geoLocation$geogLocation$latitude
lng[i] <- timeSeries[[i]]$sourceInfo$geoLocation$geogLocation$longitude
flow[i] <- timeSeries[[i]][[3]][[1]]$value[[1]]$value
dateTime[i] <- timeSeries[[i]][[3]][[1]]$value[[1]]$dateTime
}
wd <- data.frame(id=id,name=name,lat=lat,lng=lng,flow=flow,dateTime=dateTime)
time=proc.time()[3]-time1
options(width=200)
print.data.frame(wd,quote=TRUE,row.names=FALSE)
#b <- capture.output(wd) 
#c <- paste(b, "\n", sep="") 
#cat(c)	
  
}

getMean <- function(month,day){
  dat <- read.csv("/var/www/html/data/USGSDailyMeans.csv")
  dat <- dat[dat$day==day,]
  dat <- dat[dat$month==month,]
  return(dat)
}

currentDay <- function(){
  return(as.numeric(format(Sys.Date(), "%d")))
}

currentMonth <- function(){
  return(as.numeric(format(Sys.Date(), "%m")))
}

# Output starts here
setHeader(header='access-control-allow-origin',value='*')
setHeader(header='Expires',value='-1')
setHeader(header='Pragma',value='no-cache')
setContentType("text/html")

res=tryCatch(doGet(),
#res=tryCatch(test(),
 warning = function(war) {
}, error = function(err) {
  # error handler picks up where error was generated
	time=proc.time()[3]-time1  
	cat(err$message)
}, finally = {

}) # END tryCatch
DONE