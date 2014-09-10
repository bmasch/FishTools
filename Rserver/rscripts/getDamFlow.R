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

getDamFlow1 <- function(dam.data){
  #get each date
  datelist <- list()
  for(i in 1:14){ 
     line1 <- getDamLine(dam.data[6+i])
     line2 <- getDamLine(dam.data[27+i])
     line3 <- getDamLine(dam.data[47+i])
     list1 <- list()
     for(j in 1:7){
       temp <- list(id=j,flow=as.numeric(line1[2*j]),spill=as.numeric(line1[2*j+1]))
        list1[[j]] <- temp
     }
     for(j in 1:7){
       temp <- list(id=j+7,flow=as.numeric(line2[2*j]),spill=as.numeric(line2[2*j+1]))
        list1[[j+7]] <- temp
     }
     for(j in 1:4){
       temp <- list(id=j+14,flow=as.numeric(line3[2*j]),spill=as.numeric(line3[2*j+1]))
        list1[[j+14]] <- temp
     }
   temp2 <- list(date=line1[1],dams=list1)
   datelist[[i]] <- temp2
  }
  return(datelist)
}
getDamFlow <- function(dam.data){

name <- c("Grand Coulee Dam","Chief Joseph Dam","Wells  Dam","Rocky Reach  Dam","Rock Island Dam","Wanapum Dam","Priest Rapids Dam","Dworshak Dam","Hells Canyon Dam","Lower Granite Dam","Little Goose Dam","Lower Monumental Dam","Ice Harbor Dam","McNary Dam","John Day Dam","The Dalles Dam","Bonneville Dam")

operator <- c("Bureau of Reclamation","USACE Seattle District","Douglas County PUD","Chelan County PUD","Chelan County PUD","Grant County PUD","Grant County PUD","USACE Walla Walla District","Idaho Power Company","USACE Walla Walla District","USACE Walla Walla District","USACE Walla Walla District","USACE Walla Walla District","USACE Walla Walla District","USACE Portland District","USACE Portland District","USACE Portland District")

lat <- c(47.956914,47.995221,47.947336,47.532302,47.342956,46.876149,46.645048,46.515492,45.243673,46.661806,46.585452,46.563186,46.248669,45.933805,45.714265,45.61391,45.644569)

lng <- c(-118.982241,-119.641926,-119.864649,-120.298342,-120.094386,-119.97288,-119.910469,-116.295797,-116.700408,-117.42777,-118.027285,-118.538905,-118.879895,-119.298113,-120.691976,-121.133753,-121.940884)

  #get each date
  lines <- rep("",14)
  for(i in 1:14) {
     lines[i] <- getLine(dam.data[6+i])
     lines[i+14] <- getLine(dam.data[27+i])
     lines[i+28] <- getLine(dam.data[47+i])
  }

   lines2 <- rep("",14)
   for(i in 1:14){
      lines2[i] <- paste(lines[i],lines[i+14],lines[i+28],sep=",")
   }
   lines2 <- strsplit(lines2,split=",")
   
   

   #remove extra dates, populate datelist
   datelist <- rep("",14)
   for(k in 1:14) {
      lines2[[k]][19] <- lines2[[k]][20]
	  lines2[[k]][20] <- "0.0"
      lines2[[k]] <- (lines2[[k]][-29])[-16]
      datelist[k] <- lines2[[k]][1]
   }

  damlist <- list()
  for(j in 1:17){ 
       datalist <- list()
       for(k in 1:14){
            temp <- list(flow=as.numeric(lines2[[k]][2*j]),spill=as.numeric(lines2[[k]][2*j+1]))
            datalist[[k]] <- temp
       }
       damlist[[j]] <- list(id=j,name=name[j],operator=operator[j],coordinates=c(lat[j],lng[j]),data=datalist)
   }
  

  return(list(dates=datelist,dams=damlist))
}

getDamLine <- function(line){
   line <- gsub('\\s{1,}',',',line)
   line <- unlist(strsplit(line,split=","))
   return(line)
}

#get line as list
getLine <- function(line){
   line <- gsub('\\s{1,}',',',line)
   return(line)
}

doGet = function(){
dam.data <- readLines("http://www.fpc.org/currentdaily/flowspil.txt")
cat("flowdata=")
cat(toJSON(getDamFlow(dam.data)))
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