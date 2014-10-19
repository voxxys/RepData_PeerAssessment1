data <- read.csv("activity.csv")

datacast <- cast(data,interval~date,value='steps')

daysum <- apply(datacast[,2:62],2,function(x) sum(x,na.rm=TRUE))
hist(ap2,10)

mean(daysum)
median(daysum)





intmeans <- apply(ca[,2:62],1,function(x) mean(x,na.rm=TRUE))

#?data[is.na(data)] <- 0
ress <- split(data,data$date)

aggr <- rep(0,length(ress[[1]]$steps))



for (i in 1:length(ress))
aggr <- aggr + ress[[i]]$steps

aggr <- aggr/length(ress)

xax <- 1:length(aggr)

plot(xax,aggr,type="l")

data <- read.csv("activity.csv")

sum(is.na(data$steps))

newy <- rep(aggr,61)
data[is.na(data$steps),1] <- newy[is.na(data$steps)]




Sys.setlocale("LC_TIME", "English")

try <- factor(weekdays(as.POSIXct(data$date)))

lev = rep("weekday",7)
lev[which(levels(try)=="Saturday")] = "weekend"
lev[which(levels(try)=="Sunday")] = "weekend"
levels(try) <- lev

data[,4] <- try
ress <- split(data,data$date)

aggrwd <- rep(0,length(ress[[1]]$steps))
aggrwe <- rep(0,length(ress[[1]]$steps))

for (i in 1:length(ress)) {
  if(ress[[i]]$V4[1] == "weekday") {
    aggrwd <- aggrwd + ress[[i]]$steps
  } else if(ress[[i]]$V4[1] == "weekend") {
    aggrwe <- aggrwe + ress[[i]]$steps
  }
      
}

par(mfrow=c(2,1))
plot(xax,aggrwd,type="l")
plot(xax,aggrwe,type="l")

