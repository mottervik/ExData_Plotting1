loadData <- function(){
    # we read the data
    tbl <- readData("household_power_consumption.txt")
    
    # we first create an additional Date and Time column...
    tbl$DateTime <- (paste(tbl$Date, tbl$Time, sep=" "))
    tbl$DateTime <- strptime(tbl$DateTime,"%d/%m/%Y %H:%M:%S", tz="US/Pacific")
    
    # ...which allows us to subset the data
    startTime <- as.Date("2007-02-01 00:00:00","%F %H:%M:%S", tz="US/Pacific")
    stopTime <- as.Date("2007-02-02 23:59:59","%F %H:%M:%S", tz="US/Pacific")
    subTable <- subset(tbl,as.Date(tbl$DateTime) >= startTime & as.Date(tbl$DateTime) <= stopTime)
    
    makePlot(subTable)
}

readData <- function(file){
    table <- read.csv(file, stringsAsFactors=FALSE, header=TRUE, sep=";", na.strings="?")
    return(table)
}

makePlot <- function(tbl){
    png(file="plot2.png",width=480,height=480)
        plot(tbl$DateTime, tbl$Global_active_power, xlab="",
             ylab = "Global Active Power (kilowatts)",type="l")
    dev.off()
}