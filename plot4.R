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
    png(file="plot4.png",width=480,height=480)
        par(mfrow = c(2, 2))
        # plot nr. 1
        plot(tbl$DateTime, tbl$Global_active_power, xlab="Weekday",
             ylab = "Global Active Power (kilowatts)",type="l")
        # plot nr. 2
        plot(tbl$DateTime, tbl$Voltage, ylab="Voltage",type="l")
        # plot nr. 3
        plot(tbl$DateTime, tbl$Sub_metering_1, xlab="",
             ylab = "Global Active Power (kilowatts)",type="l",col="black")
        lines(tbl$DateTime,tbl$Sub_metering_2,type="l",col="red")
        lines(tbl$DateTime,tbl$Sub_metering_3,type="l",col="blue")
        legend("topright", lty=c(1,1),
               c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
               col=c("black","red", "blue"))
        # plot nr. 4
        plot(tbl$DateTime, tbl$Global_reactive_power,xlab="Weekday",
             ylab="Global Reactive Power",type="l")
    dev.off()
}