#retrieves data from file
readData <- function(file)
{
    #make sure data.table package is installed and loaded
    
    if(!"data.table" %in% installed.packages()){install.packages("data.table")}
    
    require(data.table)
    
    #read file and assign it to Btab
    Btab <- fread(file, header=T, colClasses="character",na.strings="?", data.table=F)
    
    #return Btab
    Btab
}

#filters and changes data to correct format
fixData <- function(file)
{
    #retrieve data
    Btab <- readData(file)
    
    #make Date column date object
    Btab[,1] <- as.Date(Btab[,1],"%d/%m/%Y")
    
    #filter out all but observations from 2/1/2007 to 2/2/2007 and assign to ltab
    ltab <- subset(Btab, Date >="2007-02-01" & Date < "2007-02-03")
    
    #change time to "POSIXct" object and columns 3:9 to numeric
    ltab[,2] <- as.data.frame(strptime(paste(ltab[,1],ltab[,2]),"%Y-%m-%d %T"))
    ltab[3:9]<-sapply(ltab[,3:9], function(data) {data<-as.numeric(data)})
    
    #return ltab
    ltab
}

#make plot of Sub_metering over Time columns of filtered retrieved information
plot3 <- function(file)
{ 
    #open png device 
    png(file="plot3.png")
    
    #call fixData to retrieve, filter, and format data from file
    data <- fixData(file)
    
    #create blank plot of Sub_metering 1 through 3 over Time with a customized y label
    plot(x=rep(data$Time,3),y=c(data$Sub_metering_1,data$Sub_metering_2,data$Sub_metering_3), ylab="Energy sub metering", xlab= "",type='n')
    
    #add line connecting the datapoints for each Sub metering
    lines(data$Time, data$Sub_metering_1, col="black")
    lines(data$Time, data$Sub_metering_2, col="red")
    lines(data$Time, data$Sub_metering_3, col="blue")
    
    #create legend
    legend("topright",legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), lty=1,col = c("black","red","blue"))
    
    #turn off png device
    dev.off()
    
}