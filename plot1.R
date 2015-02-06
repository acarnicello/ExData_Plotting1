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

#make histogram of the Golbal Active Power column of the filtered retrieved information
plot1 <- function(file)
{ 
    #open png device 
    png(file="plot1.png")
    
    #call fixData to retrieve, filter, and format data from file
    data <- fixData(file)
    
    #create histogram with red color and custiomized title and x label
    hist(data$Global_active_power, col="red", main="Global Active Power", xlab="Global Active Power (kilowatts)")
    
    #turn off png device
    dev.off()
   
}