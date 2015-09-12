## Assignment 1. Part III

plot3 <- function(filename = "../household_power_consumption.txt") {
    
    # Reading only the first column to extract the indexes we are interested:
    command <- paste("cut -f1 -d';'", filename, sep=" ")
    colDat <- read.csv(pipe(command), colClasses = "character")
    
    # Finding the actual indexes for the two days:
    twodays <- c("2007-02-01","2007-02-02") # 2 days date we are interested on
    days <- strptime(twodays, "%F")         # Write them at the specific format
    Dates <- strptime(colDat$Date, "%d/%m/%Y")
    
    # Selecting the row with the right dates:
    dat2days <- which(Dates == days[1] | Dates == days[2]) # 2880
    
    # Assigning the name of the columns
    namecol <- names(read.csv(file=filename, colClasses = "character", sep=";",
                              nrows=1))
    # Reading only the specific rows:
    elepow <- read.csv(file=filename, colClasses = "character", sep = ";",
                       skip = dat2days[1]-1, nrows = length(dat2days),
                       col.names = namecol, na.strings = "?")
    
    # Pasting the 1st and 2nd column to conver the entire time
    totime <- paste(elepow$Date, elepow$Time, sep = " ")
    
    # Y1, Y2, Y3 variables
    SM1 <- as.numeric(elepow$Sub_metering_1)
    SM2 <- as.numeric(elepow$Sub_metering_2)
    SM3 <- as.numeric(elepow$Sub_metering_3)
    
    # X variable
    WD <- strptime(totime, "%d/%m/%Y %T")
    
    #--------------- 
    # Starting png plot
    png(filename = "plot3.png", width = 480, height = 480, units = "px",
        bg = "white")
    plot(WD, SM1, ylab = "Energy sub metering", xlab = "", type = "l")
    lines(WD, SM2, col = "red")
    lines(WD, SM3, col = "blue")
    legend("topright", legend = c(namecol[7],namecol[8], namecol[9]), 
           lty = c(1,1,1), col = c("black", "red", "blue"))
    
    dev.off()
    #--------------- 
    
}