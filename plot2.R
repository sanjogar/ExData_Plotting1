## Assignment 1. Part II

plot2 <- function(filename = "../household_power_consumption.txt") {
    
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
    
    # Y variable
    GAP <- as.numeric(elepow$Global_active_power)
    # X variable
    WD <- strptime(totime, "%d/%m/%Y %T")
   
    #--------------- 
    # Starting png plot
    png(filename = "plot2.png", width = 480, height = 480, units = "px",
        bg = "white")
    plot(WD, GAP, ylab = "Global Active Power (kilowatts)", xlab = "", 
         type = "l")
    dev.off()
    #---------------
    
}