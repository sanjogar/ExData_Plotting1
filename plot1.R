## Assignment 1. Part I

plot1 <- function(filename = "../household_power_consumption.txt") {
    
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
    
    #####
    # X variable
    GAP <- as.numeric(elepow$Global_active_power)

    #---------------
    # Starting the histogram plot in png format
    png(filename = "plot1.png", width = 480, height = 480, units = "px",
         bg = "white")
    hist(GAP, xlab = "Global Active Power (kilowatts)", 
         main = "Global Active Power", col = "red", ylim = c(0, 1200))
    dev.off()
    #---------------
}