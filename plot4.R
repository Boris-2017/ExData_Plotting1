## 25.06.2017: This is the R code for the Week 1 Project of Exploratory data Analysis course on Coursera.org.
## The script below operates on the "Individual household electric power consumption Data Set" from the UC Irvine Machine Learning Repository, ... 
## a popular repository for machine learning datasets, as made available  on the course web site https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
##
## The purpose of the scipt below is to reproduce the first graph provided with the assignment and save it as a .png file.
##


plot4 <- function() 
{
        
        ## Output:              None as such but a plot4.png file with the graph will be generated.
        ##                      Note that the script exits with an error if the data file is not found in the working directory.

        
        File_Name="household_power_consumption.txt"
        if (!file.exists(File_Name))
                stop(paste("File", File_Name, "not found. Execution aborted.", sep=" "))                
        
        # Data are semicolon-separated, with the header in the first row and NAs specified by "?". The first two columns are date and time, the rest are numerical
        Data=read.table(File_Name, sep=";", header=TRUE, colClasses=c("character", "character", rep("numeric", 7)), na.strings="?")
        # As per instructions, only interested in the data for 1 and 2 February, 2007
        Data=Data[(Data$Date=="1/2/2007") | (Data$Date=="2/2/2007"), ]
        Check_for_Inadmissible_Values_Numeric(Data$Global_active_power, 0, 1e6, "Values too large in column Global_active_power.")
        
        # Add an R-native date-time column
        Data$Date_and_Time=strptime(paste(Data$Date, Data$Time), format="%d/%m/%Y %H:%M:%S")
        
        # At least on my system, an existing file is not overwritten by the below commands, so delete it first
        File_Name="../04. Output/plot4.png"
        if (file.exists(File_Name))
                file.remove(File_Name)
        
        # Open the device
        png(File_Name, width=480, height=480)
        
        par(mfcol=c(2, 2))
        
        # Plot 2 from before: row 1, column 1        
        plot(Data$Date_and_Time, Data$Global_active_power, type="l", xlab="", ylab="Global Active Power (kilowatts)", main="")

        # Plot 3 from before: row 2, column 1
        
        # Plot the first graph in the format prescribed
        plot(Data$Date_and_Time, Data$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering", main="")
        # Now plot the other graphs in the formats prescribed
        lines(Data$Date_and_Time, Data$Sub_metering_2, col="red")
        lines(Data$Date_and_Time, Data$Sub_metering_3, col="blue")
        legend("topright", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col=c("black", "red", "blue"), lty=1)

        # Row 1, column 2
        plot(Data$Date_and_Time, Data$Voltage, type="l", xlab="datetime", ylab="Voltage", main="")
        
        # Row 2, column 2
        plot(Data$Date_and_Time, Data$Global_reactive_power, type="l", xlab="datetime", ylab="Global_reactive_power", main="")

        # Close the device, which saves the file to the disk
        dev.off()
        
}


## This function checks if inadmissible values are present, issues a warning if so and optionally replaces them with NAs.

Check_for_Inadmissible_Values_Numeric <- function (Values, Min, Max, Warning_Message, Replace_with_NA=TRUE)
{
        
        ## Input:
        ##      Values:                 Vector of values to check for admissibility
        ##      Min:                    Values must be >= Min to be admissible
        ##      Max:                    Values must be <= Max to be admissible        
        ##      Warning_Message:        String forming a part of the warning message if inadmissible values are found
        ##      Replace_with_NAs:       If TRUE, any inadmissible values are replaced with NA. 
        ##
        ## Output:                      Vector listing indices of any inadmissible values within Values. Also output as part of the warning message.
        
        
        Inadmissible_Value_Indices=which(!is.numeric(Values) | Values<Min | Values>Max)
        if (sum(Inadmissible_Value_Indices)>0)
                print(paste(Warning_Message, "Inadmissible values found at indices:", Inadmissible_Value_Indices))
        Inadmissible_Value_Indices
        
}
