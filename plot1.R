## 25.06.2017: This is the R code for the Week 1 Project of Exploratory data Analysis course on Coursera.org.
## The script below operates on the "Individual household electric power consumption Data Set" from the UC Irvine Machine Learning Repository, ... 
## a popular repository for machine learning datasets, as made available  on the course web site https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
##
## The purpose of the scipt below is to reproduce the first graph provided with the assignment and save it as a .png file.
##


plot1 <- function() 
{
        
        ## Output:              None as such but a plot1.png file with the graph will be generated.
        ##                      Note that the script exits with an error if the data file is not found in the working directory.

        
        File_Name="household_power_consumption.txt"
        if (!file.exists(File_Name))
                stop(paste("File", File_Name, "not found. Execution aborted.", sep=" "))                
        
        # Data are semicolon-separated, with the header in the first row and NAs specified by "?". The first two columns are date and time, the rest are numerical
        Data=read.table(File_Name, sep=";", header=TRUE, colClasses=c("character", "character", rep("numeric", 7)), na.strings="?")
        # As per instructions, only interested in the data for 1 and 2 February, 2007
        Data=Data[(Data$Date=="1/2/2007") | (Data$Date=="2/2/2007"), ]
        Check_for_Inadmissible_Values_Numeric(Data$Global_active_power, 0, 1e6, "Values too large in column Global_active_power.")
        
        # At least on my system, an existing file is not overwritten by the below commands, so delete it first
        File_Name="../04. Output/plot1.png"
        if (file.exists(File_Name))
                file.remove(File_Name)
        
        # Open the device
        png(File_Name, width=480, height=480)
        # Plot the histogram in the format prescribed
        hist(Data$Global_active_power, col="red", xlab="Global Active Power (kilowatts)", ylab="Frequency", main="Global Active Power")
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
