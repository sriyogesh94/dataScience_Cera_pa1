pollutantmean <- function(directory, pollutant, id = 1:332){
    
    #Sets the Working Directory
    
    if(directory == "specdata"){
        
        old.dir <- getwd()
        
        setwd("specdata")
    }
    
    df <- data.frame()
    
    #Opens Files Specified by id and reads them into a data.frame
    
    for (i in id) {
        
        if(i < 10){
            
            file.name <- paste("00", i, ".csv", sep = "")
            
            newdf <- read.csv(file.name)
            
            df <- rbind(df, newdf)
        }
        
        else if(i < 100){
            
            file.name <- paste("0", i, ".csv", sep = "")
            
            newdf <- read.csv(file.name)
            
            df <- rbind(df, newdf)
        }
        
        else if(i >= 100){
            
            file.name <- paste(i, ".csv", sep = "")
            
            newdf <- read.csv(file.name)
            
            df <- rbind(df, newdf)
        }
        
    }
    
    #Pollutant Mean Calculator
    
    if(pollutant == "sulfate"){
        
        pollutant.mean <- mean(df$sulfate, na.rm = TRUE)
    }
    
    else if(pollutant == "nitrate"){
        
        pollutant.mean <- mean(df$nitrate, na.rm = TRUE)
    }
    
    #Resets the Working Directory
    
    setwd(old.dir)
    
    pollutant.mean
}