complete <- function(directory, id = 1:332){
    
    #Sets the Working Directory
    
    if(directory == "specdata"){
        
        old.dir <- getwd()
        
        setwd("specdata")
    }
    
    df <- data.frame()
    
    #Opens Files Specified by id and reads them into a data.frame then stores the number of complete cases of a given file in a new data.frame
    
    for (i in id) {
        
        if(i < 10){
            
            file.name <- paste("00", i, ".csv", sep = "")
            
            newdf <- read.csv(file.name)
            
            gobs <- complete.cases(newdf)
            
            no.rows <- nrow(newdf[gobs, ])
            
            vec <- c(i, no.rows)
            
            df <- rbind(df, vec)
            
            
        }
        
        else if(i < 100){
            
            file.name <- paste("0", i, ".csv", sep = "")
            
            newdf <- read.csv(file.name)
            
            gobs <- complete.cases(newdf)
            
            no.rows <- nrow(newdf[gobs, ])
            
            vec <- c(i, no.rows)
            
            df <- rbind(df, vec)
        }
        
        else if(i >= 100){
            
            file.name <- paste(i, ".csv", sep = "")
            
            newdf <- read.csv(file.name)
            
            gobs <- complete.cases(newdf)
            
            no.rows <- nrow(newdf[gobs, ])
            
            vec <- c(i, no.rows)
            
            df <- rbind(df, vec)
        }
        
    }
    
    #Resets the Working Directory
    
    setwd(old.dir)
    
    colnames(df) <- c("id", "nobs")
    
    df
    
}