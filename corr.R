corr <- function(directory, threshold = 0, id = 1:332){
    
    #Sets the Working Directory
    
    if(directory == "specdata"){
        
        old.dir <- getwd()
        
        setwd("specdata")
    }
    
    vec <- vector()
    
    #Opens Files Specified by id and reads them into a data.frame, creates a vector containing cor(sulfate, nitrate)
    
    for (i in id) {
        
        if(i < 10){
            
            file.name <- paste("00", i, ".csv", sep = "")
            
            newdf <- read.csv(file.name)
            
            gobs <- complete.cases(newdf)
            
            neodf <- newdf[gobs, ]
            
            no.rows <- nrow(neodf)
            
            if(no.rows > threshold){
                
                corr.ns <- cor(neodf$sulfate, neodf$nitrate)
                
                vec <- c(vec, corr.ns)
            }
            
            
        }
        
        else if(i < 100){
            
            file.name <- paste("0", i, ".csv", sep = "")
            
            newdf <- read.csv(file.name)
            
            gobs <- complete.cases(newdf)
            
            neodf <- newdf[gobs, ]
            
            no.rows <- nrow(neodf)
            
            if(no.rows > threshold){
                
                corr.ns <- cor(neodf$sulfate, neodf$nitrate)
                
                vec <- c(vec, corr.ns)
            }
            
        }
        
        else if(i >= 100){
            
            file.name <- paste(i, ".csv", sep = "")
            
            newdf <- read.csv(file.name)
            
            gobs <- complete.cases(newdf)
            
            neodf <- newdf[gobs, ]
            
            no.rows <- nrow(neodf)
            
            if(no.rows > threshold){
                
                corr.ns <- cor(neodf$sulfate, neodf$nitrate)
                
                vec <- c(vec, corr.ns)
            }
        }
        
    }
    
    #Resets the Working Directory
    
    setwd(old.dir)
    
    vec
}
 