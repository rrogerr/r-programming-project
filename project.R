library(data.table)

add_zeros <- function(id){ #adds zeros to format "1" as "001" and "67" and "067"
        id <- as.character(id)
        
        for(i in 1:length(id)) {
                if(as.numeric(id[i]) < 10){
                        id[i] <- paste("00", id[i], sep = "")
                }
                else if(as.numeric(id[i]) < 100){
                        id[i] <- paste("0", id[i], sep = "")
                }
        }
        
        id
}

pollutantmean <- function(directory, pollutant, id = 1:332){
        id <- add_zeros(id)
        
        #import columns from csv files        
        
        C <- data.frame()
        
        for(i in 1:length(id)){
                C <- rbind(C,fread(paste(directory, id[i], ".csv", sep = ""), select = pollutant))
        }
        
        colMeans(C, na.rm = TRUE)[[1]]
}

complete <- function(directory, id = 1:332){ #counts complete cases in specdata
        id <- add_zeros(id)
        
        nobs <- numeric()
        
        for(i in 1:length(id)){
                Q <- fread(paste(directory, id[i], ".csv", sep = ""))
                nobs[i] <- sum(complete.cases(Q))
        }
        
        data.frame(id, nobs)
}

corr <- function(directory, threshold = 0) {
        id <- add_zeros(1:332)
        
        corr_vec <- numeric()
        
        for (i in 1:length(id)) {
                Q <- fread(paste(directory, id[i], ".csv", sep = ""))
                
                if (sum(complete.cases(Q)) > threshold) {
                        Q <- Q[complete.cases(Q)]
                        corr_vec <- c(corr_vec, cor(Q$sulfate, Q$nitrate))
                }
        }
        
        corr_vec
}
