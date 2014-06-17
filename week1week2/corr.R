## Assignment 1 - Part 3 'corr'
## Rahul Garkhail a.k.a mayintara <garkhail.rahul@gmail.com>

corr <- function(directory, threshold = 0) {
            files <- dir(directory, full.names = TRUE)            
            corra<-NULL
            id=1:length(files)
            for(i in id){
                        data <- read.csv(files[i])
                        dat_comp<-data[complete.cases(data),]
                        nobs=nrow(dat_comp)
                        if (nobs > threshold) {
                            corra <- append(corra, cor(dat_comp$sulfate, 
                                    dat_comp$nitrate, use = "complete.obs") ) 
                        }
            }
            return(corra)
}