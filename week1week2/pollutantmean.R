## Assignment 1 - Part 1 'pollutantmean'
## Rahul Garkhail a.k.a mayintara <garkhail.rahul@gmail.com>

pollutantmean <- function(directory, pollutant, id = 1:332){
  files <- dir(directory, full.name = TRUE)
  data_all <- data.frame()
  for (i in id){
    data_all <- rbind(data_all, read.csv(files[i]))
  }
  data_subset <- subset(data_all, data_all$ID %in% id)
  if (pollutant == "sulfate"){
    mean(data_subset$sulfate, na.rm = TRUE)
  } else if (pollutant == "nitrate"){
    mean(data_subset$nitrate, na.rm = TRUE )
  } else {
    print(paste(pollutant,"! - are you sure?", sep = ""))
  }
}
