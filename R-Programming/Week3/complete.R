complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  if(grep("specdata", directory) == 1) {
    directory <- ("./specdata/")
  }
  id_len <- length(id)
  complete_data <- rep(0, id_len)
  # find all files in the specdata folder
  all_files <- as.character( list.files(directory) )
  file_paths <- paste(directory, all_files, sep="")
  count <- 1
  for(i in id) {
    current_file <- read.csv(file_paths[i], header=T, sep=",")
    complete_data[count] <- sum(complete.cases(current_file))
    count <- count + 1
  }
  result <- data.frame(id = id, nobs = complete_data)
  return(result)
}