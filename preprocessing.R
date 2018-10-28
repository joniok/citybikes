library(dplyr)
library(plyr)

months <- c(9:4)
files <- lapply(months, function(x){
current_path = paste0("data/json/2018-0",x,"/")  

file_list <- list.files(path= current_path, pattern = ".json") # extract from folder "data/json/2018-09" and only files with ".json"

files_with_path <- paste0(current_path, file_list) # add path to files

return(files_with_path)
})

allFiles <- do.call(c,files)

json_to_df <- function(myjson){
  require(RJSONIO)
  
  if(isValidJSON(myjson)){
    
    json_file <- RJSONIO::fromJSON(myjson)
    
    df <- lapply(json_file, function(element) # Loop through each element
    {
      data.frame(matrix(unlist(element), ncol=7, byrow=T))  # Convert each group to a data frame, assumes you have 7 elements each time
    })
    
    df <- do.call(rbind, df) # Connect list of data frames together in to one single dataframe
    
    colnames(df) <- names(json_file[[1]][[1]]) # Make column names nicer, remove row names
    rownames(df) <- NULL
    
    return(df)
  }
}

tictoc::tic("Import of jsons") # start timer before importing
my_list <- lapply(allFiles, json_to_df)
names(my_list) <- allFiles

dfs <- ldply(my_list, rbind)
tictoc::toc() # stop timer


a <- dfs$.id
a <- sub(".*_", "", a)
date <- substr(a, 1, 8) #separate date from the path

b <- sub(".*T", "", a)
time <- substr(b, 1, 4) #separate time from the path
dfs$time <- time
datetime <- as.POSIXct(paste(date, time), format="%Y%m%d %H%M") #date and time strings as a datetime
dfs$datetime <- datetime #apply new datetime column to dataframe

i <- substr(dfs$name, 1, 3) #station id from the station name column
i <- as.integer(i)
dfs$station_id <- i

dfs_sorted <- dfs[order(dfs$station_id, dfs$datetime),] #sort the dataframe by station_id and datetime

ldf <- split(dfs_sorted, dfs_sorted$station_id) #separate different stations to own datasets


ldf <- lapply(ldf,function(x){
  x$lag_val <- lag(x$avl_bikes, 1) #apply a lag column
  x <- subset.data.frame(x,(x$avl_bikes != x$lag_val) | (is.na(x$lag_val)) | x$time == '0000' | x$time == '2359') # remove rows where the number of available bikes is the same between two subsequent rows
}
)

save(ldf, file = "data/ldf.Rdata")
