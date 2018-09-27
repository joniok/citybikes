library("rjson")

my.JSON <- fromJSON(file="station.json")


df <- lapply(my.JSON, function(play) # Loop through each "play"
{
  # Convert each group to a data frame.
  # This assumes you have 6 elements each time
  data.frame(matrix(unlist(play), ncol=7, byrow=T))
})

# Now you have a list of data frames, connect them together in
# one single dataframe
df <- do.call(rbind, df)

# Make column names nicer, remove row names
colnames(df) <- names(my.JSON[[1]][[1]])
rownames(df) <- NULL

saveRDS(df, file= "stations.rds")