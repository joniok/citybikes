curr_locale <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME","en_US.UTF-8")

files <- paste0("data/", list.files(path ="data/", pattern = ".rds"))
files <- files[4:8]

tictoc::tic("Start")

allFiles <-  lapply(files, function(x){
  list <- readRDS(x)
  df <- do.call(rbind.data.frame, list)
  return(df)
})

allData <- do.call(rbind.data.frame, allFiles)

allData$name <- as.character(allData$name)
Encoding(allData$name) <- "latin1"
Encoding(allData$name) <- "UTF-8"
allData$name <- as.factor(allData$name)
tictoc::toc()

# formatting
days <- c("Monday", "Tuesday", "Wednesday","Thursday", "Friday", "Saturday", "Sunday")
my_times <- seq(5, 22, 0.25)

format_df <- function(x){
  stopifnot(is.data.frame(x))
  stopifnot("datetime" %in% colnames(x))

  x$date <- as.Date(x$datetime) 
  x$time <- format(as.POSIXct(x$datetime) ,format = "%H:%M:%S")
  x$time <- sapply(strsplit(x$time,":"),
                   function(x) {
                     x <- as.numeric(x)
                     x[1]+x[2]/60
                   })
  x$day <- weekdays(as.Date(x$date,'%Y-%m-%d'))
  return(x)
}


tictoc::tic("Test formatting")
allData <- format_df(allData)
tictoc::toc()




## Fit the models

fix_coord <- function(x){
  return(names(which.max(table(x))))
}

our_model <- function(day = day,
                      data = input,
                      station = current_station,
                      times = my_times){
  
  stopifnot(tolower(day) %in% tolower(weekdays(x=as.Date(seq(7), origin="1950-01-01")))) # check that we have proper dates
  stopifnot(is.data.frame(data))
  stopifnot(c("name","day", "time", "avl_bikes", "total_slots", "coordinates") %in% colnames(data))
  
  times_df <- data.frame(time = times)
  
  daily_data <- data[(data$day == day) & (data$name == station),]
  
  if(length(unique(daily_data$avl_bikes)) > 5){
    daily_data$avl_bikes <- as.integer(daily_data$avl_bikes)
    model <- lm(avl_bikes ~ poly(time,5), data = daily_data)
    row <- data.frame(name = station , day = day, coord = fix_coord(daily_data$coordinates), slots = median(as.numeric(daily_data$total_slots)), R2 = summary(model)$adj.r.squared)
    row = merge(times, row)
    row$pred_bikes = round(predict(model, newdata = times_df))
    return(row)
  }
}

stations <- unique(allData$name)

station_data <- lapply(stations, function(currStation){
  
  output <- lapply(days, function(currDay){
    print(currStation)
    print(currDay)
    return(our_model(day = currDay, data = allData, station = currStation, times = my_times))
  })
  
  output.df <- do.call(rbind.data.frame, output)
  
  return(output.df)
})



final_data <- do.call(rbind.data.frame, station_data)

for(i in 1:length(final_data$x)) {
  if(final_data$pred_bikes[i] < 0) {
    final_data$pred_bikes[i] = as.numeric(0)
  }
  if(final_data$pred_bikes[i] > as.numeric(final_data$slots[i])) {
    final_data$pred_bikes[i] = as.numeric(final_data$slots[i])
  }
}

# Hardcoded imputation for missing coordinates

final_data$coord <- as.character(final_data$coord)
final_data <- final_data[(final_data$name != "155 Piispansilta"),]

dt <- tidyr::separate(data = final_data,
                      col = coord,
                      into = c("lat", "long"),
                      sep=",",
                      remove= FALSE)

dt$lat <- as.numeric(dt$lat)
dt$long <- as.numeric(dt$long)
dt$pred_color <- as.integer(dt$pred_bikes)/as.integer(dt$slots)

dt$time <- format(as.POSIXct(dt$x*3600, origin = "2001-01-01", "GMT"), "%H:%M")

saveRDS(dt, "data/prediction.rds")

Sys.setlocale("LC_TIME",curr_locale)
