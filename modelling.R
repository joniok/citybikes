library("data.table")
library("tidyverse")

files <- paste0("data/", list.files(path ="data/", pattern = ".rds"))

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
    row <- data.frame(name = station , day = day, coord = daily_data$coordinates[1], slots = daily_data$total_slots[1], R2 = summary(model)$adj.r.squared)
    row = merge(times, row)
    row$pred_bikes = round(predict(model, newdata = times_df))
    print("done")
    return(row)
  }
}

#predictions = data.frame(time = 0, name = "", day = "", coord = NA, slots = 0, R2 = 0, pred_bikes = 0)


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


give_me_data <- do.call(rbind.data.frame, station_data)

saveRDS(give_me_data, "data/another_pred.rds")
####################################################################

# Alustetaan tarvittavia taulukoita ja vektoreita
mallit = data.frame(Time = 0, name = "tyhjä", day = "ma", coord = NA, slots = 0, R2 = 0, pred_bikes = 0)

# Sovitetaan mallit kaikille asemille
for(i in 1:length(ldf)){
  stat = ldf[i]
  stat = as.data.frame(stat)
  stat = stat[-c(9:15)]
  print(i)
  colnames(stat) = c("id", "name", "coordinates", "total_slots", "free_slots",  "avl_bikes", "operative", "style", "datetime", "station_id", "lag_val") 
  print(stat$name[1])
  #Ajan muotoilu
  stat$Date <- as.Date(stat$datetime) 
  stat$Time <- format(as.POSIXct(stat$datetime) ,format = "%H:%M:%S") 
  stat$Day = weekdays(as.Date(stat$Date,'%Y-%m-%d'))
  stat$Time = sapply(strsplit(stat$Time,":"),
                     function(x) {
                       x <- as.numeric(x)
                       x[1]+x[2]/60
                     })
  # Sovitetaan mallit jokaiselle viikonpäivälle
  for(day in days) {
    stat2 = subset(stat, Day == day)
    if(length(stat2$name) > 5) {
      stat2['avl_bikes'] = as.numeric(levels(stat2$avl_bikes))[stat2$avl_bikes]
      model = lm(avl_bikes ~ poly(Time, 5), data = stat2)
      uusirivi = data.frame(name= stat2$name[1], day = day, coord = stat2$coordinates[1], slots = stat2$total_slots[1], 
                            R2 = summary(model)$adj.r.squared)
      uusirivi = merge(times, uusirivi)
      uusirivi$pred_bikes = round(predict(model, newdata = times))
      mallit = rbind(mallit, uusirivi)
      # Kommentoi seuraavat rivit, jos et halua plotata
      plot1 <- data.frame(Time = times$Time)
      plot1$pred <- uusirivi$pred_bikes
      plot(avl_bikes ~ Time, data = stat2, xlim = c(5,22), main=paste(uusirivi$name[1], day, sep = ", "), xlab = "Time", ylab = "Bikes")
      lines(plot1$Time, plot1$pred, col = "red")
      # Plottaus päättyy tähän
    }  
  }
}

# Poistetaan turha ensimmäinen rivi
mallit = mallit[-1,]

#Miten malli toimii? Ei hyvältä näytä...
R2 = unique(mallit$R2)
mean(R2[!is.na(R2)])
min(mallit$pred_bikes)
max(mallit$pred_bikes)
mean(mallit$pred_bikes)

# Hiotaan karmeimpia ennusteita, for-loopilla, kuinkas muuten
for(i in 1:length(mallit$Time)) {
  if(mallit$pred_bikes[i] < 0) {
    mallit$pred_bikes[i] = as.numeric(0)
  }
  if(mallit$pred_bikes[i] > as.numeric(mallit$slots[i])) {
    mallit$pred_bikes[i] = as.numeric(mallit$slots[i])
  }
}
