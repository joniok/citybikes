


# Alustetaan tarvittavia taulukoita ja vektoreita
mallit = data.frame(Time = 0, name = "tyhjä", day = "ma", coord = NA, slots = 0, R2 = 0, pred_bikes = 0)

days <- c("Monday", "Tuesday", "Wednesday","Thursday", "Friday", "Saturday", "Sunday")
times = data.frame(Time = seq(5, 22, 0.25))

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
