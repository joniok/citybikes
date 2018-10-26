# Helsinki city bike availability prediction app
A Shiny application to predict city bike availability in Helsinki in 15 minute period and daily basis.

Shinyapps.io link: [jonioks.shinyapps.io/citybikes/](https://jonioks.shinyapps.io/citybikes/)

## Data

[Helsinki Region Transport (HSL)](https://dev.hsl.fi/) provides almost minute by minute. Application is currently using data from cycling season 2018 from April to August. Data from September was used for validation.

Data was gathered from [here](https://dev.hsl.fi/citybike/stations/) and also from [here](https://dev.hsl.fi/~haphut/citybikes/). The json files were extracted to `data/json/` to folders `data/json/2018-04/`... `data/json/2018-08/`. If you want to replicate our work, please use the same folder structure after you have downloaded and extracted the jsons from HSL's sites.

[More about Helsinki city bikes](https://kaupunkipyorat.hsl.fi/en)

## Model

Our model assumes that the amount of bikes at a certain station on a certain day of the week follows some pattern (e.g.in the morning when people go to work the amount of the bikes decreases). Model fits a polynomial regression model for every station for every day of the week and use the model to predict the number of available bikes on the stations.

The main challenge is choosing the degree of the polynomial, so that our model fits the pattern but doesn’t overfit. The same degree should be suitable for all different stations, because it is not possible to adjust the model separately for every single station 

## Application

Application is currenlty hosted at Shinyapps.io. More about using the application can be found from the help page in the application.


## Authors
Patrik Lauha, Veera Nenonen, Joni Oksanen


## Note:
This was a course project in the course Introduction to Data Science (in University of Helsinki) in fall 2018.