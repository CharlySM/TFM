
source("./dataset.R")
library(forecast)
library(dplyr)

start_time <- Sys.time()
xregTrain <- cbind(OutdoorAirTemperature = dataset$train[, c("Site Outdoor Air Drybulb Temperature (Environment)")],
                   HeatingSetpointTemperature = dataset$train[, c("Zone Thermostat Heating Setpoint Temperature (SPACE1-1)")],
                   CoolingSetpointTemperature = dataset$train[, c("Zone Thermostat Cooling Setpoint Temperature (SPACE1-1)")],
                   AirTemperature = dataset$train[, c("Zone Air Temperature (SPACE1-1)")],
                   ComfortMeanRadiantTemperature = dataset$train[, c("Zone Thermal Comfort Mean Radiant Temperature (SPACE1-1 PEOPLE 1)")],
                   PeopleAirTemperature = dataset$train[, c("People Air Temperature (SPACE1-1 PEOPLE 1)")])

xregTest <- cbind(OutdoorAirTemperature = dataset$train[, c("Site Outdoor Air Drybulb Temperature (Environment)")],
                  HeatingSetpointTemperature = dataset$train[, c("Zone Thermostat Heating Setpoint Temperature (SPACE1-1)")],
                  CoolingSetpointTemperature = dataset$train[, c("Zone Thermostat Cooling Setpoint Temperature (SPACE1-1)")],
                  AirTemperature = dataset$train[, c("Zone Air Temperature (SPACE1-1)")],
                  ComfortMeanRadiantTemperature = dataset$train[, c("Zone Thermal Comfort Mean Radiant Temperature (SPACE1-1 PEOPLE 1)")],
                  PeopleAirTemperature = dataset$train[, c("People Air Temperature (SPACE1-1 PEOPLE 1)")])
model <- auto.arima(ts(dataset$train[,c("Facility Total HVAC Electric Demand Power (Whole Building)")]), xreg=xregTrain)
y1 <- dataset$test[,c("Facility Total HVAC Electric Demand Power (Whole Building)")]
pronostico <- forecast(model, xreg = xregTest)
end_time <- Sys.time()
accuracy(object=pronostico$mean,x=y1)
length(pronostico$x)
checkresiduals(model)

plot(y1[(length(y1)-500):(length(y1)-1)],type="l", ylab="Facility Total HVAC Electric Demand Power (Whole Building)", xlab="time")
lines(pronostico$x[1:500], col=2)
plot(pronostico, include=250, col=2)
autoplot(pronostico) # + autolayer(fitted(pronostico))

index<-seq(1:500)

df<-data.frame(x=y1[(length(y1)-500):(length(y1)-1)],y=pronostico$x[1:500],index)
print(summary(df))

ggplot(df, aes(x=index)) +
  geom_line(aes(y = x, colour="Test"))+
  geom_line(aes(y = y, colour="Predict"))+ scale_color_manual(values = c("Test"="black","Predict"="brown2"))
print(end_time - start_time)
