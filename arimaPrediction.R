source("./dataset.R")
library(forecast)

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
accuracy(object=pronostico$mean,x=y1)
length(pronostico$x)
checkresiduals(model)

plot(y1[1:100],type="l", ylab="Facility Total HVAC Electric Demand Power (Whole Building)", xlab="time")
lines(pronostico$mean[1:100], col="blue")
plot(pronostico, include=250, col="blue")
autoplot(pronostico) # + autolayer(fitted(pronostico))


