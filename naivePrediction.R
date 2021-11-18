source("./dataset.R")
library(forecast)

pronostico <- naive(ts(dataset$train[,c("Facility Total HVAC Electric Demand Power (Whole Building)")]),
                    xreg=dataset$train[,c("Site Outdoor Air Drybulb Temperature (Environment)",
                                          "Zone Thermostat Heating Setpoint Temperature (SPACE1-1)",
                                          "Zone Thermostat Cooling Setpoint Temperature (SPACE1-1)", "Zone Air Temperature (SPACE1-1)",
                                          "Zone Thermal Comfort Mean Radiant Temperature (SPACE1-1 PEOPLE 1)",
                                          "People Air Temperature (SPACE1-1 PEOPLE 1)")],h=length(dataset$test))

y1 <- dataset$test[,c("Facility Total HVAC Electric Demand Power (Whole Building)")]
summary(pronostico)
pronostico$mean
accuracy(object=pronostico$mean,x=y1)
length(y1)
plot(y1[1:100],type="l", ylab="Facility Total HVAC Electric Demand Power (Whole Building)", xlab="time")
lines(pronostico$mean[1:100], col="green")
plot(pronostico, include=500)
autoplot(pronostico)# + autolayer(fitted(pronostico))

