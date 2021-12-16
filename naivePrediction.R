source("./dataset.R")
library(forecast)
library(dplyr)

pronostico <- naive(ts(dataset$train[,c("Facility Total HVAC Electric Demand Power (Whole Building)")]),
                    xreg=dataset$train[,c("Site Outdoor Air Drybulb Temperature (Environment)",
                                          "Zone Thermostat Heating Setpoint Temperature (SPACE1-1)",
                                          "Zone Thermostat Cooling Setpoint Temperature (SPACE1-1)", "Zone Air Temperature (SPACE1-1)",
                                          "Zone Thermal Comfort Mean Radiant Temperature (SPACE1-1 PEOPLE 1)",
                                          "People Air Temperature (SPACE1-1 PEOPLE 1)")],h=length(dataset$test))

y1 <- dataset$test[,c("Facility Total HVAC Electric Demand Power (Whole Building)")]

length(pronostico$x)
checkresiduals(pronostico)
print(length(y1))
print(length(pronostico$x))
plot(y1[(length(y1)-500):length(y1)],type="l", ylab="Facility Total HVAC Electric Demand Power (Whole Building)", xlab="time")
lines(pronostico$x[1:500], col="blue")
plot(pronostico, include=250, col="blue")
plot(pronostico$x[1:100]) #+ autolayer(fitted(pronostico))

index<-seq(1:500)

df<-data.frame(x=y1[(length(y1)-500):(length(y1)-1)],y=pronostico$x[1:500],index)
print(summary(df))

ggplot(df, aes(x=index)) +
  geom_line(aes(y = x),color=1) +
  geom_line(aes(y = y), color="brown2")
