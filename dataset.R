library(Hmisc)
library(corrplot)
library(psych)
require(ggplot2)
require(reshape2)

getTrainAndTest <- function(data){
  smp_size <- floor(0.75 * nrow(data))
  set.seed(45321)
  train_ind <- 1: smp_size #sample(seq_len(nrow(data)), size = smp_size)
  return(list(train=data[train_ind, ], test=data[-train_ind, ]))
}

celsius_to_kelvin <- function(temp) {
  kelvin <- temp + 273.15
  return(kelvin)
}

normalize <- function(x, na.rm = TRUE) {
    return((x- min(x)) /(max(x)-min(x)))
}

readDF <- function(route, df) {
  aux <- read.csv(file = route, header = TRUE)
  colnames(aux)<-colnames(df)
  return(aux)
}
x<-1:10
route <- paste("./datasets_energym_v2/Eplus-env-continuous-stochastic-mixed-v1-res1/monitor",x, ".csv", sep="")
dataTotal <- read.csv(file = "./Eplus-env-discrete-mixed-v1-res1(RULE_BASED_CONTROLLER)/monitor.csv", check.names=FALSE)
aux <- lapply(route, readDF, dataTotal)
df <- do.call("rbind", aux)
dataTotal <- rbind(dataTotal,df)
colnames(dataTotal)
summary(dataTotal)
data <- dataTotal[complete.cases(dataTotal), ]
data$done<-NULL
correlacion<-round(cor(data), 1)
corrplot(correlacion, method="number", type="full", number.cex=0.4, tl.cex=0.4)

data$time<-paste(data$month,"/",data$day," ",data$hour)
#Medicion temperatura aire exterior no normalizada
dfOutdoorAir<-data[1:10000,c("time","Site Outdoor Air Drybulb Temperature (Environment)")]
plot(dfOutdoorAir$`Site Outdoor Air Drybulb Temperature (Environment)`, col="red",
type="l", ylab="Site Outdoor Air Drybulb Temperature (Environment)", xlab="time",
xlim=c(0,10000), ylim = c(-100, 100))

#Medicion temperatura de consigna de calefacción del termostato de zona
dfHeatingSetPoint<-data[1:10000,c("time","Zone Thermostat Heating Setpoint Temperature (SPACE1-1)")]
plot(dfHeatingSetPoint$`Zone Thermostat Heating Setpoint Temperature (SPACE1-1)`, col="red",
type="l", ylab="Zone Thermostat Heating Setpoint Temperature (SPACE1-1)", xlab="time",
xlim=c(0,10000), ylim = c(-100, 100))

#Medicion temperatura de consigna de refrigeracion del termostato de zona
dfCoolingSetPoint<-data[1:10000,c("time","Zone Thermostat Cooling Setpoint Temperature (SPACE1-1)")]
plot(dfCoolingSetPoint$`Zone Thermostat Cooling Setpoint Temperature (SPACE1-1)`, col="red",
type="l", ylab="Zone Thermostat Cooling Setpoint Temperature (SPACE1-1)", xlab="time",
xlim=c(0,10000), ylim = c(-100, 100))

#Medicion temperatura de aire de zona
dftempAirZone<-data[1:10000,c("time","Zone Air Temperature (SPACE1-1)")]
plot(dftempAirZone$`Zone Air Temperature (SPACE1-1)`, col="red",
type="l", ylab="Zone Air Temperature (SPACE1-1)", xlab="time",
xlim=c(0,10000), ylim = c(-20, 45))

#Medicion Zona de confort térmico Temperatura radiante media
dftempConfortThermal<-data[1:10000,c("time","Zone Thermal Comfort Mean Radiant Temperature (SPACE1-1 PEOPLE 1)")]
plot(dftempConfortThermal$`Zone Thermal Comfort Mean Radiant Temperature (SPACE1-1 PEOPLE 1)`, col="red",
type="l", ylab="Zone Thermal Comfort Mean Radiant Temperature (SPACE1-1 PEOPLE 1)", xlab="time",
xlim=c(0,10000), ylim = c(-20, 45))

#Medicion Temperatura del aire de las personas
dftempPeople<-data[1:10000,c("time","People Air Temperature (SPACE1-1 PEOPLE 1)")]
plot(dftempPeople$`People Air Temperature (SPACE1-1 PEOPLE 1)`, col="red",
type="l", ylab="People Air Temperature (SPACE1-1 PEOPLE 1)", xlab="time",
xlim=c(0,10000), ylim = c(-20, 45))

dataSelected<-data[,c("time","Site Outdoor Air Drybulb Temperature (Environment)",
"Zone Thermostat Heating Setpoint Temperature (SPACE1-1)",
"Zone Thermostat Cooling Setpoint Temperature (SPACE1-1)", "Zone Air Temperature (SPACE1-1)",
"Zone Thermal Comfort Mean Radiant Temperature (SPACE1-1 PEOPLE 1)",
"People Air Temperature (SPACE1-1 PEOPLE 1)",
"Facility Total HVAC Electric Demand Power (Whole Building)")]

dfPredicted<-dataSelected[1:10000,c("time","Facility Total HVAC Electric Demand Power (Whole Building)")]
plot(dfPredicted$`Facility Total HVAC Electric Demand Power (Whole Building)`, col="red",
type="l", ylab="Facility Total HVAC Electric Demand Power (Whole Building)", xlab="time",
xlim=c(0,10000), ylim = c(173, 27477))


dataSelected[,c("Site Outdoor Air Drybulb Temperature (Environment)",
"Zone Thermostat Heating Setpoint Temperature (SPACE1-1)",
"Zone Thermostat Cooling Setpoint Temperature (SPACE1-1)", "Zone Air Temperature (SPACE1-1)",
"Zone Thermal Comfort Mean Radiant Temperature (SPACE1-1 PEOPLE 1)",
"People Air Temperature (SPACE1-1 PEOPLE 1)")]<-lapply(dataSelected[, c("Site Outdoor Air Drybulb Temperature (Environment)",
"Zone Thermostat Heating Setpoint Temperature (SPACE1-1)",
"Zone Thermostat Cooling Setpoint Temperature (SPACE1-1)", "Zone Air Temperature (SPACE1-1)",
"Zone Thermal Comfort Mean Radiant Temperature (SPACE1-1 PEOPLE 1)",
"People Air Temperature (SPACE1-1 PEOPLE 1)")], celsius_to_kelvin)

dataSelected[,c("Site Outdoor Air Drybulb Temperature (Environment)",
"Zone Thermostat Heating Setpoint Temperature (SPACE1-1)",
"Zone Thermostat Cooling Setpoint Temperature (SPACE1-1)", "Zone Air Temperature (SPACE1-1)",
"Zone Thermal Comfort Mean Radiant Temperature (SPACE1-1 PEOPLE 1)",
"People Air Temperature (SPACE1-1 PEOPLE 1)",
"Facility Total HVAC Electric Demand Power (Whole Building)")]<-lapply(dataSelected[, c("Site Outdoor Air Drybulb Temperature (Environment)",
"Zone Thermostat Heating Setpoint Temperature (SPACE1-1)",
"Zone Thermostat Cooling Setpoint Temperature (SPACE1-1)", "Zone Air Temperature (SPACE1-1)",
"Zone Thermal Comfort Mean Radiant Temperature (SPACE1-1 PEOPLE 1)",
"People Air Temperature (SPACE1-1 PEOPLE 1)",
"Facility Total HVAC Electric Demand Power (Whole Building)")], normalize)

#Medicion temperatura aire exterior no normalizada
dfOutdoorAir<-dataSelected[1:10000,c("time","Site Outdoor Air Drybulb Temperature (Environment)")]
plot(dfOutdoorAir$`Site Outdoor Air Drybulb Temperature (Environment)`, col="red",
type="l", ylab="Site Outdoor Air Drybulb Temperature (Environment)", xlab="time",
xlim=c(0,10000), ylim = c(0, 1))

#Medicion temperatura de consigna de calefacción del termostato de zona
dfHeatingSetPoint<-dataSelected[1:10000,c("time","Zone Thermostat Heating Setpoint Temperature (SPACE1-1)")]
plot(dfHeatingSetPoint$`Zone Thermostat Heating Setpoint Temperature (SPACE1-1)`, col="red",
type="l", ylab="Zone Thermostat Heating Setpoint Temperature (SPACE1-1)", xlab="time",
xlim=c(0,10000), ylim = c(0, 1))

#Medicion temperatura de consigna de refrigeracion del termostato de zona
dfCoolingSetPoint<-dataSelected[1:10000,c("time","Zone Thermostat Cooling Setpoint Temperature (SPACE1-1)")]
plot(dfCoolingSetPoint$`Zone Thermostat Cooling Setpoint Temperature (SPACE1-1)`, col="red",
type="l", ylab="Zone Thermostat Cooling Setpoint Temperature (SPACE1-1)", xlab="time",
xlim=c(0,10000), ylim = c(0, 1))

#Medicion temperatura de aire de zona
dftempAirZone<-dataSelected[1:10000,c("time","Zone Air Temperature (SPACE1-1)")]
plot(dftempAirZone$`Zone Air Temperature (SPACE1-1)`, col="red",
type="l", ylab="Zone Air Temperature (SPACE1-1)", xlab="time",
xlim=c(0,10000), ylim = c(0, 1))

#Medicion Zona de confort térmico Temperatura radiante media
dftempConfortThermal<-dataSelected[1:10000,c("time","Zone Thermal Comfort Mean Radiant Temperature (SPACE1-1 PEOPLE 1)")]
plot(dftempConfortThermal$`Zone Thermal Comfort Mean Radiant Temperature (SPACE1-1 PEOPLE 1)`, col="red",
type="l", ylab="Zone Thermal Comfort Mean Radiant Temperature (SPACE1-1 PEOPLE 1)", xlab="time",
xlim=c(0,10000), ylim = c(0, 1))

#Medicion Temperatura del aire de las personas
dftempPeople<-dataSelected[1:10000,c("time","People Air Temperature (SPACE1-1 PEOPLE 1)")]
plot(dftempPeople$`People Air Temperature (SPACE1-1 PEOPLE 1)`, col="red",
type="l", ylab="People Air Temperature (SPACE1-1 PEOPLE 1)", xlab="time",
xlim=c(0,10000), ylim = c(0, 1))

dfPredicted<-dataSelected[1:10000,c("time","Facility Total HVAC Electric Demand Power (Whole Building)")]
plot(dfPredicted$`Facility Total HVAC Electric Demand Power (Whole Building)`, col="red",
type="l", ylab="Facility Total HVAC Electric Demand Power (Whole Building)", xlab="time",
xlim=c(0,10000), ylim = c(0, 1))

#dataTotal <- dataTotal[,c("Site Outdoor Air Drybulb Temperature (Environment)", "Site Outdoor Air Relative Humidity (Environment)", "Space1-HtgSetP-RL")]
#dataTotal[c("Site Outdoor Air Drybulb Temperature (Environment)")]<-celsius_to_kelvin(dataTotal[c("Site Outdoor Air Drybulb Temperature (Environment)")])
dataset <- getTrainAndTest(dataSelected)

columns <- c("time","Site Outdoor Air Drybulb Temperature (Environment)",
"Zone Thermostat Heating Setpoint Temperature (SPACE1-1)",
"Zone Thermostat Cooling Setpoint Temperature (SPACE1-1)", "Zone Air Temperature (SPACE1-1)",
"Zone Thermal Comfort Mean Radiant Temperature (SPACE1-1 PEOPLE 1)",
"People Air Temperature (SPACE1-1 PEOPLE 1)",
"Facility Total HVAC Electric Demand Power (Whole Building)")

dataset$train[, columns] <- sapply(dataset$train[columns],as.numeric)
dataset$test[, columns] <- sapply(dataset$test[columns],as.numeric)

summary(dataSelected)
dim(dataset$train)

