library(ggplot2)


getTrainAndTest <- function(data){
  smp_size <- floor(0.75 * nrow(data))
  set.seed(45321)
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
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

data <- dataTotal[complete.cases(dataTotal), ]
data$done<-NULL

data$time<-paste(data$month,"/",data$day," ",data$hour)

dataSelected<-data[,c("time","Site Outdoor Air Drybulb Temperature (Environment)",
                      "Zone Thermostat Heating Setpoint Temperature (SPACE1-1)",
                      "Zone Thermostat Cooling Setpoint Temperature (SPACE1-1)", "Zone Air Temperature (SPACE1-1)",
                      "Zone Thermal Comfort Mean Radiant Temperature (SPACE1-1 PEOPLE 1)",
                      "People Air Temperature (SPACE1-1 PEOPLE 1)",
                      "Facility Total HVAC Electric Demand Power (Whole Building)")]

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

columns <- c("time","Site Outdoor Air Drybulb Temperature (Environment)",
             "Zone Thermostat Heating Setpoint Temperature (SPACE1-1)",
             "Zone Thermostat Cooling Setpoint Temperature (SPACE1-1)", "Zone Air Temperature (SPACE1-1)",
             "Zone Thermal Comfort Mean Radiant Temperature (SPACE1-1 PEOPLE 1)",
             "People Air Temperature (SPACE1-1 PEOPLE 1)",
             "Facility Total HVAC Electric Demand Power (Whole Building)")

#dataSelected[, columns] <- sapply(dataSelected[columns],as.numeric)

#data <- data[,c("Zone Air Temperature (SPACE1-1)", "Zone Thermal Comfort Mean Radiant Temperature (SPACE1-1 PEOPLE 1)", "Facility Total HVAC Electric Demand Power (Whole Building)")]
#data[c("Zone Air Temperature (SPACE1-1)")]<-celsius_to_kelvin(data[c("Zone Air Temperature (SPACE1-1)")])
#dataset <- getTrainAndTest(data)
#ggplot(data[1:2880,], aes(x = 1:2880, y = `Site Outdoor Air Drybulb Temperature (Environment)`)) + geom_line()

#columns <- c("Zone Air Temperature (SPACE1-1)", "Zone Thermal Comfort Mean Radiant Temperature (SPACE1-1 PEOPLE 1)", "Facility Total HVAC Electric Demand Power (Whole Building)")
#dataset$train[, columns] <- sapply(dataset$train[columns],as.numeric)
dataTotal <- data.matrix(dataSelected)
print(nrow(dataTotal))
