library(ggplot2)
library(dplyr)
library(tidyr)
library(keras)
library(tensorflow)
library(Metrics)

source("./generateTTV.R")

createNewFileName = function(path  = getwd(), pattern = "plot_of_something", extension=".png") {
  myExistingFiles = list.files(path = path, pattern = pattern)
  print(myExistingFiles)
  completePattern = paste0("^(",pattern,")([0-9]*)(",extension,")$")
  existingNumbers  = gsub(pattern = completePattern, replacement = "\\2", x = myExistingFiles)

  if (identical(existingNumbers, character(0)))
    existingNumbers = 0

  return(paste0(pattern,max(as.numeric(existingNumbers))+1,extension))
}
start_time <- Sys.time()

model <- load_model_hdf5("NNp6.h5", custom_objects=NULL, compile=TRUE)

pred <- predict(model, test_gen, batch_size=batch_size, steps=test_steps)
end_time <- Sys.time()

png(filename = createNewFileName(pattern="prediction"))
plot(pred, main="prediction")
dev.off()
df.predict <- data.frame(pred)
df.predict <- data.frame(C1 = c(t(df.predict)))
colnames(df.predict) <- c("Facility Total HVAC Electric Demand Power (Whole Building)")
summary(df.predict)
png(filename = createNewFileName(pattern="prediction_graphic"))
ggplot(df.predict, aes(x = 1:nrow(df.predict), y = `Facility Total HVAC Electric Demand Power (Whole Building)`), main="prediction graphic") + geom_line()
dev.off()

i<-(length(test)-length(pred))+1
test<-test[i:length(test)]

totalPred<-rbind(test,pred)
df.totalPred1 <- data.frame(totalPred)
df.totalPred<-t(df.totalPred1)
index<-seq(1:500)

df<-data.frame(x=test[1:500],y=pred[1:500],index)
ggplot(df, aes(x=index)) +
  geom_line(aes(y = x, colour="Test"))+
  geom_line(aes(y = y, colour="Predict"))+ scale_color_manual(values = c("Test"="black","Predict"="brown2"))
print(end_time - start_time)
rmse(test[1:500], pred[1:500])
