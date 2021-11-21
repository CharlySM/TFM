library(ggplot2)
library(dplyr)
library(tidyr)
library(keras)
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

model <- load_model_hdf5("NNp2.h5")

pred <- predict(model, test_gen, batch_size=batch_size, steps=test_steps)

png(filename = createNewFileName(pattern="prediction"))
plot(pred, main="prediction")
dev.off()
df.predict <- data.frame(pred)
colnames(df.predict) <- c("Facility Total HVAC Electric Demand Power (Whole Building)")
summary(df.predict)
png(filename = createNewFileName(pattern="prediction_graphic"))
ggplot(df.predict, aes(x = 1:nrow(df.predict), y = `Facility Total HVAC Electric Demand Power (Whole Building)`), main="prediction graphic") + geom_line()
dev.off()


x_axes = seq(1:length(pred)+length(pred))
x_axes2 = seq(1+length(pred),length(pred)+length(pred))

#plot(x_axes, test[1:length(pred)], col="red", type="l")
#lines(x_axes2, pred, col="blue")
#legend("topleft", legend=c("y-original", "y-predicted"),
#        col=c("red", "blue"), lty=1,cex=0.8)
i<-(length(test)-length(pred))+1
test<-test[i:length(test)]

totalPred<-rbind(test,pred)
df.totalPred1 <- data.frame(totalPred)
df.totalPred<-t(df.totalPred1)
index<-seq(1:(length(df.totalPred)-1))

df<-data.frame(x=test,y=pred,index)
df1 <- df[c("index","y")]
df2 <- df[c("index","x")]
df1$index<-df1$index + 128000
colnames(df2)<-c("index","y")
df1$colour<-c(rep(2, n = 128000))
df2$colour<-c(rep(1, n = 128000))
df2<-tail(df2, n=500)
df1<-head(df1,500)
df<-union(df2,df1)
df$index<-df$index-127500
summary(df)

#meltdf <- melt(df,id="index")
ggplot(df, aes(x=index,y=y,colour=colour)) + geom_line()
