library(reticulate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(keras)
source("./generateTTV.R")

source_python("./python/GeneticAlgorithm.py")
addLayer <- function(model, units, activation){
  model %>% layer_dense(units=units,activation=activation)
}
calculateCoste <- function(individual){
  print(individual)
  model <- keras_model_sequential() %>% layer_flatten(input_shape=c(lookback/step,dim(data)[-1]))
  for (val in individual[[1]] ) {addLayer(model,val,individual[[2]] )}
  model %>% layer_dense(units=1)
  print("compile")
  model %>% compile(
    optimizer = individual[[3]],
    loss = "mae",
    metrics = c("mse")
  )
  print("fit")
  fit <- model %>% fit(
    train_gen,
    steps_per_epoch = individual[[4]],
    epochs = individual[[5]],
    validation_data = val_gen,
    validation_steps = val_steps,
    batch_size=individual[[6]])

  return(list("individual"=individual,"coste"=tail(fit$metric$mse, n=1),"modelo"=model, "fit"=fit))
}

minimo <- function(res){
  min=1
  for(i in 1:length(res)){ if(res[[i]]$coste<res[[min]]$coste){ min=i}}
  return(min)
}
maximo <- function(res){
  max=1
  for(i in 1:length(res)){ if(res[[i]]$coste>res[[max]]$coste){ max=i}}
  return(max)
}

poblation<-list()
for(i in 0:8) poblation[[length(poblation)+1]]=createIndividual()
poblationCoste <- lapply(poblation, calculateCoste)

for(i in 0:4){
index <- selectToCross(poblation)
childs<-crossOX(poblation, index[[1]],index[[2]])
res<-lapply(childs, calculateCoste)
min=minimo(poblationCoste)
max=maximo(res)
if(poblationCoste[[min]]$coste<res[[max]]$coste) {res[[max]]=poblationCoste[[min]]}
poblationCoste=res
}
min=minimo(poblationCoste)
summary(poblationCoste[[min]]$modelo)
print(poblationCoste[[min]]$coste)
plot(poblationCoste[[min]]$fit)
poblationCoste[[min]]$modelo %>% save_model_hdf5("GANN.h5")
