library(ggplot2)
library(dplyr)
library(tidyr)
library(keras)
source("./generateTTV.R")

model <- keras_model_sequential() %>%
    layer_flatten(input_shape=c(lookback/step,dim(data)[-1])) %>%
    layer_dense(units=32,activation="relu") %>%
    layer_dense(units=64,activation="relu") %>%
    layer_dense(units=128,activation="relu") %>%
    layer_dense(units=32,activation="relu") %>%
    layer_dense(units=8,activation="relu") %>%
    layer_dense(units=1, activation="tanh")


model %>% compile(
  optimizer = "Nadam",
  loss = "mae",
  metrics = c("mse")
)

history <- model %>% fit(
  train_gen,
  steps_per_epoch = 10,
  epochs = 10,
  validation_data = val_gen,
  validation_steps = val_steps,
  batch_size=128)

summary(model)

plot(history)

model %>% save_model_hdf5("NNp1.h5")
