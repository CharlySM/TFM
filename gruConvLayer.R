library(ggplot2)
library(dplyr)
library(tidyr)
library(keras)
source("./generateTTV.R")

model <- keras_model_sequential() %>%
layer_conv_1d(filters=64, kernel_size=5, activation="tanh",
             input_shape = list(NULL, dim(data)[[-1]])) %>%
layer_max_pooling_1d(pool_size=4) %>%
layer_conv_1d(filters = 256, kernel_size = 7, activation = "tanh") %>%
layer_conv_1d(filters = 128, kernel_size = 7, activation = "tanh") %>%
layer_conv_1d(filters = 64, kernel_size = 7, activation = "tanh") %>%
layer_gru(units = 32,
           dropout = 0.1,
           recurrent_dropout = 0.5,
           return_sequences = TRUE,
           input_shape = list(NULL, dim(data)[[-1]]), activation="tanh") %>%
 layer_gru(units = 16, dropout = 0.1, recurrent_dropout = 0.5, activation="tanh") %>%
 layer_dense(units=1, activation="tanh")

  model %>% compile(
    optimizer = "rmsprop",
    loss = "mae",
    metrics = c("mse")
  )

  history <- model %>% fit(
    train_gen,
    steps_per_epoch = 50,
    epochs = 10,
    validation_data = val_gen,
    validation_steps = val_steps,
    batch_size=256)

summary(model)

plot(history)

model %>% save_model_hdf5("NNp6.h5")
