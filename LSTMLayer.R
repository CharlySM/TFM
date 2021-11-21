library(ggplot2)
library(dplyr)
library(tidyr)
library(keras)
source("./generateTTV.R")

model <- keras_model_sequential() %>%
  layer_lstm(units = 64, batch_input_shape = c(1024,10,8),
    dropout = 0.2,
    recurrent_dropout = 0.1,
    return_sequences = TRUE, stateful = TRUE) %>%
 layer_lstm(units = 512,
    dropout = 0.1,
    recurrent_dropout = 0.1,
    activation = "relu",
    return_sequences = TRUE) %>%
  layer_lstm(units = 256,
    dropout = 0.1,
    recurrent_dropout = 0.1,
    activation = "relu",
    return_sequences = TRUE) %>%
  layer_lstm(units = 128,
      dropout = 0.1,
      recurrent_dropout = 0.1,
      activation = "relu",
      return_sequences = TRUE) %>%
  layer_dense(units = 1, activation = "tanh")

  model %>% compile(
    optimizer = "rmsprop",
    loss = "mae",
    metrics = c("mse")
  )

  history <- model %>% fit(
    train_gen,
    steps_per_epoch = 20,
    epochs = 8,
    validation_data = val_gen,
    validation_steps = val_steps,
    batch_size=256)

    summary(model)

    plot(history)

    model %>% save_model_hdf5("NNp3.h5")
