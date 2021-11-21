library(ggplot2)
library(dplyr)
library(tidyr)
library(keras)
source("./generateTTV.R")

model <- keras_model_sequential() %>%
  layer_gru(units = 32,
            dropout = 0.1,
            recurrent_dropout = 0.5,
            return_sequences = TRUE,
            input_shape = list(NULL, dim(data)[[-1]])) %>%
  layer_gru(units = 64, activation = "relu",
            dropout = 0.1,
            recurrent_dropout = 0.5) %>%
  layer_dense(units = 1)

  model %>% compile(
    optimizer = "Nadam",
    loss = "mae",
    metrics = c("mse")
  )

  history <- model %>% fit(
    train_gen,
    steps_per_epoch = 15,
    epochs = 5,
    validation_data = val_gen,
    validation_steps = val_steps,
    batch_size=128)

    summary(model)

    plot(history)

    model %>% save_model_hdf5("NNp2.h5")
