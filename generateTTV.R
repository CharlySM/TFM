source("./datasetNN.R")

generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 128, step = 6) {
  if (is.null(max_index))
    max_index <- nrow(data) - delay - 1
  i <- min_index + lookback
  function() {
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      rows <- c(i:min(i+batch_size-1, max_index))
      i <<- i + length(rows)
    }

    samples <- array(0, dim = c(length(rows),
                                lookback / step,
                                dim(data)[[-1]]))
    targets <- array(0, dim = c(length(rows)))

    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]]-1,
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      targets[[j]] <- data[rows[[j]] + delay,2]
    }
    list(samples=samples, targets=targets)
  }
}

build_matrix <- function(tseries, overall_timesteps) {
  t(sapply(1:(length(tseries) - overall_timesteps + 1), function(x)
    tseries[x:(x + overall_timesteps - 1)]))
}

lookback <- 96
step <- 1
delay <- 96
batch_size <- 1024

set1 <- nrow(dataTotal)/3
set2 <- set1*2
set3 <- set1*3

train_data <- dataTotal[1:set1,]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
data <- scale(dataTotal, center = mean, scale = std)


train_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = set1,
  shuffle = TRUE,
  step = step,
  batch_size = batch_size
)

val_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = set1,
  max_index = set2,
  step = step,
  batch_size = batch_size
)

test_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = set2,
  max_index = NULL,
  step = step,
  batch_size = batch_size
)

test<-dataTotal[set2:set3,]
# How many steps to draw from val_gen in order to see the entire validation set
val_steps <- (set2 - set1 - lookback) / batch_size

# How many steps to draw from test_gen in order to see the entire test set
test_steps <- (nrow(dataTotal) - set2 - lookback) / batch_size
#test_matrix <- build_matrix(test, 96)
#test_y <- as.matrix(test)
