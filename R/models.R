#' Function to generate deepSDF Keras model
#'
#' @param input_len
#' @param net_breadth
#' @param dropout_rate
#'
#' @return
#' @export
#'
#' @examples
deepSDF_model <- function(input_len, net_breadth = 256L, dropout_rate = 0.5) {

  coord_input <- keras::layer_input(shape = c(input_len), dtype = "float32", name = "coord_input")

  first_block <- coord_input %>%
    keras::layer_dense(units = net_breadth) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_leaky_relu() %>%
    keras::layer_dropout(rate = dropout_rate) %>%
    keras::layer_dense(units = net_breadth) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_leaky_relu() %>%
    keras::layer_dropout(rate = dropout_rate) %>%
    keras::layer_dense(units = net_breadth) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_leaky_relu() %>%
    keras::layer_dropout(rate = dropout_rate) %>%
    keras::layer_dense(units = net_breadth) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_leaky_relu() %>%
    keras::layer_dropout(rate = dropout_rate)

  sdf_output <- keras::layer_concatenate(list(first_block, coord_input)) %>%
    keras::layer_dense(units = net_breadth) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_leaky_relu() %>%
    keras::layer_dropout(rate = dropout_rate) %>%
    keras::layer_dense(units = net_breadth) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_leaky_relu() %>%
    keras::layer_dropout(rate = dropout_rate) %>%
    keras::layer_dense(units = net_breadth) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_leaky_relu() %>%
    keras::layer_dropout(rate = dropout_rate) %>%
    keras::layer_dense(units = net_breadth) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_leaky_relu() %>%
    keras::layer_dropout(rate = dropout_rate) %>%
    keras::layer_dense(units = 1L, activation = "tanh")

  model <- keras::keras_model(
    inputs = coord_input,
    outputs = sdf_output
  )

  model
}


#' Function to run the model on a set of data
#' @param train_dat
#' @param validation_dat
#' @param test_dat
#' @param callbacks
#' @param epochs
#' @param batch_size
#'
#' @return
#' @export
#'
#' @examples
run_model <- function(train_dat, validation_dat = NULL, test_dat = NULL,
                      epochs, batch_size) {

  train_x <- train_dat$train$x
  train_y <- train_dat$train$y

  if(!is.null(validation_dat)) {
    validation_list <- list(validation_dat$x, validation_dat$y)
  } else {
    validation_list <- NULL
  }

  model %>% keras::fit(
    x = train_x,
    y = train_y,
    validation_data = validation_list,
    epochs = epochs,
    batch_size = batch_size,
    view_metrics = TRUE,
    callbacks = callbacks
  )


}
