#' Build an Artificial Neural Network (ANN) Model for reconstruction
#'
#' Constructs and compiles a sequential ANN model using the \code{keras3} package. TThe model allows users to define the number and configuration of hidden layers, the output layer activation function, and the learning rate for the optimizer.
#'
#' @param input_dim An integer specifying the number of input features.
#' @param output_dim An integer specifying the number of units in the output layer.
#' @param hidden_layers A list of named lists, where each named list specifies:
#' \describe{
#'   \item{\code{units}}{An integer specifying the number of units in the layer.}
#'   \item{\code{activation}}{A string specifying the activation function for the layer (e.g., \code{"tanh"}, \code{"relu"}, etc.).}
#' }
#' The default configuration includes:
#' \enumerate{
#'   \item Dense layer with 40 units and \code{"tanh"} activation.
#'   \item Dense layer with 64 units and \code{"tanh"} activation.
#'   \item Dense layer with 40 units and \code{"tanh"} activation.
#'   \item Dense layer with 32 units and \code{"tanh"} activation.
#' }
#' @param learning_rate A numeric value specifying the learning rate for the \code{adam} optimizer. Default is \code{0.001}.
#' @param output_activation A string specifying the activation function for the output layer (e.g., \code{"sigmoid"}, \code{"tanh"}, or \code{NULL} for no activation). Default is \code{NULL}.
#'
#' @return A compiled \code{keras} model ready for training.
#'
#' @details
#' The function builds a customable sequential ANN model where users can:
#' \enumerate{
#'   \item Specify the architecture of hidden layers, including the number of units and activation functions for each layer.
#'   \item Define the activation function for the output layer to suit regression or classification tasks.
#'   \item Adjust the learning rate for the \code{adam} optimizer to fine-tune the training process.
#' }
#' The model is compiled with \code{"mean_squared_error"} loss and \code{"adam"} optimizer, and it tracks \code{"mean_absolute_error"} as a metric.
#'
#' @import keras3
#'
#' @note
#' This function uses 'keras3' for ANN reconstruction. If 'keras3' is not installed,
#' please install it using:
#' \code{install.packages("keras3")}
#' Alternatively, you may use 'linear' reconstruction, which does not require 'keras3'.
#'
build_ann = function(input_dim, output_dim,
                     hidden_layers = list(
                       list(units = 40, activation = "tanh"),
                       list(units = 64, activation = "tanh"),
                       list(units = 40, activation = "tanh"),
                       list(units = 32, activation = "tanh")
                     ),
                     learning_rate = 0.001, output_activation = NULL){

  inputs = layer_input(shape = c(input_dim))

  if (!all(sapply(hidden_layers, function(layer) all(c("units", "activation") %in% names(layer))))) {
    stop("Each layer in `hidden_layers` must be a named list with `units` and `activation`.")
  }

  x = inputs

  for (layer in hidden_layers) {
    units = layer$units
    activation = layer$activation
    x = x %>% layer_dense(units = units, activation = activation)
  }
  outputs = x %>% layer_dense(units = output_dim, activation = output_activation)

  model = keras3::keras_model(inputs = inputs, outputs = outputs)

  model %>% compile(
    optimizer = optimizer_adam(learning_rate = learning_rate),
    loss = "mean_squared_error", metrics = c("mean_absolute_error")
  )

  return(model)
}
