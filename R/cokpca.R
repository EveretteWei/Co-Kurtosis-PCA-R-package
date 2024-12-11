#' Co-Kurtosis PCA
#'
#' Performs Co-Kurtosis PCA on the provided dataset with options for data reconstruction methods (linear or ANN).
#'
#' @param df A matrix or dataframe of input data.
#' @param k (Optional) Number of principal components to retain in reduction.
#' @param alpha (Optional) Cumulative contribution ratio to determine the number of components.
#' @param center Logical indicating whether to center the data before scaling. Default is `TRUE`.
#' @param scale Logical indicating whether to scale the data to unit variance. Default is `TRUE`.
#' @param method Method for reconstruction: `"linear"` or `"ann"`. Default is `"linear"`.
#' @param epochs Number of epochs for ANN training (default is `100`).
#' @param batch_size Batch size for ANN training (default is `32`).
#' @param set_seed The random seed for ANN model reproducibility. Default is `1234`.
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
#' @param learning_rate A numeric value specifying the learning rate for the ANN optimizer. Default is `0.001`.
#' @param output_activation A string specifying the activation function for the output layer (e.g., \code{"sigmoid"}, \code{"tanh"}, or \code{NULL} for no activation). Default is `NULL`.
#'
#' @return A list containing:
#' \describe{
#'   \item{scores}{Principal component scores matrix.}
#'   \item{loadings}{Matrix of principal component loadings.}
#'   \item{k}{The number of principal components selected.}
#'   \item{alpha}{The cumulative contribution ratio achieved with the selected `k` components.}
#'   \item{reconstructed_data}{Reconstructed data matrix.}
#'   \item{individual_contribution}{Individual contribution ratios of each principal component.}
#'   \item{cumulative_contribution}{Cumulative contribution ratios up to each principal component.}
#'   \item{ann_model}{(Optional) The trained ANN model if `method` is `"ann"`.}
#'   \item{training_history}{(Optional) The training history of the ANN model if `method` is `"ann"`.}
#' }
#'
#' @details
#' This function performs dimensionality reduction using Co-Kurtosis PCA and provides options for reconstruction via a linear method or an Artificial Neural Network (ANN). When using the ANN reconstruction method, users can customize the architecture by defining the number and configuration of hidden layers, the optimizer learning rate, and the output layer activation function.
#'
#' If the `linear` reconstruction method is chosen, the function performs simple linear reconstruction using the principal component scores and loadings.
#'
#' If the `ann` reconstruction method is chosen, the function requires the \code{keras3} package. Ensure \code{keras3} is installed before running the function.
#'
#' @note
#' If `method = "ann"`, this function uses the \code{keras3} package for ANN reconstruction. If \code{keras3} is not installed, please install it using:
#' \code{install.packages("keras3")}.
#' Alternatively, you may use \code{method = "linear"} for reconstruction without requiring \code{keras3}.
#'
#' @examples
#' # Example 1: Using linear reconstruction
#' df =  matrix(rnorm(100), nrow = 20, ncol = 5)
#' result = cokpca(df, k = 2, center = TRUE, scale = TRUE, method = "linear")
#'
#' # Example 2: Using ANN reconstruction with default ANN parameters
#' result = cokpca(df, k = 2, center = TRUE, scale = TRUE, method = "ann")
#'
#' # Example 3: Using ANN reconstruction with a custom architecture and parameters
#' result = cokpca(
#'   df = df, k = 2, center = TRUE, scale = TRUE, method = "ann",
#'   hidden_layers = list(
#'     list(units = 128, activation = "relu"),
#'     list(units = 64, activation = "relu")
#'   ),
#'   learning_rate = 0.0001,
#'   output_activation = "tanh",
#'   epochs = 50,
#'   batch_size = 64
#' )
#' @importFrom stats predict
#'
#' @export
#'
cokpca = function(df, k = NULL, alpha = NULL, center = TRUE, scale = TRUE,
                  method = "linear", epochs = 100, batch_size = 32, set_seed = 1234,
                  hidden_layers = list(
                    list(units = 40, activation = "tanh"),
                    list(units = 64, activation = "tanh"),
                    list(units = 40, activation = "tanh"),
                    list(units = 32, activation = "tanh")
                  ),
                  learning_rate = 0.001, output_activation = NULL) {
  if (!is.matrix(df) && !is.data.frame(df)) {
    stop("Input data must be a matrix or data frame.")
  }
  Dat = as.matrix(df)

  # Standardization with optional centering and scaling
  data = scale(Dat, center = center, scale = scale)

  # Retrieve scaling attributes if scaling was performed
  col_means = if (center) attr(data, "scaled:center") else NULL
  col_sds = if (scale) attr(data, "scaled:scale") else NULL

  if(scale && any(col_sds == 0))
    stop("Cannot rescale a constant/zero column to unit variance")

  K = cumulant_tensor_cpp(data)

  Tm = mode1unfold(K)

  svd_result = svd(Tm)
  U = svd_result$u
  D = svd_result$d
  V = svd_result$v

  reduction_result = reduction(U, D, V, k = k, alpha = alpha)
  Uk = reduction_result$Uk
  Dk = reduction_result$Dk
  Vk = reduction_result$Vk
  k_selected = reduction_result$k
  alpha_selected = reduction_result$alpha

  Score = scores(data, Uk)

  if (method == "linear") {#linear
    X_reconstructed = reconstruct(Score, Uk, col_means, col_sds)
  }
  else if (method == "ann") {#ANN
    if (!requireNamespace("keras3", quietly = TRUE)) {
      stop("The 'keras3' package is required for ANN reconstruction but is not installed. ",
           "Please install it with install.packages('keras3').")
    }
    set.seed(set_seed)
    train_idx = sample(1:nrow(Score), size = 0.8 * nrow(Score))
    x_train = Score[train_idx, ]
    x_test = Score[-train_idx, ]
    y_train = Dat[train_idx, ]
    y_test = Dat[-train_idx, ]

    model = model = build_ann(
      input_dim = ncol(Score),
      output_dim = ncol(data),
      hidden_layers = hidden_layers,
      learning_rate = learning_rate,
      output_activation = output_activation
    )

    history = model %>% fit(
      x = x_train, y = y_train,
      epochs = epochs, batch_size = batch_size,
      validation_split = 0.2
    )

    X_reconstructed = model %>% predict(Score)
  }
  else {
    stop("Invalid method. Choose 'linear' or 'ann'.")
  }

  contr_result = contribution(D)
  individual_contribution = contr_result$individual_contribution
  cumulative_contribution = contr_result$cumulative_contribution


  return(list(
    scores = Score,
    loadings = U,
    k = k_selected,
    alpha = alpha_selected,
    reconstructed_data = X_reconstructed,
    individual_contribution = individual_contribution,
    cumulative_contribution = cumulative_contribution,
    ann_model = if (method == "ann") model else NULL,
    training_history = if (method == "ann") history else NULL
  ))
}
