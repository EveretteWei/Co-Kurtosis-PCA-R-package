#' Linear reconstruction of data
#'
#' Reconstructs the original data from the principal component scores and loadings using a linear method.
#' @param Score A numeric matrix of principal component scores, typically obtained from the `scores` function.
#' @param Uk A matrix of the top `k` left singular vectors (loadings) obtained from the `reduction` function.
#' @param col_means A numeric vector containing the means of the original data columns, obtained from the `scale` function.
#' @param col_sds A numeric vector containing the standard deviations of the original data columns, obtained from the `scale` function.
#'
#' @return A numeric matrix representing the reconstructed data using linear reconstruction.
#'
reconstruct = function(Score, Uk, col_means = NULL, col_sds = NULL){
  data_reconstructed = Score %*% t(Uk)
  # If scaling was performed, multiply by the column standard deviations
  if (!is.null(col_sds)) {
    data_reconstructed = sweep(data_reconstructed, 2, col_sds, FUN = "*")
  }

  # If centering was performed, add the column means
  if (!is.null(col_means)) {
    data_reconstructed = sweep(data_reconstructed, 2, col_means, FUN = "+")
  }
  return(data_reconstructed)
}
