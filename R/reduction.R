#' Dimensionality Reduction
#'
#' Reduces the dimension of the data.
#'
#' @param U A matrix of left singular vectors obtained from Singular Vector Decomposition.
#' @param D A numeric vector of singular values obtained from Singular Vector Decomposition.
#' @param V A matrix of right singular vectors obtained from Singular Vector Decomposition.
#' @param k (Optional) An integer specifying the number of principal components to retain. Must be a positive integer no more than the length of `D`.
#' @param alpha (Optional) A numerical value in [0,1] specifying the cumulative contribution ratio to determine the number of principal components to retain.
#'
#' @return A list containing:
#' \describe{
#'   \item{Uk}{A matrix of the top `k` left singular vectors.}
#'   \item{Dk}{A diagonal matrix of the top `k` singular values.}
#'   \item{Vk}{A matrix of the top `k` right singular vectors.}
#'   \item{k}{The number of principal components selected.}
#'   \item{alpha}{The cumulative contribution ratio achieved with the selected `k` components.}
#' }
#'
reduction = function(U, D, V, k = NULL, alpha = NULL) {
  if (!is.numeric(D) || any(D < 0)) stop("D must be a non-negative numeric vector.")
  if (!is.null(k) && (!is.numeric(k) || k <= 0 || k > length(D))) {
    stop("Invalid k: must be a positive integer less than or equal to the length of D.")
  }
  if (!is.null(alpha) && (!is.numeric(alpha) || alpha <= 0 || alpha > 1)) {
    stop("Invalid alpha: must be a numeric value in the range (0, 1].")
  }
  if (!is.null(k) && !is.null(alpha)) stop("You cannot specify both k and alpha.")

  contribution = D / sum(D)

  if (!is.null(alpha)) {
    k = which(cumsum(contribution) >= alpha)[1]
  } else if (is.null(k)) {
    k = which(cumsum(contribution) >= 0.8)[1]
    alpha = sum(contribution[1:k])
  } else {
    alpha = sum(contribution[1:k])
  }

  Uk = U[, 1:k, drop = FALSE]
  Dk = diag(D[1:k])
  Vk = V[, 1:k, drop = FALSE]
  return(list(Uk = Uk, Dk = Dk, Vk = Vk, k = k, alpha = alpha))
}
