#' Compute Individual and Cumulative Contribution Ratios
#'
#' Calculates the individual and cumulative contribution ratios based on the provided vector of singular values or eigenvalues.
#'
#' @param D A numeric vector representing singular values or eigenvalues.
#'
#' @return A list containing:
#' \describe{
#'   \item{individual_contribution}{A numeric vector of individual contribution ratios.}
#'   \item{cumulative_contribution}{A numeric vector of cumulative contribution ratios.}
#' }
#'
contribution = function(D){
  contr = D/sum(D)
  cumulative_contr = cumsum(contr)
  return(list(individual_contribution = contr, cumulative_contribution = cumulative_contr))
}
