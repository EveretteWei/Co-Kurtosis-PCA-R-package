#' Compute Scores for Principal Components
#'
#' Calculates the scores for selected principal components.
#' @param data A numeric matrix of scaled data.
#' @param Uk A matrix of the top `k` left singular vectors obtained from the `reduction` function.
#'
#' @return A numeric matrix of scores.
#'
scores = function(data, Uk){
  Score = data%*%Uk
  return(Score)
}
