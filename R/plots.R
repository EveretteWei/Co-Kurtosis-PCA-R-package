#' Plot Individual and Cumulative Contribution Ratios
#'
#' Generates side-by-side plots showing the individual contribution ratios of principal components and their cumulative contribution ratios.
#'
#' @param result The result of cokpca function containing:
#' \describe{
#'   \item{individual_contribution}{A numeric vector representing the individual contribution ratios of each principal component.}
#'   \item{cumulative_contribution}{A numeric vector representing the cumulative contribution ratios up to each principal component.}
#' }
#'
#' @return None. This function generates plots as a side effect.
#'
#' @details
#' The function creates two plots in a single plotting window:
#' \enumerate{
#'   \item **Individual Contribution Bar Plot**: Displays the contribution ratio of each principal component using a bar chart.
#'   \item **Cumulative Contribution Line Plot**: Shows the cumulative contribution ratio up to each principal component using a line plot.
#' }
#'
#' @examples
#' data = matrix(rnorm(100), nrow = 20)
#' result = cokpca(data, alpha = 0.85)
#'
#' # Generate contribution plots
#' contributions_plot(result)
#'
#' @importFrom graphics barplot par
#'
#' @export
#'
contributions_plot = function(result) {
  individual_contribution = result$individual_contribution
  cumulative_contribution = result$cumulative_contribution
  components = 1:length(individual_contribution)

  par(mfrow = c(1, 2))

  # Individual Contribution Bar Plot
  barplot(
    individual_contribution,
    names.arg = components,
    col = "steelblue",
    main = "Individual Contribution",
    xlab = "Principal Component",
    ylab = "Contribution Ratio"
  )

  # Cumulative Contribution Line Plot
  plot(
    components,
    cumulative_contribution,
    type = "b",
    col = "red",
    pch = 19,
    lty = 2,
    main = "Cumulative Contribution",
    xlab = "Principal Component",
    ylab = "Cumulative Contribution Ratio"
  )

  par(mfrow = c(1, 1))
}






#' Plot Loadings Matrix
#'
#' Generates a scatter plot of loadings for specified principal components, with arrows representing each variable's loading on the components.
#'
#' @param result The result of cokpca function containing:
#' \describe{
#'   \item{loadings}{A numeric matrix where rows represent variables and columns represent principal components.}
#' }
#' @param components An integer vector specifying which principal components to plot. Default is \code{c(1, 2)}.
#' @param variable_names (Optional) A character vector specifying the names of the variables. If \code{NULL}, existing row names of the loadings matrix are used, or default names are assigned.
#' @param title A string specifying the title of the plot. Default is \code{"Loadings Plot"}.
#'
#' @return None. This function generates a scatter plot as a side effect.
#'
#' @details
#' The function creates a plot of the loadings for the specified principal components. Arrows are drawn from the origin to each point, representing the loading of each variable on the components. A unit circle is also drawn for reference.
#'
#' @examples
#' data = matrix(rnorm(100), nrow = 20)
#' result = cokpca(data, alpha = 0.85)
#' loadings_plot(result)
#'
#' @importFrom graphics arrows text symbols
#'
#' @export
#'
loadings_plot = function(result, components = c(1, 2), variable_names = NULL, title = "Loadings Plot") {

  loadings = result$loadings

  # check if the components input is valid
  if (any(components > ncol(loadings)) || any(components < 1)) {
    stop("Specified components exceed the number of available components in the loadings matrix.")
  }

  loadings_subset = loadings[, components, drop = FALSE]

  # set variable names
  if (!is.null(variable_names)) {
    if (length(variable_names) != nrow(loadings)) {
      stop("Length of variable_names must match the number of variables (rows) in the loadings matrix.")
    }
    rownames(loadings_subset) = variable_names
  } else {
    if (is.null(rownames(loadings))) {
      rownames(loadings_subset) = paste0("Var", 1:nrow(loadings))
    } else {
      rownames(loadings_subset) = rownames(loadings)
    }
  }

  plot(
    loadings_subset[, 1], loadings_subset[, 2],
    xlab = paste("Component", components[1]),
    ylab = paste("Component", components[2]),
    main = title,
    xlim = c(min(loadings_subset[, 1]) - 0.1, max(loadings_subset[, 1]) + 0.1),
    ylim = c(min(loadings_subset[, 2]) - 0.1, max(loadings_subset[, 2]) + 0.1),
    type = "n"
  )
  arrows(0, 0, loadings_subset[, 1], loadings_subset[, 2], length = 0.1, angle = 20, col = "black")
  text(loadings_subset[, 1], loadings_subset[, 2], labels = rownames(loadings_subset), pos = 4, cex = 0.8)
  symbols(0, 0, circles = 1, inches = FALSE, add = TRUE, lty = 2)
}




#' Plot Original vs. Reconstructed Data
#'
#' Visualizes the comparison between original data and its reconstruction for a specified variable.
#'
#' @param df A matrix or data frame containing the original data, where rows represent observations and columns represent variables.
#' @param result the result of cokpca function containing:
#' \describe{
#'   \item{reconstructed_data}{A matrix or data frame containing the reconstructed data, matching the structure of \code{df}.}
#' }
#' @param k An integer specifying which variable's data to plot (i.e., which column in the data matrices). Default is \code{1}.
#'
#' @return None. This function generates a line plot as a side effect.
#'
#' @details
#' The function plots the original and reconstructed data for the specified variable \code{k}. It displays the original data in blue and the reconstructed data in red, with a legend indicating each.
#'
#' @examples
#' data = matrix(rnorm(100), nrow = 20, ncol = 5)
#' result = cokpca(data)
#' reconstruction_plot(df = data, result = result, k = 2)
#'
#' @importFrom graphics lines legend
#'
#' @export
#'
reconstruction_plot = function(df, result, k = 1){
  if (!is.matrix(df) && !is.data.frame(df)) {
    stop("Input data must be a matrix or data frame.")
  }
  original = as.matrix(df)
  reconstructed = result$reconstructed_data
  num_columns = ncol(original)
  if (!is.numeric(k) || length(k) != 1 || k <= 0 || k != as.integer(k)) {
    stop("Parameter k must be a single positive integer.")
  }
  if (k > num_columns) {
    stop(paste("Parameter k exceeds the number of columns in the data. The data has", num_columns, "columns."))
  }
  par(mfrow = c(1, 1))
  plot(original[, k], type = "l", col = "blue", main = "Original vs Reconstructed", ylab = "Values")
  lines(reconstructed[, k], col = "red")
  legend("topright", legend = c("Original", "Reconstructed"), col = c("blue", "red"), lty = 1)
}
