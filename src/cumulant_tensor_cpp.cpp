#include <Rcpp.h>
using namespace Rcpp;
//' Computes the cumulant tensor for a data matrix.
//'
//' This function calculates the co-kurtosis cumulant tensor for the given input matrix.
//' It uses second-order statistics (covariance) and higher-order tensor computations.
//'
//' @param data A numeric matrix (n x p), where rows are observations and columns are variables.
//' @return A 4-dimensional cumulant tensor (p x p x p x p) as a NumericVector with dimensions assigned.
//'
// [[Rcpp::export]]
NumericVector cumulant_tensor_cpp(NumericMatrix data) {
  int n = data.nrow();
  int p = data.ncol();

  NumericMatrix C(p, p);
  for (int i = 0; i < p; ++i) {
    for (int j = i; j < p; ++j) {
      double sum = 0.0;
      for (int k = 0; k < n; ++k) {
        sum += data(k, i) * data(k, j);
      }
      C(i, j) = sum / (n - 1);
      C(j, i) = C(i, j);
    }
  }

  NumericVector tensor_T(p * p * p * p, 0.0);
  NumericVector tensor_K(p * p * p * p, 0.0);

  for (int i = 0; i < p; ++i) {
    for (int j = 0; j < p; ++j) {
      // Compute outer_product
      std::vector<double> outer_product(n, 0.0);
      for (int k = 0; k < n; ++k) {
        outer_product[k] = data(k, i) * data(k, j);
      }
      // Compute crossprod(data, (outer_product * data))
      for (int k = 0; k < p; ++k) {
        for (int l = 0; l < p; ++l) {
          double sum = 0.0;
          for (int m = 0; m < n; ++m) {
            sum += data(m, k) * outer_product[m] * data(m, l);
          }
          sum /= n;
          int idx = ((i * p + j) * p + k) * p + l;
          tensor_T[idx] = sum;
        }
      }
    }
  }

  for (int i = 0; i < p; ++i) {
    for (int j = 0; j < p; ++j) {
      for (int k = 0; k < p; ++k) {
        for (int l = 0; l < p; ++l) {
          int idx = ((i * p + j) * p + k) * p + l;
          tensor_K[idx] = tensor_T[idx]
          - C(i, j) * C(k, l)
            - C(i, k) * C(j, l)
            - C(i, l) * C(j, k);
        }
      }
    }
  }

  // Assign dimensions to the tensor_K
  IntegerVector dims = IntegerVector::create(p, p, p, p);
  tensor_K.attr("dim") = dims;

  return tensor_K;
}
