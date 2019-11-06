#include <Rcpp.h>
#include <unordered_set>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
NumericVector sortUniqueC(NumericVector x) {
  std::unordered_set<int> seen;
  int n = x.size();
  std::vector<double> out;

  for (int i = 0; i < n; ++i) {
    if (seen.insert(x[i]).second) out.push_back(x[i]);
  }

  std::sort(out.begin(), out.end());

  return wrap(out);
}
