#include <RcppEigen.h>
#include <R_ext/Lapack.h>

using Rcpp::List;
using Rcpp::NumericMatrix;
using Rcpp::NumericVector;
using Rcpp::S4;
using Rcpp::Named;

// [[Rcpp::export(rng=false)]]
List spdt_cholesky(NumericVector diag, NumericVector subDiag) {
    int n = diag.size();
    NumericVector diagP = Rcpp::clone(diag);
    NumericVector subDiagP = Rcpp::clone(subDiag);
    int info;

    F77_NAME(dpttrf)(
        &n,
        diagP.begin(),
        subDiagP.begin(),
        &info
    );

    return List::create(
        Named("major") = diagP,
        Named("minor") = subDiagP
    );
}

// [[Rcpp::export(rng=false)]]
List spdt_cholesky_obj(S4 x) {
    List factors = x.slot("factors");
    if (factors.containsElementNamed("ldl")) {
        return factors["ldl"];
    }

    List ldl = spdt_cholesky(x.slot("major"), x.slot("minor"));
    NumericVector a = ldl["major"];
    double logDeterminant = Rcpp::sum(Rcpp::log(a));
    ldl["log_determinant"] = logDeterminant;

    factors["ldl"] = ldl;
    x.slot("factors") = factors;

    return ldl;
}

// [[Rcpp::export(rng=false)]]
NumericVector spdt_mul_vec(NumericVector diag, NumericVector subDiag, NumericVector rhs, int nRhs) {
    int n = diag.size();
    NumericVector output(rhs.size());

    for (int k = 0; k < nRhs; ++k) {
        output[n * k + 0] = (
            diag[0] * rhs[n * k + 0]
            + subDiag[0] * rhs[n * k + 1]
        );
        int i = 1;
        for (; i < n - 1; ++i) {
            output[n * k + i] = (
                diag[i] * rhs[n * k + i]
                + subDiag[i - 1] * rhs[n * k + i - 1]
                + subDiag[i] * rhs[n * k + i + 1]
            );
        }
        output[n * k + i] = (
            diag[i] * rhs[n * k + i]
            + subDiag[i - 1] * rhs[n * k + i - 1]
        );
    }

    return output;
}

// [[Rcpp::export(rng=false)]]
NumericVector spdt_mul_mat(NumericVector diag, NumericVector subDiag, NumericMatrix rhs) {
    int n = diag.size();
    NumericMatrix output(rhs.nrow(), rhs.ncol());

    for (int k = 0; k < rhs.ncol(); ++k) {
        output(0, k) = (
            diag[0] * rhs(0, k)
            + subDiag[0] * rhs(1, k)
        );
        int i = 1;
        for (; i < n - 1; ++i) {
            output(i, k) = (
                diag[i] * rhs(i, k)
                + subDiag[i - 1] * rhs(i - 1, k)
                + subDiag[i] * rhs(i + 1, k)
            );
        }
        output(i, k) = (
            diag[i] * rhs(i, k)
            + subDiag[i - 1] * rhs(i - 1, k)
        );
    }

    return output;
}

// [[Rcpp::export(rng=false)]]
NumericVector spdt_ldl_solve_vec(NumericVector diag, NumericVector subDiag, NumericVector rhs, int nRhs) {
    int n = diag.size();
    NumericVector diagP = Rcpp::clone(diag);
    NumericVector subDiagP = Rcpp::clone(subDiag);
    NumericVector rhsP = Rcpp::clone(rhs);
    int info;

    F77_NAME(dpttrs)(
        &n,
        &nRhs,
        diagP.begin(),
        subDiagP.begin(),
        rhsP.begin(),
        &n,
        &info
    );

    return rhsP;
}

// [[Rcpp::export(rng=false)]]
NumericMatrix spdt_ldl_solve_mat(NumericVector diag, NumericVector subDiag, NumericMatrix rhs) {
    int n = diag.size();
    int nRhs = rhs.ncol();
    NumericVector diagP = Rcpp::clone(diag);
    NumericVector subDiagP = Rcpp::clone(subDiag);
    NumericMatrix rhsP = Rcpp::clone(rhs);
    int info;

    F77_NAME(dpttrs)(
        &n,
        &nRhs,
        diagP.begin(),
        subDiagP.begin(),
        rhsP.begin(),
        &n,
        &info
    );

    return rhsP;
}
