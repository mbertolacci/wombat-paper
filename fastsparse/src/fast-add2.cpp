#include <Rcpp.h>

using Rcpp::IntegerVector;
using Rcpp::List;
using Rcpp::NumericVector;
using Rcpp::S4;

// Fast addition for general + general and symmetric + symmetric
// [[Rcpp::export(name=".fast_add2", rng=false)]]
S4 fast_add2(S4 a, S4 b) {
    if (a.inherits("dsCMatrix") && b.inherits("dsCMatrix")) {
        std::string aUplo = a.slot("uplo");
        std::string bUplo = b.slot("uplo");
        if (aUplo != bUplo) {
            Rcpp::stop("a and b must use same upper triangle representation");
        }
    } else if (a.inherits("dsCMatrix") || b.inherits("dsCMatrix")) {
        Rcpp::stop("Symmetric plus general not supported");
    }

    IntegerVector aDims(a.slot("Dim"));
    IntegerVector bDims(b.slot("Dim"));

    if (aDims[0] != bDims[0] || aDims[1] != bDims[1]) {
        Rcpp::stop("Matrix dimensions do not match");
    }

    IntegerVector aI_(a.slot("i"));
    int *aI = &aI_[0];
    IntegerVector aP_(a.slot("p"));
    int *aP = &aP_[0];
    NumericVector aX_(a.slot("x"));
    double *aX = &aX_[0];

    IntegerVector bI_(b.slot("i"));
    int *bI = &bI_[0];
    IntegerVector bP_(b.slot("p"));
    int *bP = &bP_[0];
    NumericVector bX_(b.slot("x"));
    double *bX = &bX_[0];

    int nRows = aDims[0];
    int nColumns = aDims[1];

    // Compute P
    IntegerVector outputP_ = Rcpp::no_init(nColumns + 1);
    int *outputP = &outputP_[0];
    outputP[0] = 0;
    for (int col = 0; col < nColumns; ++col) {
        int aK = aP[col];
        int bK = bP[col];
        outputP[col + 1] = outputP[col];

        while (aK < aP[col + 1] || bK < bP[col + 1]) {
            if (aK == aP[col + 1]) {
                // a has reached end of column, advance b
                ++bK;
            } else if (bK == bP[col + 1]) {
                // b has reached end of column, advance a
                ++aK;
            } else if (aI[aK] < bI[bK]) {
                // a is behind b, advance a
                ++aK;
            } else if (bI[bK] < aI[aK]) {
                // b is behind a, advance b
                ++bK;
            } else {
                // they are at the same row, advance both
                ++aK;
                ++bK;
            }
            ++outputP[col + 1];
        }
    }

    // Compute sum
    IntegerVector outputI_ = Rcpp::no_init(outputP[nColumns]);
    int *outputI = &outputI_[0];
    NumericVector outputX_ = Rcpp::no_init(outputP[nColumns]);
    double *outputX = &outputX_[0];
    for (int col = 0; col < nColumns; ++col) {
        int aK = aP[col];
        int bK = bP[col];

        int outputK = outputP[col];
        while (aK < aP[col + 1] || bK < bP[col + 1]) {
            if (aK == aP[col + 1]) {
                // a has reached end of column, advance b
                outputX[outputK] = bX[bK];
                outputI[outputK] = bI[bK];
                ++bK;
            } else if (bK == bP[col + 1]) {
                // b has reached end of column, advance a
                outputX[outputK] = aX[aK];
                outputI[outputK] = aI[aK];
                ++aK;
            } else if (aI[aK] < bI[bK]) {
                // a is behind b, advance a
                outputX[outputK] = aX[aK];
                outputI[outputK] = aI[aK];
                ++aK;
            } else if (bI[bK] < aI[aK]) {
                // b is behind a, advance b
                outputX[outputK] = bX[bK];
                outputI[outputK] = bI[bK];
                ++bK;
            } else {
                // they are at the same row, advance both
                outputX[outputK] = aX[aK] + bX[bK];
                outputI[outputK] = bI[bK];
                ++aK;
                ++bK;
            }
            ++outputK;
        }
    }

    S4 output;
    if (a.inherits("dsCMatrix")) {
        output = S4("dsCMatrix");
        output.slot("uplo") = a.slot("uplo");
    } else {
        output = S4("dgCMatrix");
    }
    output.slot("i") = outputI_;
    output.slot("x") = outputX_;
    output.slot("p") = outputP_;
    output.slot("Dim") = IntegerVector({ nRows, nColumns });
    output.slot("factors") = List();
    output.slot("Dimnames") = List({ R_NilValue, R_NilValue });

    return output;
}
