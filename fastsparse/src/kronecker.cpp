#include <Rcpp.h>

using Rcpp::IntegerVector;
using Rcpp::List;
using Rcpp::NumericVector;
using Rcpp::S4;

// Fast kronecker with general sparse matrices
// [[Rcpp::export(name=".fast_kronecker", rng=false)]]
S4 fast_kronecker(S4 a, S4 b) {
    IntegerVector aDims(a.slot("Dim"));
    IntegerVector aI(a.slot("i"));
    IntegerVector aP(a.slot("p"));
    NumericVector aX(a.slot("x"));

    IntegerVector bDims(b.slot("Dim"));
    IntegerVector bI(b.slot("i"));
    IntegerVector bP(b.slot("p"));
    NumericVector bX(b.slot("x"));

    int nRows = aDims[0] * bDims[0];
    int nColumns = aDims[1] * bDims[1];
    NumericVector outputX(aX.size() * bX.size());
    IntegerVector outputI(aX.size() * bX.size());
    IntegerVector outputP(nColumns + 1);

    int outputK = 0;
    outputP[0] = 0;
    for (int outputCol = 0; outputCol < nColumns; ++outputCol) {
        int aCol = outputCol / bDims[1];
        int bCol = outputCol % bDims[1];

        // Each row in this column of a
        for (int aK = aP[aCol]; aK < aP[aCol + 1]; ++aK) {
            int aRow = aI[aK];
            // Each row in this column of b
            for (int bK = bP[bCol]; bK < bP[bCol + 1]; ++bK) {
                int bRow = bI[bK];

                outputX[outputK] = aX[aK] * bX[bK];
                outputI[outputK] = aRow * bDims[0] + bRow;
                ++outputK;
            }
        }

        outputP[outputCol + 1] = outputK;
    }

    S4 output("dgCMatrix");
    output.slot("Dim") = IntegerVector({ nRows, nColumns });
    output.slot("x") = outputX;
    output.slot("i") = outputI;
    output.slot("p") = outputP;
    output.slot("factors") = List();
    output.slot("Dimnames") = List({ R_NilValue, R_NilValue });

    return output;
}

// Count the number of non-zero elements on the diagonal
int nnzero_diag(S4 x) {
    IntegerVector xDims(x.slot("Dim"));
    IntegerVector xI(x.slot("i"));
    IntegerVector xP(x.slot("p"));

    int count = 0;
    for (int xCol = 0; xCol < xDims[1]; ++xCol) {
        if (xP[xCol + 1] == xP[xCol]) continue;
        if (xI[xP[xCol + 1] - 1] == xCol) ++count;
    }
    return count;
}

// Fast kronecker when a and b are symmetrical
// [[Rcpp::export(name=".fast_kronecker_sym", rng=false)]]
S4 fast_kronecker_sym(S4 a, S4 b) {
    std::string aUplo = a.slot("uplo");
    std::string bUplo = b.slot("uplo");
    if (aUplo != "U" || bUplo != "U") {
        Rcpp::stop("Both a and b must use upper triangle representation");
    }

    IntegerVector aDims(a.slot("Dim"));
    IntegerVector aI(a.slot("i"));
    IntegerVector aP(a.slot("p"));
    NumericVector aX(a.slot("x"));

    IntegerVector bDims(b.slot("Dim"));
    IntegerVector bI(b.slot("i"));
    IntegerVector bP(b.slot("p"));
    NumericVector bX(b.slot("x"));

    int nnzA = aX.size();
    int nnzB = bX.size();
    int nnzADiag = nnzero_diag(a);
    int nnzBDiag = nnzero_diag(b);

    // Count the number of non-zero entries in each row of B
    std::vector<int> nnzBelowDiagB(bDims[0]);
    for (int bCol = 0; bCol < bDims[1]; ++bCol) {
        for (int bK = bP[bCol]; bK < bP[bCol + 1]; ++bK) {
            if (bCol == bI[bK]) continue;
            ++nnzBelowDiagB[bI[bK]];
        }
    }

    int nRows = aDims[0] * bDims[0];
    int nColumns = aDims[1] * bDims[1];
    int outputSize = nnzADiag * nnzB + (nnzA - nnzADiag) * (nnzBDiag + 2 * (nnzB - nnzBDiag));

    NumericVector outputX(outputSize);
    IntegerVector outputI(outputSize);
    IntegerVector outputP(nColumns + 1);

    // Compute outputP
    outputP[0] = 0;
    for (int outputCol = 0; outputCol < nColumns; ++outputCol) {
        int aCol = outputCol / bDims[1];
        int nnzACol = aP[aCol + 1] - aP[aCol];

        int n;
        if (nnzACol == 0) {
            n = 0;
        } else {
            int nnzAColDiag;
            int nnzAColAboveDiag;
            int lastARow = aI[aP[aCol + 1] - 1];
            if (lastARow == aCol) {
                nnzAColDiag = 1;
                nnzAColAboveDiag = aP[aCol + 1] - aP[aCol] - 1;
            } else {
                nnzAColDiag = 0;
                nnzAColAboveDiag = aP[aCol + 1] - aP[aCol];
            }

            int bCol = outputCol % bDims[1];
            int nnzBColUpper = bP[bCol + 1] - bP[bCol];
            int nnzBColLower = nnzBelowDiagB[bCol];

            n = nnzAColDiag * nnzBColUpper + nnzAColAboveDiag * (nnzBColUpper + nnzBColLower);
        }

        outputP[outputCol + 1] = outputP[outputCol] + n;
    }

    std::vector<int> bBelowDiagSoFar(bDims[0]);
    for (int aCol = 0; aCol < aDims[1]; ++aCol) {
        for (int aK = aP[aCol]; aK < aP[aCol + 1]; ++aK) {
            int aRow = aI[aK];
            std::fill(bBelowDiagSoFar.begin(), bBelowDiagSoFar.end(), 0);
            for (int bCol = 0; bCol < bDims[1]; ++bCol) {
                for (int bK = bP[bCol]; bK < bP[bCol + 1]; ++bK) {
                    int bRow = bI[bK];

                    double value = aX[aK] * bX[bK];

                    int outputCol = aCol * bDims[1] + bCol;
                    int outputRow = aRow * bDims[0] + bRow;
                    int outputK = (
                        // Start of column
                        outputP[outputCol]
                        // Number of rows before it
                        + (aK - aP[aCol]) * (bP[bCol + 1] - bP[bCol] + nnzBelowDiagB[bCol])
                        // How far into this row of b so far
                        + bK - bP[bCol]
                    );
                    outputX[outputK] = value;
                    outputI[outputK] = outputRow;

                    if (aCol != aRow && bCol != bRow) {
                        // Off a diagonal, so need to reflect b
                        outputCol = aCol * bDims[1] + bRow;
                        outputRow = aRow * bDims[0] + bCol;
                        outputK = (
                            // Start of column
                            outputP[outputCol]
                            // Number of rows before it
                            + (aK - aP[aCol]) * (bP[bRow + 1] - bP[bRow] + nnzBelowDiagB[bRow])
                            // Offset into the lower triangle
                            + bP[bRow + 1] - bP[bRow]
                            // How far into this col of b so far
                            + bBelowDiagSoFar[bRow]
                        );
                        outputX[outputK] = value;
                        outputI[outputK] = outputRow;

                        ++bBelowDiagSoFar[bRow];
                    }
                }

            }
        }
    }

    S4 output("dsCMatrix");
    output.slot("Dim") = IntegerVector({ nRows, nColumns });
    output.slot("x") = outputX;
    output.slot("i") = outputI;
    output.slot("p") = outputP;
    output.slot("uplo") = "U";
    output.slot("factors") = List();
    output.slot("Dimnames") = List({ R_NilValue, R_NilValue });

    return output;
}
