#include <RcppEigen.h>

using Rcpp::IntegerVector;
using Rcpp::List;
using Rcpp::NumericVector;
using Rcpp::S4;

using Eigen::MatrixXd;
typedef Eigen::SparseMatrix<double> MatrixSd;
typedef Eigen::Map<MatrixSd> MapSd;
typedef Eigen::Map<MatrixXd> MapXd;

void checkCompatibility(const S4& a, const S4& b) {
    IntegerVector aDim = a.slot("Dim");
    IntegerVector bDim = b.slot("Dim");

    if (!Rcpp::is_true(Rcpp::all(aDim == bDim))) {
        Rcpp::stop("Matrices have incompatible dimensions");
    }
}

bool isMatrixSparse(const S4& a) {
    return a.is("CsparseMatrix");
}

bool isMatrixDense(const S4& a) {
    return a.is("dgeMatrix") || a.is("dsyMatrix");
}

bool isMatrixDiagonal(const S4& a) {
    return a.is("ddiMatrix");
}

bool isMatrixSymmetric(const S4& a) {
    return a.is("dsyMatrix") || a.is("dsCMatrix");
}

bool isMatrixUp(const S4& a) {
    if (isMatrixSymmetric(a)) {
        std::string uplo = a.slot("uplo");
        return uplo == "U";
    }
    return false;
}

MapSd toEigenSparse(S4 x) {
    IntegerVector xDimensions = x.slot("Dim");

    IntegerVector xP = x.slot("p");
    IntegerVector xI = x.slot("i");
    NumericVector xX = x.slot("x");

    return MapSd(
        xDimensions[0],
        xDimensions[1],
        xP[xDimensions[1]],
        xP.begin(),
        xI.begin(),
        xX.begin()
    );
}

MapXd toEigenDense(S4 x) {
    NumericVector xX = x.slot("x");
    Rcpp::Dimension xDim = x.slot("Dim");
    return MapXd(
        xX.begin(),
        xDim[0],
        xDim[1]
    );
}

S4 toMatrixSparse(MatrixSd& x, bool symmetric, bool up) {
    x.makeCompressed();

    S4 output(symmetric ? "dsCMatrix" : "dgCMatrix");
    const int numNonZero = x.nonZeros();
    output.slot("Dim") = Rcpp::Dimension(x.rows(), x.cols());
    output.slot("i") = IntegerVector(x.innerIndexPtr(), x.innerIndexPtr() + numNonZero);
    output.slot("p") = IntegerVector(x.outerIndexPtr(), x.outerIndexPtr() + x.outerSize() + 1);
    output.slot("x") = NumericVector(x.valuePtr(), x.valuePtr() + numNonZero);
    if (symmetric) {
        output.slot("uplo") = up ? "U" : "L";
    }
    return output;
}

S4 toMatrixDense(const MatrixXd& x, bool symmetric, bool up) {
    S4 output(symmetric ? "dsyMatrix" : "dgeMatrix");
    output.slot("x") = NumericVector(x.data(), x.data() + x.size());
    output.slot("Dim") = IntegerVector({
        static_cast<int>(x.rows()),
        static_cast<int>(x.cols())
    });
    output.slot("Dimnames") = List({ R_NilValue, R_NilValue });
    output.slot("factors") = List();
    if (symmetric) {
        output.slot("uplo") = up ? "U" : "L";
    }
    return output;
}

template <typename Derived>
typename Derived::PlainObject sparseSymmetricToGeneral(Eigen::SparseMatrixBase<Derived>& x, bool up) {
    typedef typename Derived::PlainObject Output;

    Output output(x.rows(), x.cols());
    if (up) {
        output = x.template selfadjointView<Eigen::Upper>();
    } else {
        output = x.template selfadjointView<Eigen::Lower>();
    }

    return output;
}

S4 fastAddSparseSparse(const S4& aR, const S4& bR) {
    bool symmetric = false;
    bool transposeB = false;
    if (isMatrixSymmetric(aR) && isMatrixSymmetric(bR)) {
        symmetric = true;
        std::string aUplo = aR.slot("uplo");
        std::string bUplo = bR.slot("uplo");
        if (aUplo != bUplo) {
            transposeB = true;
        }
    }
    MapSd a = toEigenSparse(aR);
    MapSd b = toEigenSparse(bR);

    MatrixSd result;
    if (transposeB) {
        result = a + MatrixSd(b.transpose());
    } else {
        if (!symmetric && isMatrixSymmetric(aR)) {
            result = sparseSymmetricToGeneral(a, isMatrixUp(aR)) + b;
        } else if (!symmetric && isMatrixSymmetric(bR)) {
            result = a + sparseSymmetricToGeneral(b, isMatrixUp(bR));
        } else {
            result = a + b;
        }
    }

    return toMatrixSparse(
        result,
        symmetric,
        isMatrixUp(aR)
    );
}

S4 fastAddSparseDiagonal(const S4& aR, const S4& bR) {
    bool symmetric = false;
    bool up = false;
    if (isMatrixSymmetric(aR)) {
        symmetric = true;
        std::string aUplo = aR.slot("uplo");
        if (aUplo == "U") up = true;
    }

    MatrixSd result = toEigenSparse(aR);

    std::string bDiag = bR.slot("diag");
    if (bDiag == "U") {
        MatrixSd b(result.rows(), result.rows());
        b.setIdentity();

        result += b;
    } else {
        NumericVector bX = bR.slot("x");
        Eigen::Map<Eigen::VectorXd> b = Rcpp::as<Eigen::Map<Eigen::VectorXd>>(bX);

        result += MatrixSd(b.asDiagonal());
    }
    return toMatrixSparse(result, symmetric, up);
}

S4 fastAddSparseDense(const S4& aR, const S4& bR) {
    MapSd a = toEigenSparse(aR);
    MapXd b = toEigenDense(bR);

    MatrixXd result;
    if (isMatrixSymmetric(aR) && isMatrixSymmetric(bR)) {
        if (isMatrixUp(aR) && isMatrixUp(bR)) {
            result = b;
        } else {
            result = b.transpose();
        }
        result += a;
    } else {
        if (isMatrixSymmetric(bR)) {
            if (isMatrixUp(bR)) {
                result = b.selfadjointView<Eigen::Upper>();
            } else {
                result = b.selfadjointView<Eigen::Lower>();
            }
        } else {
            result = b;
        }

        if (isMatrixSymmetric(aR)) {
            result += sparseSymmetricToGeneral(a, isMatrixUp(aR));
        } else {
            result += a;
        }
    }

    return toMatrixDense(
        result,
        isMatrixSymmetric(aR) && isMatrixSymmetric(bR),
        isMatrixUp(aR)
    );
}

S4 fastAddDiagonalDiagonal(const S4& aR, const S4& bR) {
    IntegerVector aDim = aR.slot("Dim");
    int n = aDim[0];
    std::string aDiag = aR.slot("diag");
    std::string bDiag = bR.slot("diag");

    NumericVector aX = aR.slot("x");
    NumericVector bX = bR.slot("x");
    if (aDiag == "U") {
        aX = NumericVector(n, 1.0);
    }
    if (bDiag == "U") {
        bX = NumericVector(n, 1.0);
    }

    S4 result = S4("ddiMatrix");
    result.slot("Dim") = aDim;
    result.slot("Dimnames") = aR.slot("Dimnames");
    result.slot("x") = aX + bX;
    result.slot("diag") = "N";
    return result;
}

S4 fastAddDiagonalDense(const S4& aR, const S4& bR) {
    MatrixXd result = toEigenDense(bR);

    std::string aDiag = aR.slot("diag");
    if (aDiag == "U") {
        result.diagonal().array() += 1;
    } else {
        NumericVector aX = aR.slot("x");
        for (int i = 0; i < result.rows(); ++i) {
            result(i, i) += aX[i];
        }
    }
    return toMatrixDense(
        result,
        isMatrixSymmetric(bR),
        isMatrixUp(bR)
    );
}

S4 fastAddDenseDense(const S4& aR, const S4& bR) {
    bool symmetric = isMatrixSymmetric(aR) && isMatrixSymmetric(bR);

    NumericVector aX = aR.slot("x");
    NumericVector bX = bR.slot("x");

    S4 output(symmetric ? "dsyMatrix" : "dgeMatrix");
    output.slot("x") = aX + bX;
    output.slot("Dim") = aR.slot("Dim");
    output.slot("Dimnames") = aR.slot("Dimnames");
    output.slot("factors") = List();
    if (symmetric) {
        output.slot("uplo") = isMatrixUp(aR) ? "U" : "L";
    }

    return output;
}

// Fast addition for general + general and symmetric + symmetric
// [[Rcpp::export(name=".fast_add", rng=false)]]
S4 fast_add(S4 a, S4 b) {
    if (isMatrixDiagonal(a) || isMatrixDense(a)) {
        std::swap(a ,b);
    }

    checkCompatibility(a, b);

    if (isMatrixSparse(a)) {
        if (isMatrixSparse(b)) {
            return fastAddSparseSparse(a, b);
        } else if (isMatrixDiagonal(b)) {
            return fastAddSparseDiagonal(a, b);
        } else if (isMatrixDense(b)) {
            return fastAddSparseDense(a, b);
        }
    } else if (isMatrixDiagonal(a)) {
        if (isMatrixDiagonal(b)) {
            return fastAddDiagonalDiagonal(a, b);
        } else if (isMatrixDense(b)) {
            return fastAddDiagonalDense(a, b);
        }
    } else if (isMatrixDense(a)) {
        if (isMatrixDense(b)) {
            return fastAddDenseDense(a, b);
        } else if (isMatrixDiagonal(b)) {
            return fastAddDiagonalDense(b, a);
        }
    }

    Rcpp::stop("Invalid combination of matrices encountered");
}
