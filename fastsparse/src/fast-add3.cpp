#include <RcppEigen.h>

#define AS_CHM_SP(x) as_cholmod_sparse ((CHM_SP)alloca(sizeof(cholmod_sparse)), x, (Rboolean)TRUE, (Rboolean)FALSE)

cholmod_common c;
bool started = false;

// Fast addition for general + general and symmetric + symmetric
// [[Rcpp::export(name=".fast_add3", rng=false)]]
SEXP fast_add3(Rcpp::S4 a_, Rcpp::S4 b_) {
    if (!started) {
        cholmod_start(&c);
        started = TRUE;
    }

    CHM_SP a = AS_CHM_SP(a_);
    CHM_SP b = AS_CHM_SP(b_);

    double one[] = {1, 0};

    CHM_SP ans = cholmod_add(
        a, b, one, one,
        TRUE,
        TRUE,
        &c
    );

    return M_chm_sparse_to_SEXP(
        ans,
        TRUE,
        0,
        0,
        "",
        a_.slot("Dimnames")
    );
}
