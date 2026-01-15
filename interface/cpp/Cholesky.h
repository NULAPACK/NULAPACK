#ifndef CHOLESKY_H
#define CHOLESKY_H

#include "types.h"
#include "mangling.h"

/* =========================
 * FORTRAN API DECLARATIONS
 * ========================= */

fortran API_cpoctrf(INTEGER* N, COMPLEX* A, COMPLEX* L, INTEGER* LDA, INTEGER* INFO);

fortran API_dpoctrf(INTEGER* N, DOUBLE* A, DOUBLE* L, INTEGER* LDA, INTEGER* INFO);

fortran API_spoctrf(INTEGER* N, REAL* A, REAL* L, INTEGER* LDA, INTEGER* INFO);

fortran API_zpoctrf(INTEGER* N, DOUBLE_COMPLEX* A, DOUBLE_COMPLEX* L, INTEGER* LDA, INTEGER* INFO);


#ifdef __cplusplus

    /* ==============
     * C++ INTERFACE
     * ============== */

    SUBROUTINE CHOLESKY(INTEGER* N, REAL* A, REAL* L, INTEGER* LDA, INTEGER* INFO) {
        API_spoctrf(N, A, L, LDA, INFO);
    }

    SUBROUTINE CHOLESKY(INTEGER* N, DOUBLE* A, DOUBLE* L, INTEGER* LDA, INTEGER* INFO) {
        API_dpoctrf(N, A, L, LDA, INFO);
    }

    SUBROUTINE CHOLESKY(INTEGER* N, COMPLEX* A, COMPLEX* L, INTEGER* LDA, INTEGER* INFO) {
        API_cpoctrf(N, A, L, LDA, INFO);
    }

    SUBROUTINE CHOLESKY(INTEGER* N, DOUBLE_COMPLEX* A, DOUBLE_COMPLEX* L, INTEGER* LDA, INTEGER* INFO) {
        API_zpoctrf(N, A, L, LDA, INFO);
    }

#else  // C-only fallback

    /* ===========
     * C INTERFACE
     * ============ */

    #define CHOLESKY(N, A, L, LDA, INFO)  \
        _Generic((A),                          \
            REAL*:            API_spoctrf,     \
            DOUBLE*:          API_dpoctrf,     \
            COMPLEX*:         API_cpoctrf,     \
            DOUBLE_COMPLEX*:  API_zpoctrf      \
        )(N, A, L, LDA, INFO)

#endif

#endif
