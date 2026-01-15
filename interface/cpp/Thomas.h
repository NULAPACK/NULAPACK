#ifndef THOMAS_H
#define THOMAS_H

#include "types.h"
#include "mangling.h"

/* =========================
 * FORTRAN API DECLARATIONS
 * ========================= */

fortran API_cgttsv(INTEGER* N, COMPLEX* A, COMPLEX* B, COMPLEX* X, INTEGER* INFO);

fortran API_dgttsv(INTEGER* N, DOUBLE* A, DOUBLE* B, DOUBLE* X, INTEGER* INFO);

fortran API_sgttsv(INTEGER* N, REAL* A, REAL* B, REAL* X, INTEGER* INFO);

fortran API_zgttsv(INTEGER* N, DOUBLE_COMPLEX* A, DOUBLE_COMPLEX* B, DOUBLE_COMPLEX* X, INTEGER* INFO);


#ifdef __cplusplus

    /* ==============
     * C++ INTERFACE
     * ============== */

    SUBROUTINE THOMAS(INTEGER* N, REAL* A, REAL* B, REAL* X, INTEGER* INFO) {
        API_sgttsv(N, A, B, X, INFO);
    }

    SUBROUTINE THOMAS(INTEGER* N, DOUBLE* A, DOUBLE* B, DOUBLE* X, INTEGER* INFO) {
        API_dgttsv(N, A, B, X, INFO);
    }

    SUBROUTINE THOMAS(INTEGER* N, COMPLEX* A, COMPLEX* B, COMPLEX* X, INTEGER* INFO) {
        API_cgttsv(N, A, B, X, INFO);
    }

    SUBROUTINE THOMAS(INTEGER* N, DOUBLE_COMPLEX* A, DOUBLE_COMPLEX* B, DOUBLE_COMPLEX* X, INTEGER* INFO) {
        API_zgttsv(N, A, B, X, INFO);
    }

#else  // C-only fallback

    /* ===========
     * C INTERFACE
     * ============ */

    #define THOMAS(N, A, B, X, INFO)  \
        _Generic((A),                      \
            REAL*:            API_sgttsv,  \
            DOUBLE*:          API_dgttsv,  \
            COMPLEX*:         API_cgttsv,  \
            DOUBLE_COMPLEX*:  API_zgttsv   \
        )(N, A, B, X, INFO)

#endif

#endif
