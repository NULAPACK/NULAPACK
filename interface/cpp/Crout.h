#ifndef CROUT_H
#define CROUT_H

#include "types.h"
#include "mangling.h"

/* =========================
 * FORTRAN API DECLARATIONS
 * ========================= */

fortran API_sgectrf(INTEGER* N, REAL* A, REAL* L, REAL* U, INTEGER* INFO);

fortran API_dgectrf(INTEGER* N, DOUBLE* A, DOUBLE* L, DOUBLE* U, INTEGER* INFO);

fortran API_cgectrf(INTEGER* N, COMPLEX* A, COMPLEX* L, COMPLEX* U, INTEGER* INFO);

fortran API_zgectrf(INTEGER* N, DOUBLE_COMPLEX* A, DOUBLE_COMPLEX* L, DOUBLE_COMPLEX* U, INTEGER* INFO);


#ifdef __cplusplus

    /* ==============
     * C++ INTERFACE
     * ============== */

    SUBROUTINE CROUT(INTEGER* N, REAL* A, REAL* L, REAL* U, INTEGER* INFO) {
        API_sgectrf(N, A, L, U, INFO);
    }

    SUBROUTINE CROUT(INTEGER* N, DOUBLE* A, DOUBLE* L, DOUBLE* U, INTEGER* INFO) {
        API_dgectrf(N, A, L, U, INFO);
    }

    SUBROUTINE CROUT(INTEGER* N, COMPLEX* A, COMPLEX* L, COMPLEX* U, INTEGER* INFO) {
        API_cgectrf(N, A, L, U, INFO);
    }

    SUBROUTINE CROUT(INTEGER* N, DOUBLE_COMPLEX* A, DOUBLE_COMPLEX* L, DOUBLE_COMPLEX* U, INTEGER* INFO) {
        API_zgectrf(N, A, L, U, INFO);
    }

#else  // C-only fallback

    /* ===========
     * C INTERFACE
     * ============ */

    #define CROUT(N, A, L, U, INFO)  \
        _Generic((A),                          \
            REAL*:            API_sgectrf,     \
            DOUBLE*:          API_dgectrf,     \
            COMPLEX*:         API_cgectrf,     \
            DOUBLE_COMPLEX*:  API_zgectrf      \
        )(N, A, L, U, INFO)

#endif

#endif
