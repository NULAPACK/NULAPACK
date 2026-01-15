#ifndef DOOLITTLE_H
#define DOOLITTLE_H

#include "types.h"
#include "mangling.h"

/* =========================
 * FORTRAN API DECLARATIONS
 * ========================= */

fortran API_sgedtrf(INTEGER* N, REAL* A, REAL* L, REAL* U, INTEGER* INFO);

fortran API_dgedtrf(INTEGER* N, DOUBLE* A, DOUBLE* L, DOUBLE* U, INTEGER* INFO);

fortran API_cgedtrf(INTEGER* N, COMPLEX* A, COMPLEX* L, COMPLEX* U, INTEGER* INFO);

fortran API_zgedtrf(INTEGER* N, DOUBLE_COMPLEX* A, DOUBLE_COMPLEX* L, DOUBLE_COMPLEX* U, INTEGER* INFO);


#ifdef __cplusplus

    /* ==============
     * C++ INTERFACE
     * ============== */

    SUBROUTINE DOOLITTLE(INTEGER* N, REAL* A, REAL* L, REAL* U, INTEGER* INFO) {
        API_sgedtrf(N, A, L, U, INFO);
    }

    SUBROUTINE DOOLITTLE(INTEGER* N, DOUBLE* A, DOUBLE* L, DOUBLE* U, INTEGER* INFO) {
        API_dgedtrf(N, A, L, U, INFO);
    }

    SUBROUTINE DOOLITTLE(INTEGER* N, COMPLEX* A, COMPLEX* L, COMPLEX* U, INTEGER* INFO) {
        API_cgedtrf(N, A, L, U, INFO);
    }

    SUBROUTINE DOOLITTLE(INTEGER* N, DOUBLE_COMPLEX* A, DOUBLE_COMPLEX* L, DOUBLE_COMPLEX* U, INTEGER* INFO) {
        API_zgedtrf(N, A, L, U, INFO);
    }

#else  // C-only fallback

    /* ===========
     * C INTERFACE
     * ============ */

    #define DOOLITTLE(N, A, L, U, INFO)  \
        _Generic((A),                          \
            REAL*:            API_sgedtrf,     \
            DOUBLE*:          API_dgedtrf,     \
            COMPLEX*:         API_cgedtrf,     \
            DOUBLE_COMPLEX*:  API_zgedtrf      \
        )(N, A, L, U, INFO)

#endif

#endif
