/**
 * @file thomas.h
 *
 * ====================================================================
 *                           N U L A P A C K
 *                           U U L A P A C K
 *                           L L L A P A C K
 *                           A A A A P A C K
 *                           P P P P P A C K
 *                           A A A A A A C K
 *                           C C C C C C C K
 *                           K K K K K K K K
 *
 *  This file is part of NULAPACK - NUmerical Linear Algebra PACKage
 *
 *  Copyright (C) 2025  Saud Zahir
 *
 *  NULAPACK is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  NULAPACK is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with NULAPACK.  If not, see <https://www.gnu.org/licenses/>.
 * ====================================================================
 */

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

    /**
     * @brief Thomas algorithm (tridiagonal solver) for A * X = B.
     *
     * Direct solver for tridiagonal linear systems A * X = B using the
     * Thomas algorithm. A is supplied as a full N x N matrix in flat
     * row-major storage and is overwritten during computation.
     *
     * On output, X contains the solution vector. No pivoting is performed;
     * zero diagonal entries produce failure.
     *
     * @param[in]     N    Pointer to the matrix size (N x N).
     * @param[in,out] A    Flat row-major tridiagonal matrix of size N*N (modified on output).
     * @param[in,out] B    Right-hand side vector of size N (modified on output).
     * @param[out]    X    Solution vector of size N.
     * @param[out]    INFO Pointer to the return code:
     *                     - 0: success
     *                     - < 0: zero diagonal detected at row |INFO|
     */
    SUBROUTINE thomas(INTEGER* N, REAL* A, REAL* B, REAL* X, INTEGER* INFO) {
        API_sgttsv(N, A, B, X, INFO);
    }

    /**
     * @overload
     * @brief Thomas algorithm (double-precision).
     *
     * @param[in]     N    Pointer to the matrix size (N x N).
     * @param[in,out] A    Flat row-major tridiagonal matrix of size N*N (modified on output).
     * @param[in,out] B    Right-hand side vector of size N (modified on output).
     * @param[out]    X    Solution vector of size N.
     * @param[out]    INFO Pointer to the return code.
     */
    SUBROUTINE thomas(INTEGER* N, DOUBLE* A, DOUBLE* B, DOUBLE* X, INTEGER* INFO) {
        API_dgttsv(N, A, B, X, INFO);
    }

    /**
     * @overload
     * @brief Thomas algorithm (single-precision complex).
     *
     * @param[in]     N    Pointer to the matrix size (N x N).
     * @param[in,out] A    Flat row-major tridiagonal matrix of size N*N (modified on output).
     * @param[in,out] B    Right-hand side vector of size N (modified on output).
     * @param[out]    X    Solution vector of size N.
     * @param[out]    INFO Pointer to the return code.
     */
    SUBROUTINE thomas(INTEGER* N, COMPLEX* A, COMPLEX* B, COMPLEX* X, INTEGER* INFO) {
        API_cgttsv(N, A, B, X, INFO);
    }

    /**
     * @overload
     * @brief Thomas algorithm (double-precision complex).
     *
     * @param[in]     N    Pointer to the matrix size (N x N).
     * @param[in,out] A    Flat row-major tridiagonal matrix of size N*N (modified on output).
     * @param[in,out] B    Right-hand side vector of size N (modified on output).
     * @param[out]    X    Solution vector of size N.
     * @param[out]    INFO Pointer to the return code.
     */
    SUBROUTINE thomas(INTEGER* N, DOUBLE_COMPLEX* A, DOUBLE_COMPLEX* B, DOUBLE_COMPLEX* X, INTEGER* INFO) {
        API_zgttsv(N, A, B, X, INFO);
    }

#else  // C-only fallback

    /* ===========
     * C INTERFACE
     * ============ */

    #define thomas(N, A, B, X, INFO)  \
        _Generic((A),                      \
            REAL*:            API_sgttsv,  \
            DOUBLE*:          API_dgttsv,  \
            COMPLEX*:         API_cgttsv,  \
            DOUBLE_COMPLEX*:  API_zgttsv   \
        )(N, A, B, X, INFO)

#endif

#endif
