/**
 * @file crout.h
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

    /**
     * @brief Crout LU factorization of a general matrix: A = L * U.
     *
     * Computes the LU decomposition of a general N x N matrix A using
     * the Crout algorithm. A is stored as a flat row-major array.
     * The lower-triangular matrix L and the upper-triangular matrix U
     * (with ones on the diagonal) are written to the output arrays.
     *
     * @param[in]     N    Pointer to the matrix size (N x N).
     * @param[in]     A    Flat row-major input matrix of size N*N.
     * @param[out]    L    Flat row-major output lower-triangular matrix.
     * @param[out]    U    Flat row-major output upper-triangular matrix (unit diagonal).
     * @param[out]    INFO Pointer to the return code:
     *                     - 0: success
     *                     - < 0: zero diagonal detected in L at column |INFO|
     */
    SUBROUTINE crout(INTEGER* N, REAL* A, REAL* L, REAL* U, INTEGER* INFO) {
        API_sgectrf(N, A, L, U, INFO);
    }

    /**
     * @overload
     * @brief Crout LU factorization (double-precision).
     *
     * @param[in]     N    Pointer to the matrix size (N x N).
     * @param[in]     A    Flat row-major input matrix of size N*N.
     * @param[out]    L    Flat row-major output lower-triangular matrix.
     * @param[out]    U    Flat row-major output upper-triangular matrix (unit diagonal).
     * @param[out]    INFO Pointer to the return code.
     */
    SUBROUTINE crout(INTEGER* N, DOUBLE* A, DOUBLE* L, DOUBLE* U, INTEGER* INFO) {
        API_dgectrf(N, A, L, U, INFO);
    }

    /**
     * @overload
     * @brief Crout LU factorization (single-precision complex).
     *
     * @param[in]     N    Pointer to the matrix size (N x N).
     * @param[in]     A    Flat row-major input matrix of size N*N.
     * @param[out]    L    Flat row-major output lower-triangular matrix.
     * @param[out]    U    Flat row-major output upper-triangular matrix (unit diagonal).
     * @param[out]    INFO Pointer to the return code.
     */
    SUBROUTINE crout(INTEGER* N, COMPLEX* A, COMPLEX* L, COMPLEX* U, INTEGER* INFO) {
        API_cgectrf(N, A, L, U, INFO);
    }

    /**
     * @overload
     * @brief Crout LU factorization (double-precision complex).
     *
     * @param[in]     N    Pointer to the matrix size (N x N).
     * @param[in]     A    Flat row-major input matrix of size N*N.
     * @param[out]    L    Flat row-major output lower-triangular matrix.
     * @param[out]    U    Flat row-major output upper-triangular matrix (unit diagonal).
     * @param[out]    INFO Pointer to the return code.
     */
    SUBROUTINE crout(INTEGER* N, DOUBLE_COMPLEX* A, DOUBLE_COMPLEX* L, DOUBLE_COMPLEX* U, INTEGER* INFO) {
        API_zgectrf(N, A, L, U, INFO);
    }

#else  // C-only fallback

    /* ===========
     * C INTERFACE
     * ============ */

    #define crout(N, A, L, U, INFO)  \
        _Generic((A),                          \
            REAL*:            API_sgectrf,     \
            DOUBLE*:          API_dgectrf,     \
            COMPLEX*:         API_cgectrf,     \
            DOUBLE_COMPLEX*:  API_zgectrf      \
        )(N, A, L, U, INFO)

#endif

#endif
