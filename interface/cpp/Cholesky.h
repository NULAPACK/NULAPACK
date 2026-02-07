/**
 * @file cholesky.h
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

    /**
     * @brief Cholesky factorization of a symmetric positive-definite matrix: A = L * L^T.
     *
     * Computes the Cholesky factorization of a real symmetric positive-definite
     * matrix A stored in a flat row-major array. The lower-triangular factor L
     * is written to the output array.
     *
     * @param[in]     N    Pointer to the order of the matrix (N x N).
     * @param[in]     A    Flat row-major input matrix of size LDA*N.
     * @param[out]    L    Flat row-major output lower-triangular matrix of size LDA*N.
     * @param[in]     LDA  Pointer to the leading dimension of A (usually N).
     * @param[out]    INFO Pointer to the return code:
     *                     - 0: success
     *                     - < 0: illegal argument
     *                     - > 0: matrix is not positive definite (failure at row INFO)
     */
    SUBROUTINE cholesky(INTEGER* N, REAL* A, REAL* L, INTEGER* LDA, INTEGER* INFO) {
        API_spoctrf(N, A, L, LDA, INFO);
    }

    /**
     * @overload
     * @brief Cholesky factorization (double-precision).
     *
     * @param[in]     N    Pointer to the order of the matrix (N x N).
     * @param[in]     A    Flat row-major input matrix of size LDA*N.
     * @param[out]    L    Flat row-major output lower-triangular matrix of size LDA*N.
     * @param[in]     LDA  Pointer to the leading dimension of A (usually N).
     * @param[out]    INFO Pointer to the return code.
     */
    SUBROUTINE cholesky(INTEGER* N, DOUBLE* A, DOUBLE* L, INTEGER* LDA, INTEGER* INFO) {
        API_dpoctrf(N, A, L, LDA, INFO);
    }

    /**
     * @overload
     * @brief Cholesky factorization (single-precision complex).
     *
     * @param[in]     N    Pointer to the order of the matrix (N x N).
     * @param[in]     A    Flat row-major input matrix of size LDA*N.
     * @param[out]    L    Flat row-major output lower-triangular matrix of size LDA*N.
     * @param[in]     LDA  Pointer to the leading dimension of A (usually N).
     * @param[out]    INFO Pointer to the return code.
     */
    SUBROUTINE cholesky(INTEGER* N, COMPLEX* A, COMPLEX* L, INTEGER* LDA, INTEGER* INFO) {
        API_cpoctrf(N, A, L, LDA, INFO);
    }

    /**
     * @overload
     * @brief Cholesky factorization (double-precision complex).
     *
     * @param[in]     N    Pointer to the order of the matrix (N x N).
     * @param[in]     A    Flat row-major input matrix of size LDA*N.
     * @param[out]    L    Flat row-major output lower-triangular matrix of size LDA*N.
     * @param[in]     LDA  Pointer to the leading dimension of A (usually N).
     * @param[out]    INFO Pointer to the return code.
     */
    SUBROUTINE cholesky(INTEGER* N, DOUBLE_COMPLEX* A, DOUBLE_COMPLEX* L, INTEGER* LDA, INTEGER* INFO) {
        API_zpoctrf(N, A, L, LDA, INFO);
    }

#else  // C-only fallback

    /* ===========
     * C INTERFACE
     * ============ */

    #define cholesky(N, A, L, LDA, INFO)  \
        _Generic((A),                          \
            REAL*:            API_spoctrf,     \
            DOUBLE*:          API_dpoctrf,     \
            COMPLEX*:         API_cpoctrf,     \
            DOUBLE_COMPLEX*:  API_zpoctrf      \
        )(N, A, L, LDA, INFO)

#endif

#endif
