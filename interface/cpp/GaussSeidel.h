/**
 * @file gaussseidel.h
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

#ifndef GAUSS_SEIDEL_H
#define GAUSS_SEIDEL_H

#include "types.h"
#include "mangling.h"

/* =========================
 * FORTRAN API DECLARATIONS
 * ========================= */

fortran API_cgegssv(INTEGER* N, COMPLEX* A, COMPLEX* B, COMPLEX* X,
                   INTEGER* MAX_ITER, REAL* TOL, REAL* OMEGA, INTEGER* STATUS);

fortran API_dgegssv(INTEGER* N, DOUBLE* A, DOUBLE* B, DOUBLE* X,
                   INTEGER* MAX_ITER, DOUBLE* TOL, DOUBLE* OMEGA, INTEGER* STATUS);

fortran API_sgegssv(INTEGER* N, REAL* A, REAL* B, REAL* X,
                   INTEGER* MAX_ITER, REAL* TOL, REAL* OMEGA, INTEGER* STATUS);

fortran API_zgegssv(INTEGER* N, DOUBLE_COMPLEX* A, DOUBLE_COMPLEX* B, DOUBLE_COMPLEX* X,
                   INTEGER* MAX_ITER, DOUBLE* TOL, DOUBLE* OMEGA, INTEGER* STATUS);


#ifdef __cplusplus

    /* ==============
     * C++ INTERFACE
     * ============== */

    /**
     * @brief Gauss-Seidel iterative solver for A * X = B.
     *
     * Solves the linear system A * X = B using the iterative Gauss-Seidel
     * method with optional successive over-relaxation (SOR). A is a square
     * N x N matrix in flat row-major array format.
     *
     * On input, X contains the initial guess. On output, X contains the
     * computed solution. Convergence is based on maximum absolute difference
     * per iteration.
     *
     * @param[in]     N        Pointer to the size of the matrix (N x N).
     * @param[in]     A        Flat row-major input matrix of size N*N.
     * @param[in]     B        Right-hand side vector of size N.
     * @param[in,out] X        On input: initial guess. On output: solution vector.
     * @param[in]     MAX_ITER Pointer to the maximum number of iterations.
     * @param[in]     TOL      Pointer to the convergence tolerance.
     * @param[out]    STATUS   Pointer to the return code:
     *                         - 0: success (converged)
     *                         - > 0: did not converge within MAX_ITER iterations
     *                         - < 0: zero diagonal element detected at row |STATUS|
     * @param[in]     OMEGA    Relaxation factor (default = 1.0, i.e. standard Gauss-Seidel).
     */
    SUBROUTINE gaussSeidel(INTEGER* N, REAL* A, REAL* B, REAL* X,
                            INTEGER* MAX_ITER, REAL* TOL, INTEGER* STATUS, REAL OMEGA = 1.0) {
        API_sgegssv(N, A, B, X, MAX_ITER, TOL, &OMEGA, STATUS);
    }

    /**
     * @overload
     * @brief Gauss-Seidel iterative solver (double-precision).
     *
     * @param[in]     N        Pointer to the size of the matrix (N x N).
     * @param[in]     A        Flat row-major input matrix of size N*N.
     * @param[in]     B        Right-hand side vector of size N.
     * @param[in,out] X        On input: initial guess. On output: solution vector.
     * @param[in]     MAX_ITER Pointer to the maximum number of iterations.
     * @param[in]     TOL      Pointer to the convergence tolerance.
     * @param[out]    STATUS   Pointer to the return code.
     * @param[in]     OMEGA    Relaxation factor (default = 1.0).
     */
    SUBROUTINE gaussSeidel(INTEGER* N, DOUBLE* A, DOUBLE* B, DOUBLE* X,
                            INTEGER* MAX_ITER, DOUBLE* TOL, INTEGER* STATUS, DOUBLE OMEGA = 1.0) {
        API_dgegssv(N, A, B, X, MAX_ITER, TOL, &OMEGA, STATUS);
    }

    /**
     * @overload
     * @brief Gauss-Seidel iterative solver (single-precision complex).
     *
     * @param[in]     N        Pointer to the size of the matrix (N x N).
     * @param[in]     A        Flat row-major input matrix of size N*N.
     * @param[in]     B        Right-hand side vector of size N.
     * @param[in,out] X        On input: initial guess. On output: solution vector.
     * @param[in]     MAX_ITER Pointer to the maximum number of iterations.
     * @param[in]     TOL      Pointer to the convergence tolerance (REAL).
     * @param[out]    STATUS   Pointer to the return code.
     * @param[in]     OMEGA    Relaxation factor (default = 1.0).
     */
    SUBROUTINE gaussSeidel(INTEGER* N, COMPLEX* A, COMPLEX* B, COMPLEX* X,
                            INTEGER* MAX_ITER, REAL* TOL, INTEGER* STATUS, REAL OMEGA = 1.0) {
        API_cgegssv(N, A, B, X, MAX_ITER, TOL, &OMEGA, STATUS);
    }

    /**
     * @overload
     * @brief Gauss-Seidel iterative solver (double-precision complex).
     *
     * @param[in]     N        Pointer to the size of the matrix (N x N).
     * @param[in]     A        Flat row-major input matrix of size N*N.
     * @param[in]     B        Right-hand side vector of size N.
     * @param[in,out] X        On input: initial guess. On output: solution vector.
     * @param[in]     MAX_ITER Pointer to the maximum number of iterations.
     * @param[in]     TOL      Pointer to the convergence tolerance (DOUBLE).
     * @param[out]    STATUS   Pointer to the return code.
     * @param[in]     OMEGA    Relaxation factor (default = 1.0).
     */
    SUBROUTINE gaussSeidel(INTEGER* N, DOUBLE_COMPLEX* A, DOUBLE_COMPLEX* B, DOUBLE_COMPLEX* X,
                            INTEGER* MAX_ITER, DOUBLE* TOL, INTEGER* STATUS, DOUBLE OMEGA = 1.0) {
        API_zgegssv(N, A, B, X, MAX_ITER, TOL, &OMEGA, STATUS);
    }

#else  // C-only fallback

    /* ===========
     * C INTERFACE
     * ============ */

    #define gaussSeidel(N, A, B, X, MAX_ITER, TOL, OMEGA, STATUS)  \
        _Generic((A),                                                \
            REAL*:            API_sgegssv,                            \
            DOUBLE*:          API_dgegssv,                            \
            COMPLEX*:         API_cgegssv,                            \
            DOUBLE_COMPLEX*:  API_zgegssv                             \
        )(N, A, B, X, MAX_ITER, TOL, OMEGA, STATUS)

#endif

#endif
