# ====================================================================
#                           N U L A P A C K
#                           U U L A P A C K
#                           L L L A P A C K
#                           A A A A P A C K
#                           P P P P P A C K
#                           A A A A A A C K
#                           C C C C C C C K
#                           K K K K K K K K
#
#  This file is part of NULAPACK - NUmerical Linear Algebra PACKage
#
#  Copyright (C) 2025  Saud Zahir
#
#  NULAPACK is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  NULAPACK is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with NULAPACK.  If not, see <https://www.gnu.org/licenses/>.
# ====================================================================

import _nulapack
import numpy as np


def cholesky(a: np.ndarray):
    """
    Compute the Cholesky factorization of a symmetric/Hermitian
    positive-definite matrix A using NULAPACK.

    Parameters
    ----------
    a : ndarray
        Coefficient matrix (n x n) stored as a full matrix. Real matrices
        should be symmetric, complex matrices should be Hermitian and
        positive-definite.

    Returns
    -------
    L : ndarray
        Lower-triangular matrix from the factorization (A = L * L^T or
        A = L * L^H).
    info : int
        0 if success, >0 if the matrix is not positive-definite.
    """
    a = np.ascontiguousarray(a)
    n = a.shape[0]
    lda = n

    a_flat = a.ravel(order="C")
    l_flat = np.zeros_like(a_flat)
    info = np.zeros(1, dtype=np.int32)

    if np.issubdtype(a.dtype, np.floating):
        if a.dtype == np.float32:
            _nulapack.spoctrf(n, a_flat, l_flat, lda, info)
        else:  # float64
            _nulapack.dpoctrf(n, a_flat, l_flat, lda, info)
    elif np.issubdtype(a.dtype, np.complexfloating):
        if a.dtype == np.complex64:
            _nulapack.cpoctrf(n, a_flat, l_flat, lda, info)
        else:  # complex128
            _nulapack.zpoctrf(n, a_flat, l_flat, lda, info)
    else:
        raise TypeError(f"Unsupported array dtype: {a.dtype}")

    return np.tril(l_flat.reshape(n, n, order="C")), int(info[0])
