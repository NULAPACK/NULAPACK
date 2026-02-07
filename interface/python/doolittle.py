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


def doolittle(a: np.ndarray):
    """
    Compute the LU Doolittle decomposition of a general matrix A.

    Parameters
    ----------
    a : ndarray
        Coefficient matrix (n x n) stored as a full matrix.

    Returns
    -------
    L : ndarray
        Lower triangular matrix from the factorization.
    U : ndarray
        Upper triangular matrix from the factorization.
    info : int
        0 if success, <0 if a zero diagonal in U was detected.
    """
    a = np.ascontiguousarray(a)
    n = a.shape[0]

    a_flat = a.ravel(order="C")
    l_flat = np.zeros_like(a_flat)
    u_flat = np.zeros_like(a_flat)
    info = np.zeros(1, dtype=np.int32)

    if np.issubdtype(a.dtype, np.floating):
        if a.dtype == np.float32:
            _nulapack.sgedtrf(n, a_flat, l_flat, u_flat, info)
        else:  # float64
            _nulapack.dgedtrf(n, a_flat, l_flat, u_flat, info)
    elif np.issubdtype(a.dtype, np.complexfloating):
        if a.dtype == np.complex64:
            _nulapack.cgedtrf(n, a_flat, l_flat, u_flat, info)
        else:  # complex128
            _nulapack.zgedtrf(n, a_flat, l_flat, u_flat, info)
    else:
        raise TypeError(f"Unsupported array dtype: {a.dtype}")

    return l_flat.reshape(n, n, order="C"), u_flat.reshape(n, n, order="C"), int(info[0])
