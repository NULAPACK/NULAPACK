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


def thomas(a: np.ndarray, b: np.ndarray):
    """
    Solve a tridiagonal linear system A * X = B using the Thomas algorithm.

    Parameters
    ----------
    a : ndarray
        Coefficient matrix (n x n) stored as a full matrix.
    b : ndarray
        Right-hand side vector (n,)

    Returns
    -------
    x : ndarray
        Solution vector
    info : int
        0 if success, <0 if zero diagonal detected
    """
    a = np.ascontiguousarray(a)
    b = np.asfortranarray(b)
    n = a.shape[0]

    x = np.zeros_like(b)

    a_flat = a.ravel()

    if np.issubdtype(a.dtype, np.floating):
        if a.dtype == np.float32:
            status = _nulapack.sgttsv(a_flat, b, x, 0, n)
        else:  # float64
            status = _nulapack.dgttsv(a_flat, b, x, 0, n)
    elif np.issubdtype(a.dtype, np.complexfloating):
        if a.dtype == np.complex64:
            status = _nulapack.cgttsv(a_flat, b, x, 0, n)
        else:  # complex128
            status = _nulapack.zgttsv(a_flat, b, x, 0, n)
    else:
        raise TypeError(f"Unsupported array dtype: {a.dtype}")

    return x, int(status) if status is not None else 0
