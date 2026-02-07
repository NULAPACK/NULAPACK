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


def jacobi(a, b, max_iter=1000, tol=1e-8, omega=1.0):
    """
    Solve the linear system ax = b using the Jacobi method.

    Parameters
    ----------
    a : ndarray
        Coefficient matrix (n x n)
    b : ndarray
        Right-hand side vector (n,)
    max_iter : int, optional
        Maximum number of iterations
    tol : float, optional
        Convergence tolerance
    omega : float, optional
        Relaxation factor

    Returns
    -------
    x : ndarray
        Solution vector
    status : int
        0 if converged, non-zero otherwise
    """
    a = np.ascontiguousarray(a)
    b = np.asfortranarray(b)
    n = a.shape[0]

    x = np.zeros_like(b)

    a_flat = a.ravel()

    if np.issubdtype(a.dtype, np.floating):
        if a.dtype == np.float32:
            status = _nulapack.sgejsv(a_flat, b, x, max_iter, tol, omega, 0, n)
        else:  # float64
            status = _nulapack.dgejsv(a_flat, b, x, max_iter, tol, omega, 0, n)
    elif np.issubdtype(a.dtype, np.complexfloating):
        if a.dtype == np.complex64:
            status = _nulapack.cgejsv(a_flat, b, x, max_iter, tol, omega, 0, n)
        else:  # complex128
            status = _nulapack.zgejsv(a_flat, b, x, max_iter, tol, omega, 0, n)
    else:
        raise TypeError(f"Unsupported array dtype: {a.dtype}")

    return x, int(status) if status is not None else 0
