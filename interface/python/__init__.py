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

from importlib.metadata import PackageNotFoundError, version

from .cholesky import cholesky
from .crout import crout
from .doolittle import doolittle
from .gauss_seidel import gauss_seidel
from .jacobi import jacobi
from .thomas import thomas


try:
    __version__ = version("nulapack")
except PackageNotFoundError:
    # package is not installed
    pass

__all__ = ["__version__", "cholesky", "crout", "doolittle", "gauss_seidel", "jacobi", "thomas"]
