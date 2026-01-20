C     ====================================================================
C     This file is part of NULAPACK - NUmerical Linear Algebra PACKage
C
C     Copyright (C) 2025  Saud Zahir
C
C     NULAPACK is free software: you can redistribute it and/or modify
C     it under the terms of the GNU General Public License as published by
C     the Free Software Foundation, either version 3 of the License, or
C     (at your option) any later version.
C
C     NULAPACK is distributed in the hope that it will be useful,
C     but WITHOUT ANY WARRANTY; without even the implied warranty of
C     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C     GNU General Public License for more details.
C
C     You should have received a copy of the GNU General Public License
C     along with NULAPACK.  If not, see <https://www.gnu.org/licenses/>.
C
C     ====================================================================
C       ZGECTRF  -  Crout Factorization for A = L * U
C     ====================================================================
C       Description:
C       ------------------------------------------------------------------
C         Computes the LU decomposition of a general N x N matrix A
C         using the Crout algorithm. A is stored as a flat row-major
C         array. The lower triangular matrix L and the upper triangular
C         matrix U (with ones on the diagonal) are stored in L and U arrays.
C
C     ====================================================================
C       Arguments:
C       ------------------------------------------------------------------
C         N      : INTEGER            -> matrix size (N x N)
C         A(*)   : DOUBLE COMPLEX     -> flat row-major matrix A
C         L(*)   : DOUBLE COMPLEX     -> lower triangular matrix L
C         U(*)   : DOUBLE COMPLEX     -> upper triangular matrix U
C         INFO   : INTEGER            -> return code:
C                                           0 = success
C                                          <0 = zero diagonal in L detected
C     ====================================================================
      SUBROUTINE ZGECTRF(N, A, L, U, INFO)

C   I m p l i c i t   T y p e s
C   ------------------------------------------------------------------
      IMPLICIT NONE

C   D u m m y   A r g u m e n t s
C   ------------------------------------------------------------------
      INTEGER            :: N, INFO
      DOUBLE COMPLEX     :: A(*), L(*), U(*)

C   L o c a l   V a r i a b l e s
C   ------------------------------------------------------------------
      INTEGER            :: I, J, K
      DOUBLE COMPLEX     :: SUM

C   I n i t i a l i z e
C   ------------------------------------------------------------------
      INFO = 0
      DO I = 1, N * N
         L(I) = (0.0D0, 0.0D0)
         U(I) = (0.0D0, 0.0D0)
      END DO

C   M a i n   D e c o m p o s i t i o n   L o o p
C   ------------------------------------------------------------------
      DO J = 1, N
C        L o w e r   T r i a n g u l a r
C        --------------------------------------------------------------
         DO I = J, N
            SUM = (0.0D0, 0.0D0)
            DO K = 1, J - 1
               SUM = SUM + L((I-1)*N + K) * U((K-1)*N + J)
            END DO
            L((I-1)*N + J) = A((I-1)*N + J) - SUM
         END DO

C        U p p e r   T r i a n g u l a r
C        --------------------------------------------------------------
         DO I = J, N
            IF (I .EQ. J) THEN
               U((J-1)*N + I) = (1.0D0, 0.0D0)
            ELSE
               SUM = (0.0D0, 0.0D0)
               DO K = 1, J - 1
                  SUM = SUM + L((J-1)*N + K) * U((K-1)*N + I)
               END DO

C              Check for zero diagonal to avoid division by zero
               IF (L((J-1)*N + J) .EQ. (0.0D0, 0.0D0)) THEN
                  INFO = -J
                  RETURN
               END IF

               U((J-1)*N + I) = (A((J-1)*N + I) - SUM) / L((J-1)*N + J)
            END IF
         END DO
      END DO

C   S u c c e s s f u l   e x i t
C   ------------------------------------------------------------------
      RETURN
      END
