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
C       CPOCTRF  -  Cholesky Factorization for A = L * L^T (complex single)
C     ====================================================================
C       Description:
C       ------------------------------------------------------------------
C         Computes the Cholesky factorization of a complex Hermitian
C         positive-definite matrix A stored in a flat row-major array.
C         The lower-triangular matrix L overwrites the lower triangle of A.
C
C         On output: A contains L in the lower triangle, upper triangle is
C         not referenced.
C
C     ====================================================================
C       Arguments:
C       ------------------------------------------------------------------
C         N    : INTEGER       -> order of the matrix
C         A(*) : COMPLEX       -> flat row-major matrix, size (LDA*N)
C         L(*) : COMPLEX       -> flat row-major matrix, size (LDA*N)
C         LDA  : INTEGER       -> leading dimension of A (usually N)
C         INFO : INTEGER       -> return code:
C                                0 = success
C                               <0 = illegal argument
C                               >0 = matrix not positive definite
C     ====================================================================
      SUBROUTINE CPOCTRF(N, A, L, LDA, INFO)

C   I m p l i c i t   t y p e s
C   ------------------------------------------------------------------
      IMPLICIT NONE

C   D u m m y   a r g u m e n t s
C   ------------------------------------------------------------------
      INTEGER       :: N, LDA, INFO
      COMPLEX       :: A(*), L(*)

C   L o c a l   v a r i a b l e s
C   ------------------------------------------------------------------
      INTEGER       :: I, J, K, INDEX
      COMPLEX       :: SUM

C   I n i t i a l i z e
C   ------------------------------------------------------------------
      INFO = 0

C   M a i n   l o o p   o v e r   r o w s
C   ------------------------------------------------------------------
      DO I = 1, N

C       Compute diagonal element L(I,I)
         SUM = (0.0, 0.0)
         DO K = 1, I-1
            INDEX = (I-1)*LDA + K
            SUM = SUM + L(INDEX)*CONJG(L(INDEX))
         END DO

         INDEX = (I-1)*LDA + I
         IF (REAL(A(INDEX)) - REAL(SUM) .LE. 0.0) THEN
            INFO = I
            RETURN
         END IF
         L(INDEX) = CSQRT(A(INDEX) - SUM)

C       Compute off-diagonal elements L(J,I), J = I+1:N
         DO J = I+1, N
            SUM = (0.0, 0.0)
            DO K = 1, I-1
               SUM = SUM + L((J-1)*LDA + K) * CONJG(L((I-1)*LDA + K))
            END DO
            INDEX = (J-1)*LDA + I
            L(INDEX) = (A(INDEX) - SUM)/L((I-1)*LDA + I)
         END DO

      END DO

C   S u c c e s s f u l   e x i t
C   ------------------------------------------------------------------
      RETURN
      END
