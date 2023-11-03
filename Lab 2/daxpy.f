      PROGRAM DAXPY
      INCLUDE 'omp_lib.h'
      INTEGER I, N
      PARAMETER (N= 1000000)
      DOUBLE PRECISION S /0.1/, T1, T2
      DOUBLE PRECISION, ALLOCATABLE :: Y(:), X(:)
      ALLOCATE (Y(N), X(N))
      X(1:N) = 1.0
      Y(1:N) = 2.0
      T1 = OMP_GET_WTIME()
      DO I=1, N
        Y(I) = Y(I) + S*X(I)
      END DO
      T2 = OMP_GET_WTIME()
      PRINT *, 'computational time: ', T2-T1, 's'
      DEALLOCATE (X, Y)
      END
