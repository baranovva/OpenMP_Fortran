      PROGRAM OPENMP_DDOT_DO
      INCLUDE 'omp_lib.h'
      INTEGER I, N
      PARAMETER (N = 100000000)
      DOUBLE PRECISION R, T1, T2
      DOUBLE PRECISION, ALLOCATABLE :: Y(:), X(:)
      ALLOCATE (Y(N), X(N))
      X(1:N) = 1.0
      Y(1:N) = 2.0
      T1 = OMP_GET_WTIME()
!$OMP PARALLEL DO REDUCTION(+:R)
      DO I=1, N
        R = R + X(I)*Y(I)
      END DO
!$OMP END PARALLEL DO
      T2 = OMP_GET_WTIME()
      PRINT *, 'computational time: ', T2-T1, 's'
      DEALLOCATE (X, Y)
      END
