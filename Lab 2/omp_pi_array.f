      PROGRAM OPENMP_PI_ARRAY
      INCLUDE 'omp_lib.h'
      INTEGER N, I, TID
      DOUBLE PRECISION X, STEP, PI, SUM1(4)
      PARAMETER (N=100000000)
      STEP = 1.0/N
      T1 = OMP_GET_WTIME()
!$OMP PARALLEL PRIVATE(X, TID)
      TID = OMP_GET_THREAD_NUM() + 1
!$OMP DO
      DO I=1, N
        X = (I-0.5)*STEP
        SUM1(TID) = SUM1(TID) + 1.0/SQRT(1.0-X*X)
      END DO
!$OMP END DO
!$OMP END PARALLEL
      T2 = OMP_GET_WTIME()
      PRINT *, 'computational time: ', T2-T1, 's'
      DO I=1, 4
        PI = PI + SUM1(I)*STEP
      END DO
      PRINT *, 'PI = ', PI
      END
