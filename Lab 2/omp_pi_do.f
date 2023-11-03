      PROGRAM OPENMP_PI_DO
      INTEGER N, I
      DOUBLE PRECISION X, STEP, PI, SUM /0.0/
      PARAMETER (N=100000000)
      STEP = 1.0/N
      T1 = OMP_GET_WTIME()
!$OMP PARALLEL PRIVATE(X)
!$OMP DO REDUCTION(+:SUM)
      DO I=1, N
        X = (I-0.5)*STEP
        SUM = SUM + 1.0/SQRT(1.0-X*X)
      END DO
!$OMP END PARALLEL
      T2 = OMP_GET_WTIME()
      PRINT *, 'computational time: ', T2-T1, 's'
      PI = STEP * SUM
      WRITE(*,*) PI
      END
