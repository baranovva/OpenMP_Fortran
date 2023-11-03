      PROGRAM OPENMP_PI_ATOMIC
      INTEGER I, N
      DOUBLE PRECISION X, STEP, PI /0.0/, THREAD_SUM /0.0/, SUM /0.0/
      N = 100000000
      STEP = 1.0/N
      T1 = OMP_GET_WTIME()
!$OMP PARALLEL PRIVATE(X) FIRSTPRIVATE(THREAD_SUM)
!$OMP DO
      DO I=1, N
        X = (I-0.5)*STEP
        THREAD_SUM = THREAD_SUM + 1.0/SQRT(1.0-X*X)
      END DO
!$OMP END DO
!$OMP ATOMIC
      SUM = SUM + THREAD_SUM
!$OMP END PARALLEL
      T2 = OMP_GET_WTIME()
      PRINT *, 'computational time: ', T2-T1, 's'
      PI = SUM * STEP
      WRITE(*,*) PI
      END
