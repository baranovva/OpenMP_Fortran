      PROGRAM INT_PI
      INTEGER N, I
      DOUBLE PRECISION X, STEP, PI, SUM /0.0/, T1, T2
      PARAMETER (N=100000000)
      STEP = 1.0/N
      T1 = OMP_GET_WTIME()
      DO I=1, N
        X = (I-0.5)*STEP
        SUM = SUM + 1.0/SQRT(1.0-X*X)
      END DO
      T2 = OMP_GET_WTIME()
      PRINT *, 'computational time: ', T2-T1, 's'
      PI = STEP * SUM
      WRITE(*,*) PI
      END
