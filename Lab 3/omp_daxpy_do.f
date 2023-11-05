      PROGRAM OPENMP_DO
      INCLUDE 'omp_lib.h'
      INTEGER Q, I, N
      PARAMETER (N=100000000)
      REAL(8) A, T1, T2, R(2)
      REAL(8), ALLOCATABLE :: X(:)

      CALL INIT_RANDOM_SEED()
      CALL RANDOM_NUMBER(R)
      A = R(1)
      ALLOCATE (X(N))
      X(1:N) = R(2)

      T1 = OMP_GET_WTIME()
      DO Q = 1,2
!$OMP PARALLEL DO SCHEDULE(RUNTIME)
      DO I=1, N
        X(I) = X(I) + A
      END DO
!$OMP END PARALLEL DO
      END DO
      T2 = OMP_GET_WTIME()
      PRINT *, 'computational time: ', T2-T1, 's'
      DEALLOCATE (X)
      END
    
      SUBROUTINE INIT_RANDOM_SEED()
      INTEGER :: I, N, CLOCK
      INTEGER, DIMENSION(:), ALLOCATABLE :: SEED

      CALL RANDOM_SEED(SIZE = N)
      ALLOCATE(SEED(N))
      CALL SYSTEM_CLOCK(COUNT=CLOCK)
      SEED = CLOCK + 37 * (/ (I - 1, I = 1, N) /)
      CALL RANDOM_SEED(PUT = SEED)

      DEALLOCATE(SEED)
      END SUBROUTINE
    
