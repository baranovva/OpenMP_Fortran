      PROGRAM OPENMP_MVM
      INCLUDE 'omp_lib.h'
      INTEGER I, J, N
      PARAMETER (N = 10000)
      DOUBLE PRECISION T1, T2
      DOUBLE PRECISION, ALLOCATABLE :: A(:,:), X(:), Y(:), R1(:), R2(:,:)
      ALLOCATE (A(N,N), X(N), Y(N), R1(N), R2(N,N))
      
      !READ(*,*) A
      !X = (/10,2/)
    
      CALL INIT_RANDOM_SEED()
      CALL RANDOM_NUMBER(R1)
      X(1:N) = R1(1:N)
      CALL RANDOM_NUMBER(R2)
      FORALL(I = 1:N,J = 1:N) A(I,J) = R2(I,J)
      DEALLOCATE (R1, R2)

      T1 = OMP_GET_WTIME()
!$OMP PARALLEL DO PRIVATE(J,I)
      DO J = 1, N
        Y(J) = 0.0
        DO I = 1, N
          Y(I) = Y(I) + A(I,J)*X(J)
        END DO
      END DO
!$OMP END PARALLEL DO
      T2 = OMP_GET_WTIME()

      PRINT *, 'computational time: ', T2-T1, 's'
      !WRITE (*,*) 'A', A
      !WRITE (*,*) '======'
      !WRITE (*,*) 'X',X
      !WRITE (*,*) '======'
      !WRITE (*,*) 'Y', Y
      DEALLOCATE (A, X, Y)
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
