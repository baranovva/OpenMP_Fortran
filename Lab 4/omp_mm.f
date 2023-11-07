      PROGRAM OPENMP_MM
      INCLUDE 'omp_lib.h'
      INTEGER N, I, J
      PARAMETER (N=10000)
      REAL(8) T1, T2
      REAL(8), ALLOCATABLE :: A(:,:), B (:,:), C(:,:), R(:,:)
      ALLOCATE (A(N,N), B(N,N), C(N,N), R(N,N))

      !READ(*,*) A
      !READ(*,*) B
      
      CALL INIT_RANDOM_SEED()
      CALL RANDOM_NUMBER(R)
      FORALL(I = 1:N,J = 1:N) A(I,J) = R(I,J)
      CALL RANDOM_NUMBER(R)
      FORALL(I = 1:N,J = 1:N) B(I,J) = R(I,J)
      DEALLOCATE (R)
      FORALL(I = 1:N,J = 1:N) C(I,J) = 0.0

      T1 = OMP_GET_WTIME()
!$OMP PARALLEL DO PRIVATE(J, I)
      DO J = 1, N
        DO I = 1, N
            C(I,J) = A(I,J) + B(I,J)
        END DO
      END DO
!$OMP END PARALLEL DO
      T2 = OMP_GET_WTIME()
      PRINT *, 'computational time: ', T2-T1, 's'
      !WRITE (*,*) 'A', A
      !WRITE (*,*) '======'
      !WRITE (*,*) 'B',B
      !WRITE (*,*) '======'
      !WRITE (*,*) 'C', C
      DEALLOCATE (A, B, C)
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