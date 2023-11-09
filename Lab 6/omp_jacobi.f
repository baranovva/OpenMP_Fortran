      PROGRAM OPENMP_POISSON
      INTEGER, PARAMETER:: N=301, K=601
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:):: W, WOLD
      ALLOCATE (W(N,K), WOLD(N,K))
      CALL INIT(N, K, W)
      CALL JACOBI(N, K, W, WOLD)
      DEALLOCATE (W, WOLD)
      END
    
      SUBROUTINE INIT(N, K, W)
      INTEGER N, K
      DOUBLE PRECISION W(N,K)
      W(2:N-1,2:K-1) = 0.5
      W(1,1:K) = 0.0
      W(N,1:K) = 0.0
      W(1:N,1) = 0.0
      W(1:N,K) = 0.0
      END SUBROUTINE
    
      SUBROUTINE JACOBI(N, K, W, WOLD)
      INCLUDE 'omp_lib.h'
      INTEGER N, K, L, MAXIT, I, J, S, Q
      DOUBLE PRECISION W(N,K), WOLD(N,K)
      DOUBLE PRECISION ERROR, EPS, H, T1, T2
      MAXIT=10000000000
      EPS = 1E-6
      ERROR = 1E6
      L = 1
      Q = 1
      H = 2.0/DBLE(N-1)
      T1 = OMP_GET_WTIME()
      DO WHILE (L .LE. MAXIT .AND. ERROR .GT. EPS)
      ERROR = 0.0
!$OMP PARALLEL PRIVATE(I,J,S)
      DO S=1, Q
!$OMP WORKSHARE
      WOLD(1:N,1:K) = W(1:N,1:K)
!$OMP END WORKSHARE
!$OMP DO
      DO J = 2, K-1
        DO I = 2, N-1
            W(I,J) = 0.25*(WOLD(I-1,J)+WOLD(I+1,J)+WOLD(I,J-1)+WOLD(I,J+1)+H*H)
        END DO
      END DO
!$OMP END DO
      END DO
!$OMP DO REDUCTION(MAX:ERROR)
      DO J = 2, K-1
        DO I = 2, N-1
            ERROR = MAX(ERROR, ABS(W(I,J) - WOLD(I,J)))
        END DO
      ENDDO
!$OMP END DO
!$OMP END PARALLEL
      L = L + Q
      END DO
      T2 = OMP_GET_WTIME()
      PRINT *, 'computational time: ', T2-T1, 's'
      WRITE (*,*) W(150,300)
      !OPEN(10,FILE='Res.xlsx')  
      !DO I=1, N
      !  DO J=1, K
      !      WRITE (10,*) I, J, W(i,j)
      !  END DO
      !END DO
      !CLOSE(10)
      RETURN
      END SUBROUTINE
