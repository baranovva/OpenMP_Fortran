      PROGRAM OPENMP_POISSON
      PARAMETER (N=101)
      DOUBLE PRECISION, ALLOCATABLE :: W(:,:),WOLD(:,:)
      ALLOCATE (W(N,N), WOLD(N,N))
C Задание начальных и граничных условий
      CALL INIT(N, W)
C Решение уравнения итерационным методом Якоби с параллелизацией
      CALL JACOBI(N, W, WOLD)
      DEALLOCATE (W, WOLD)
      END

      SUBROUTINE INIT(N, W)
      INTEGER N
      DOUBLE PRECISION W(N,*)
      W(2:N-1,2:N-1) = 0.5
      W(1,1:N) = 0.0
      W(N,1:N) = 0.0
      W(1:N,1) = 0.0
      W(1:N,N) = 0.0
      END SUBROUTINE INIT

      SUBROUTINE JACOBI(N, W, WOLD)
      INTEGER N, L, MAXIT
      DOUBLE PRECISION W(N,*), WOLD(N,*)
      DOUBLE PRECISION ERROR, EPS, H
      MAXIT=5000
      EPS = 1E-6
      ERROR = 1E6
      L = 1
      H = 2.0/DBLE(N-1)
      DO WHILE (L .LE. MAXIT .AND. ERROR .GT. EPS)
        ERROR = 0.0
!$OMP PARALLEL PRIVATE(I,J)
!$OMP WORKSHARE
      WOLD(1:N,1:N) = W(1:N,1:N)
!$OMP END WORKSHARE
!$OMP DO REDUCTION(MAX:ERROR)
        DO J = 2, N-1
          DO I = 2, N-1
            W(I,J) = 0.25 * (WOLD(I-1,J) + WOLD(I+1,J)
     &            + WOLD(I,J-1) + WOLD(I,J+1) + H*H)
            ERROR = MAX(ERROR, ABS(W(I,J) - WOLD(I,J)))
          END DO
        ENDDO
!$OMP END DO
!$OMP END PARALLEL
        L = L + 1
      END DO
      RETURN
      END SUBROUTINE JACOBI
