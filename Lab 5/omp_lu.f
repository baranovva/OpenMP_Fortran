      PROGRAM G
      INTEGER I,J,K,S,N
      PARAMETER (N=1000)
      INTEGER(8) T1, T2
      REAL(4) T
      REAL(8) Q, W 
      REAL(8), ALLOCATABLE , DIMENSION(:,:):: A, R2
      REAL(8), ALLOCATABLE , DIMENSION(:):: X, B, R1
      ALLOCATE (A(N,N), B(N), X(N), R1(N), R2(N,N))
    
      !DO I=1,N  ! ввод данных с консоли
      !   WRITE(*,*)  I,'URAVNENIE'
      !   READ(*,*) A(I,1:N),B(I)
      !END DO 

      CALL INIT_RANDOM_SEED()
      CALL RANDOM_NUMBER(R1)
      B(1:N) = R1(1:N)
      CALL RANDOM_NUMBER(R2)
      FORALL(I = 1:N,J = 1:N) A(I,J) = R2(I,J)
      DEALLOCATE (R1, R2)

      !WRITE(*,*) A
      !WRITE(*,*) '=============='
      !WRITE(*,*) B
      !WRITE(*,*) '=============='
      CALL SYSTEM_CLOCK(COUNT=T1)
!$OMP PARALLEL PRIVATE(I,J,K,W,Q)
!$OMP DO 
      DO K=1,N-1
        W=ABS(A(K,K))
        IF (W<1.0D-20) THEN
            DO S=K+1,N
                IF (ABS(A(S,K))<=W) CYCLE
                W=ABS(A(S,K))
            END DO
        END IF 
        DO I=K+1,N
            Q=A(I,K)/A(K,K)
            DO J=K+1,N
                A(I,J)= A(I,J)-Q*A(K,J)
            END DO
            B(I)=B(I)-Q*B(K)
        END DO
      END DO
!$OMP END DO
!$OMP SINGLE
      X(N)=B(N)/A(N,N)   
!$OMP END SINGLE  
!$OMP DO 
      DO I=N-1,1,-1
        Q=0
        DO J=I+1,N
            Q=Q+A(I,J)*X(J)
        END DO
        X(I)=(B(I)-Q)/A(I,I)
      END DO
!$OMP END DO
!$OMP END PARALLEL
      CALL SYSTEM_CLOCK(COUNT=T2)
      T = REAL(T2-T1)/10**6
      WRITE (*,*) 'computational time: ', T, 's'
      !WRITE(*,*) X 
      DEALLOCATE (A, B, X)
      END PROGRAM
    
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
   