      PROGRAM OPENMP_3D_ARRAY
      INCLUDE 'omp_lib.h'
      INTEGER I, J, K, L, NI, NJ, NK, INFILE, OUTFILE
      DOUBLE PRECISION T1, T2, S
      DOUBLE PRECISION, ALLOCATABLE :: X(:,:,:), Y(:,:,:), Z(:,:,:)

      OPEN(1, FILE="CUBE.MSH")
      READ(1,*) NI, NJ, NK 
      ALLOCATE (X(NI,NJ,NK), Y(NI,NJ,NK), Z(NI,NJ,NK))
      DO K=1, NK 
        DO J=1, NJ 
         DO I=1, NI 
          READ(1,*) X(I,J,K), Y(I,J,K), Z(I,J,K) 
         END DO 
        END DO
      END DO
      CLOSE(INFILE)

      T1 = OMP_GET_WTIME()
!$OMP PARALLEL PRIVATE(I,J,K)
      DO L=1,10
!$OMP DO
      DO K=2, NK-1
       DO J=2, NJ-1
        DO I=2, NI-1
            S=abs((X(I+1,J,K) - X(I,J,K))*(X(I,J,K+1) - X(I,J,K)))
        END DO
       END DO
      END DO
!$OMP END DO
      END DO
!$OMP END PARALLEL
      T2 = OMP_GET_WTIME()
      PRINT *, 'computational time: ', T2-T1, 's'

      OPEN(2, FILE="CUBE_NEW.MSH")
      WRITE(2,*) NI, NJ, NK 
      DO K=1, NK 
        DO J=1, NJ 
         DO I=1, NI 
          WRITE(2,*) S 
         END DO 
        END DO
      END DO
      CLOSE(2)
      DEALLOCATE(X,Y,Z)

      END
