      SUBROUTINE WRITEDATAFILE(FILENAME,NMAX,EMAX,MMAX,SMAX,CAT,NODES,NN
     1                        ,ELEMS,NE,MATES,COUNTMAT,SECTS,COUNTSECT)
      IMPLICIT NONE
      
      INTEGER NMAX,EMAX,SMAX,MMAX
      CHARACTER (7) CAT
      DOUBLE PRECISION NODES(NMAX,*)
      INTEGER ELEMS(EMAX,*)
      DOUBLE PRECISION MATES(MMAX,*)
      DOUBLE PRECISION SECTS(SMAX,*)
      CHARACTER*80 FILENAME
      INTEGER I,J,NN,NE,COUNTMAT,COUNTSECT
      
      FILENAME(LEN_TRIM(FILENAME)-2:LEN_TRIM(FILENAME))='dat'
      
      OPEN(80,FILE=FILENAME)
      
* Display type of problem                                              *
      WRITE(80,*) '2DFRMAE'
      
* Display Nodes                                                        *      
      WRITE(80,*) 'NODES',NN
      DO I=1, NN
          WRITE(80,*) I,(NODES(I,J), J=1,2)
      ENDDO
      
* Display Elements                                                     *      
      WRITE(80,*) 'ELEMENTS',NE
      DO I=1, NE
          WRITE(80,*) I,(ELEMS(I,J), J=1,5)
      ENDDO
      
* Display Materials                                                    *
      WRITE(80,*) 'MATERIALS',COUNTMAT
      DO I=1, COUNTMAT
          WRITE(80,*) I,(MATES(I,J), J=1,4)
      ENDDO
      
* Display Sections                                                     *
      WRITE(80,*) 'SECTIONS',COUNTSECT
      DO I=1, COUNTSECT
          WRITE(80,*) I,(SECTS(I,J), J=1,3)
      ENDDO
      
      CLOSE(80)
      
      
      END
