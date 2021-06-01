************************************************************************
* This file is part of DynamicSiffness, a Fortran library that         * 
* implements the Dynamic Stiffness Method                              *
* Copyright (C) 2018  Jean-Baptiste CASIMIR,                           *
* Quartz Laboratory - Supmeca                                          *
* 3 rue Ferand Hainaut                                                 *
* 93407 SAINT-OUEN - FRANCE                                            *      
* jean-baptiste.casimir@supmeca.fr                                     *
*                                                                      *
* This program is distributed in the hope that it will be useful,      *
* but WITHOUT ANY WARRANTY; without even the implied warranty of       *
* This program is free software: you can redistribute it and/or modify *
* it under the terms of the GNU General Public License as published by *
* the Free Software Foundation, either version 3 of the License, or    *
* (at your option) any later version.                                  *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
* GNU General Public License for more details.                         *
*                                                                      *
* You should have received a copy of the GNU General Public License    *
* along with this program.  If not, see <http://www.gnu.org/licenses/>.*
************************************************************************
      
************************************************************************
*     This program is an example for using the fortran library         *
*     DynamicStiffness                                                 *
*                                                                      *
*     This program computes the harmonic response of a 2D beam         *
*     structure described in a data file (see ReadDataFile.for)        *
*                                                                      *  
*     A harmonic unit force is applied on a chosen DOF and the harmonic*
*     response is computed in the direction of another DOF             *
*                                                                      *      
*     The solutions are obtained thanks to a Lapack's procedure that   * 
*     solves the symmetric linear algebraic system (ZSYSV)             *    
************************************************************************

************************************************************************
*     Update for Timoshenko's theory                                   *
* 04/2021 by Tanguy BEVANCON                                           *
*tanguy.bevancon@supmeca.fr                                            *
************************************************************************
   
      PROGRAM DYNAMICSTIFFNESS
      IMPLICIT NONE

*     Declarations of static arrays that describe the structure        *
      INTEGER NMAX,EMAX,SMAX,MMAX
      PARAMETER (NMAX=100,EMAX=100,SMAX=100,MMAX=100)
      DOUBLE PRECISION NODES(NMAX,2),MATES(MMAX,4),SECTS(SMAX,3)
      INTEGER ELEMS(EMAX,5)

*     CAT is the category of problem (Yet only 2DFRAME)                *
      CHARACTER(7) CAT
      
*     NN,NE,NM,NS,NF are the number of nodes, the number of elements,  *
*     the number of materials, the number of sections and the number of*
*     processed frequencies respectively                               *                         
      INTEGER N,NN,NE,NM,NS,NF
      
*     W and F are circular frequency and frequency respectively        * 
*     [F1,F2] is the frequency range, FSTEP the frequency step         *      
      DOUBLE PRECISION W,F,PI,F1,F2,FSTEP

*     FDOF is the chosen DOF subjected to an unit harmonic force and   *
*     DDOF is the chosen DOF processed response                        *
      INTEGER FDOF,DDOF
      
*     KWST is the dynamic stiffness matrix of the structure for a given*
*     circular frequency and B if the force vector                     *                                 
      COMPLEX*16 KWST(NMAX*3,NMAX*3),B(NMAX*3)
      
*     Lapack's variables required by ZSYSV                             *
      COMPLEX*16 WORK(3*NMAX)
      INTEGER IPIV(NMAX),INFO
      
*     The name of the text data file                                   *
      CHARACTER*80 FILENAME
      
      INTEGER I,J
      
      

*     License informations                                             *
      WRITE(*,*) 'DynamicStiffness 1.0 Copyright (C) 2018 JB. CASIMIR'
      WRITE(*,*) "This program comes with ABSOLUTELY NO WARRANTY"
      WRITE(*,*) "This is free software, and you are welcome to redistri
     1bute it under certain conditions"
      PRINT*
      WRITE(*,*) 'Data File ? (******.dat) or (******.inp)'
      READ(*,*) FILENAME

*     The name of the data file is required                            *      
      WRITE(*,*) 'Data reading ...'
      
      IF (FILENAME(LEN_TRIM(FILENAME)-2:LEN_TRIM(FILENAME)).EQ.'dat')
     1 THEN
          CALL READDATAFILE(FILENAME,NMAX,EMAX,MMAX,SMAX,CAT,NODES,NN,
     1                  ELEMS,NE,MATES,NM,SECTS,NS)
      ELSEIF (FILENAME(LEN_TRIM(FILENAME)-2:LEN_TRIM(FILENAME)).
     1 EQ.'inp') THEN
          CALL READINPUTFILE(FILENAME,NMAX,EMAX,MMAX,SMAX,CAT,NODES,NN,
     1                  ELEMS,NE,MATES,NM,SECTS,NS)
          CALL WRITEDATAFILE(FILENAME,NMAX,EMAX,MMAX,SMAX,CAT,NODES,NN,
     1                  ELEMS,NE,MATES,NM,SECTS,NS)
      WRITE(*,*) 'File ', TRIM(FILENAME), ' created' 
      ENDIF
      
*      
*     The characteristics of the structure are displayed               *
      PRINT*
      WRITE(*,*) 'Type of structure : ',CAT
      WRITE(*,*) 'Number of nodes : ',NN
      WRITE(*,*) 'Number of elements : ',NE
      WRITE(*,*) 'Number of materials : ',NM
      WRITE(*,*) 'Number of sections : ',NS
      PRINT*
*     Display modified for the Timoshenko's theory : 04/2021           *
      WRITE(*,'(3A3,7A9,A7)') 'NB','N1','N2','DENS','E','TgD','Nu','S',
     1                        'IZ','kY','THEORY'
      DO I=1,NE
          WRITE(*,'(3I3,7D9.2,I7)') I,ELEMS(I,1),ELEMS(I,2),
     1    MATES(ELEMS(I,3),1),MATES(ELEMS(I,3),2),MATES(ELEMS(I,3),3),
     2    MATES(ELEMS(I,3),4),SECTS(ELEMS(I,4),1),SECTS(ELEMS(I,4),2),
     3    SECTS(ELEMS(I,4),3),ELEMS(I,5)
      ENDDO
     
      
*     The result file is opened                                        *    
      OPEN(10,FILE=FILENAME(1:INDEX(FILENAME,'.'))//'res')
      PI=ACOS(-1.0)
      PRINT*
      WRITE(*,*) 'Enter the frequency range : F1,F2'
      READ(*,*) F1,F2
      WRITE(*,*) 'Enter the number of processed frequencies'
      READ(*,*) NF
      WRITE(*,*) 'Enter the DOF subjected to an harmonic unit force'
      READ(*,*) FDOF
      WRITE(*,*) 'Enter the processed harmonic response DOF'
      READ(*,*) DDOF
      WRITE(10,*) NF
      FSTEP=(F2-F1)/(NF-1)
*     The main frequency loop                                          *
      F=F1
      DO N=1,NF
          W=2*PI*F
          DO I=1,3*NN
              DO J=1,3*NN
                  KWST(I,J)=0
              ENDDO
          ENDDO
   
*         Computation of the dynamic stiffness matrix                 *
          CALL DYNAMICSTIFFNESS2D(W,NODES,ELEMS,MATES,SECTS,NMAX,EMAX,
     1                            MMAX,SMAX,NE,3*NMAX,KWST)
*         Definition of the force vector                              *          
          DO I=1,3*NN
              B(I)=DCMPLX(0.0,0.0)    
          ENDDO
          B(FDOF)=DCMPLX(1.0,0.0)
          
*         Solving the linear algebraic system                         *          
          CALL ZSYSV('U',3*NN,1,KWST,3*NMAX,IPIV,B,3*NMAX,WORK,3*NMAX,
     1               INFO)

*         The chosen displacement is written on the result file       *          
          WRITE(10,*) F,CDABS(B(DDOF))
	  F=F+FSTEP
      ENDDO
      CLOSE(10)
      WRITE(*,*) 'Result file is '//FILENAME(1:INDEX(FILENAME,'.'))
     1            //'res'
      END
      
