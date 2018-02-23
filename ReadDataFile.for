************************************************************************
* This file is part of DynamicSiffness, a Fortran library that         * 
* implements the Dynamic Stiffness Method                              *
* Copyright (C) 2018  Jean-Baptiste CASIMIR,                           *
* Quartz Laboratory - Supmeca                                          *
* 3 rue Ferand Hainaut                                                 *
* 93407 SAINT-OUEN - FRANCE                                            *
* jean-baptiste.casimir@supmeca.fr                                     *
*                                                                      *
* This program is free software: you can redistribute it and/or modify *
* it under the terms of the GNU General Public License as published by *
* the Free Software Foundation, either version 3 of the License, or    *
* (at your option) any later version.                                  *
*                                                                      *
* This program is distributed in the hope that it will be useful,      *
* but WITHOUT ANY WARRANTY; without even the implied warranty of       *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
* GNU General Public License for more details.                         *
*                                                                      *
* You should have received a copy of the GNU General Public License    *
* along with this program.  If not, see <http://www.gnu.org/licenses/>.*
************************************************************************  
      
************************************************************************
*     This subroutine reads data files that describe a 2D beam frame   *
*     The data file is a text file as described below                  *
*                                                                      *
*     ST                                                               *
*     NODES NN                                                         *
*     1 XN1 YN1                                                        *
*     2 XN2 YN2                                                        *
*     ...                                                              *
*     NN XNN YNN                                                       *
*     ELEMENTS NE                                                      *
*     1 N11 N21 M1 S1 T1                                               *
*     2 N12 N22 M2 S2 T2                                               *
*     ...                                                              *
*     NE N1NE N2NE MNE SNE TNE                                         *
*     MATERIALS NM                                                     *
*     1 R1 E1 D1                                                       *
*     2 R2 E2 D2                                                       *
*     ...                                                              *
*     NM RNM ENM DNM                                                   *
*     SECTIONS NS                                                      *
*     1 S1 IZ1                                                     *
*     2 S2 IZ2                                                     *
*     ...                                                              *      
*     NS SNS IZNS                                                 * 
*                                                                      *      
*     Where :                                                          *
*     ST : Type of structure (ONLY 2DFRAME)                            *
*     NN : Number of nodes                                             *
*     XNI,YNI : Coordinates of node I                                  *
*     NE : Number of elements                                          *
*     N1I,N2I : Node 1 and Node 2 of element I                         *
*     MI : Constitutive material of element I                          *
*     SI : Section of element I                                        *
*     TI : Type of element I (1 = Bernoulli, 2 = Rayleigh)             *
*     NM : Number of materials                                         *
*     RI : Mass density of material I                                  *
*     EI : Young modulus of material I                                 *
*     DI : Damping (loss angle) of material I                          *
*     NS : Number of sections                                          *
*     SI : Area o section I                                            *
*     IZI : Quadratic moment of inertia of section I                   *
*                                                                      *
*     Input Args :                                                     * 
*          FILENAME : The data filename                                *
*          NMAX : The leading dimension of array NODES                 *
*          EMAX : The leading dimension of array ELEMS                 *
*          MMAX : The leading dimension of array MATES                 *      
*          SMAX : The leading dimension of array SECTS                 *
*                                                                      *
*     Output Args :                                                    *
*          CAT :  Category of problem (ONLY 2DFRAME)                   *
*          NODES : Coordinates of nodes (XI,YI)                        *      
*          NN : number of nodes                                        *
*          ELEMS : Elements' table (N1I,N2I,SI,MI,TI)                  *
*          NE : number of elements                                     *
*          MATES : Materials' table (RI,EI,DI)                         *
*          NM : number of materials                                    *
*          SECTS : Sections' table (SI,IZI,IOI)                        *
*          NS : number of sections                                     *
************************************************************************   
      SUBROUTINE READDATAFILE(FILENAME,NMAX,EMAX,MMAX,SMAX,CAT,NODES,NN,
     1                        ELEMS,NE,MATES,NM,SECTS,NS)
      IMPLICIT NONE

*     Input Arguments                                                  * 
      CHARACTER(*) FILENAME
      INTEGER NMAX,EMAX,MMAX,SMAX

*     OutputArguments                                                  *      
      CHARACTER(7) CAT
      DOUBLE PRECISION NODES(NMAX,*)
      INTEGER NN,ELEMS(EMAX,*),NE
      DOUBLE PRECISION MATES(MMAX,*)
      INTEGER NM
      DOUBLE PRECISION SECTS(SMAX,*)
      INTEGER NS
      
* Local variables                                                      *
      CHARACTER(9) CODE
      INTEGER I,J,K
      
* Opening the datafile and reading the first line                      *
      OPEN(10,FILE=FILENAME)
      READ(10,'(A7)') CAT
      
* Seeking and reading NN nodes                                         *
      DO WHILE(CODE.NE.'NODES')
          READ(10,*) CODE,NN
      ENDDO
      DO I=1,NN
          READ(10,*) K,NODES(K,1),NODES(K,2)
      ENDDO

* Seeking and reading NE elements                                      *
      DO WHILE(CODE.NE.'ELEMENTS')
          READ(10,*) CODE,NE
      END DO
      DO I=1,NE
          READ(10,*) K,(ELEMS(K,J),J=1,5)
      ENDDO
      
* Seeking and reading materials.                                       *                     
      DO WHILE(CODE.NE.'MATERIALS')
          READ(10,*) CODE,NM
      END DO
      DO I=1,NM
          READ(10,*) K,(MATES(K,J),J=1,3)
      ENDDO
      
* Seeking and reading sections.                                        *
      DO WHILE(CODE.NE.'SECTIONS')
          READ(10,*) CODE,NS
      END DO
      DO I=1,NS
          READ(10,*) K,(SECTS(K,J),J=1,2)
      ENDDO

      CLOSE(10)

      END