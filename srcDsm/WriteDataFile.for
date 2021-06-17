************************************************************************
* This file is part of DynamicSiffness, a Fortran library that         * 
* implements the Dynamic Stiffness Method                              *
* Copyright (C) 2021 BEVANCON Tanguy,                                  *
* Quartz Laboratory - Supmeca                                          *
* 3 rue Ferand Hainaut                                                 *
* 93407 SAINT-OUEN - FRANCE                                            *
* tanguy.bevancon@supmeca.fr                                           *
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
*     This subroutine create data files (as read in the readdatafile   *
*     subroutine) from the parameters stacked in the tables from the   *
*     readinputfile subroutine                                         *
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
*     1 R1 E1 D1 NU1                                                   *
*     2 R2 E2 D2 NU2                                                   *
*     ...                                                              *
*     NM RNM ENM DNM NUNM                                              *
*     SECTIONS NS                                                      *
*     1 S1 IZ1 KY1                                                     *
*     2 S2 IZ2 KY2                                                     *
*     ...                                                              * 
*     NS SNS IZNS KYNS                                                 * 
************************************************************************ 

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
      
* Rewrite the file with .dat                                           *
      FILENAME(LEN_TRIM(FILENAME)-2:LEN_TRIM(FILENAME))='dat'
      
      OPEN(80,FILE=FILENAME)
      
* Write type of problem                                                *
      WRITE(80,*) CAT
      
* Write Nodes                                                          *      
      WRITE(80,*) 'NODES',NN
      DO I=1, NN
          WRITE(80,*) I,(NODES(I,J), J=1,2)
      ENDDO
      
* Write Elements                                                       *      
      WRITE(80,*) 'ELEMENTS',NE
      DO I=1, NE
          WRITE(80,*) I,(ELEMS(I,J), J=1,5)
      ENDDO
      
* Write Materials                                                      *
      WRITE(80,*) 'MATERIALS',COUNTMAT
      DO I=1, COUNTMAT
          WRITE(80,*) I,(MATES(I,J), J=1,4)
      ENDDO
      
* Write Sections                                                       *
      WRITE(80,*) 'SECTIONS',COUNTSECT
      DO I=1, COUNTSECT
          WRITE(80,*) I,(SECTS(I,J), J=1,3)
      ENDDO
      
      CLOSE(80)
      
      END
