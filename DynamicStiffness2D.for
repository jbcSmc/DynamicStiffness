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
*     This subroutine builds the dynamic stiffness matrix of a whole   *
*     planar beam structure                                            * 
*                                                                      *
*     This matrix relates displacement vector D and external force     *
*     vector F at the nodes of the structure for a given circular      *
*     frequency w according to KW . D = F where                        *
*     D = (U1,V1,T1,...,UN,VN,TN)^T and                                *
*     F = (FX1,FY1,M1,...,FXN,FYN,MN)^T                                *
*     Ui, Vi are displacements along X-axis and Y-axis of node i resp. * 
*     Ti is the rotation about Z-axis of node i                        *
*                                                                      *
*     Input Args :                                                     * 
*          W : circular frequency                                      *
*          NODES : Coordinates of nodes (see ReadDataFile.for)         *
*          ELEMS : Table of elements (see ReadDataFile.for)            *
*          MATES : Table of materials (see ReadDataFile.for)           *
*          SECTS : Table of sections (see ReadDataFile.for)            *
*          NMAX : The leading dimension of NODES                       *      
*          EMAX : The leading dimension of ELEMS                       *
*          MMAX : The leading dimension of MATES                       *
*          SMAX : The leading dimension of SECTS                      *
*          NE : The number of elements                                 *
*          DOFMAX : The leading dimension of KWST                      *
*                                                                      *
*     Output Args :                                                    *
*            KWST : The dynamic stiffness matrix of the whole structure*
*                                                                      *      
************************************************************************  
      
      SUBROUTINE DYNAMICSTIFFNESS2D(W,NODES,ELEMS,MATES,SECTS,NMAX,EMAX,
     1                               MMAX,SMAX,NE,DOFMAX,KWST)
      IMPLICIT NONE
      
*     Input arguments                                                  *
      INTEGER NMAX,EMAX,MMAX,SMAX,DOFMAX,ELEMS(EMAX,*)
      DOUBLE PRECISION W,NODES(NMAX,*),MATES(MMAX,*),SECTS(SMAX,*)
      INTEGER NE
      
*     Output argument                                                  *
      COMPLEX*16 KWST(DOFMAX,DOFMAX)

*     Local variables                                                  *
      INTEGER E,IE,JE,I,J,TE
      DOUBLE PRECISION L,S,IZ,RHO,X(2),ER,ETG
      COMPLEX*16 YOUNG,KW(6,6)
      LOGICAL RET

*     External functions                                               *      
      LOGICAL GLOBALPLANARBEAM
      
*     Main loop on elements                                            *
      DO E=1,NE
*         Computing the element's length L                             *
          IE=ELEMS(E,1)
          JE=ELEMS(E,2)
          X(1)=NODES(JE,1)-NODES(IE,1)
          X(2)=NODES(JE,2)-NODES(IE,2)
          L=DSQRT(X(1)**2+X(2)**2)
          
*         Extraction of material and section characteristics           *
          S=SECTS(ELEMS(E,4),1)
          IZ=SECTS(ELEMS(E,4),2)
          RHO=MATES(ELEMS(E,3),1)
          ER=MATES(ELEMS(E,3),2)
          ETG=MATES(ELEMS(E,3),3)

*         Calculating complex Young modulus                            *
          YOUNG=DCMPLX(ER,ER*ETG)
          
*         Computing the element dynamic stiffness matrix in a global   *
*         coordinate system                                            *
          TE=ELEMS(E,5)         
          RET=GLOBALPLANARBEAM(W,S,IZ,L,RHO,YOUNG,X,TE,KW)

*         Assembling the processed matrix                              *          
          CALL PLANARASSEMBLY(KW,IE,JE,DOFMAX,KWST)
      ENDDO
      END