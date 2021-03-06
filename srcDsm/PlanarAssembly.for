************************************************************************
* This file is part of DynamicSiffness, a Fortran library that         * 
* implements the Dynamic Stiffness Method                              *
* Copyright (C) 2017  Jean-Baptiste CASIMIR,                           *
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
*     This subroutine add an element's dynamic KW stiffness matrix to  *
*     to the dynamic stiffness matrix of a whole planar beam structure *
*                                                                      *
*     This matrix relates displacement vector D and external force     *
*     vector F at the nodes of the structure for a given circular      *
*     frequency w according to KWST . D = F where                      *
*     D = (U1,V1,T1,...,UN,VN,TN)^T and                                *
*     F = (FX1,FY1,M1,...,FXN,FYN,MN)^T                                *
*     Ui, Vi are displacements along X-axis and Y-axis of node i resp. * 
*     Ti is the rotation about Z-axis at node i                        *
*                                                                      *
*     Input Args :                                                     * 
*          KW : The element's dynamic stiffness matrix                 *
*          IE : The number of the first node of the beam element       *
*          JE : The number of the second node of the beam element      *
*          DOFMAX : The leading dimension of the matrix KWST           *      
*                                                                      *
*     Output Args :                                                    *
*            KWST : The dynamic stiffness matrix of the whole structure*
*                                                                      *      
************************************************************************  
      
      SUBROUTINE PLANARASSEMBLY(KW,IE,JE,DOFMAX,KWST)
*  Input Args                                                          *     
      COMPLEX*16 KW(6,6)
      INTEGER IE,JE,DOFMAX

* Output Args                                                          *
      COMPLEX*16 KWST(DOFMAX,*)

      INTEGER I,J,IG,JG
      
* Addition of the top-left submatrix block                             *     
      IG=(IE-1)*3
      JG=(IE-1)*3
      DO I=1,3
          DO J=1,3
              KWST(IG+I,JG+J)=KWST(IG+I,JG+J)+KW(I,J)
          ENDDO
      ENDDO
      
* Addition of the top-right submatrix block                            *      
      IG=(IE-1)*3
      JG=(JE-1)*3
      DO I=1,3
          DO J=1,3
              KWST(IG+I,JG+J)=KWST(IG+I,JG+J)+KW(I,3+J)
          ENDDO
      ENDDO
    
* Addition of the bottom-left submatrix block                          *
      IG=(JE-1)*3
      JG=(IE-1)*3
      DO I=1,3
          DO J=1,3
              KWST(IG+I,JG+J)=KWST(IG+I,JG+J)+KW(3+I,J)
          ENDDO
      ENDDO

* Addition of the bottom-right submatrix block                         *      
      IG=(JE-1)*3
      JG=(JE-1)*3
      DO I=1,3
          DO J=1,3
              KWST(IG+I,JG+J)=KWST(IG+I,JG+J)+KW(3+I,3+J)
          ENDDO
      ENDDO

      END
