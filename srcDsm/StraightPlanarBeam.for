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
*     This function computes the components of the dynamic stiffness   * 
*     matrix for a straight planar beam in a local basis. (XY) is the  *
*     plane of the beam.      
*                                                                      *
*     This matrix relates displacement vector D and external force     *
*     vector F at the tips of the AB beam for a given circular         *
*     frequency w according to KW . D = F where                        *
*     D = (UA,VA,TA,UB,VB,TB)^T and F = (FXA,FYA,MA,FXB,FYB,MB)^T      *
*     UA, VA are displacements along X-axis and Y-axis of tip A resp.  * 
*     TA is the rotation about Z-axis of tip A                         *
*     UB, VB are displacements along X-axis and Y-axis of tip B resp.  * 
*     TB is the rotation about Z-axis of tip B                         *    
*                                                                      *
*     Input Args :                                                     * 
*          W : circular frequency                                      *
*          S : section area                                            *
*          IZ : quadratic moment of the section about Z-axis           *
*          L : length of the beam                                      *      
*          RHO : mass density                                          *
*          E : complex Young's modulus including structural damping    *
*          TE : Bending theory                                         *
*                                                                      *
*     Output Args :                                                    *
*            KW : Computed dynamic stiffness matrix                    *
*                                                                      *      
*     Return value :                                                   *
*            unused logical error flag                                 *
*                                                                      *
*     Update in inputs for the use of Timoshenko's theory              *
************************************************************************      
      FUNCTION PLANARBEAM(W,S,IZ,L,RHO,NU,KY,E,TE,KW)
      IMPLICIT NONE
      LOGICAL PLANARBEAM

*     Traction and XY-Bending Dynamic Stiffness Functions              *
      LOGICAL TRACTION, XYBENDING,XYRAYLEIGHBENDING,XYTIMOSHENKOBENDING
      
*     Circular frequency                                               *
      DOUBLE PRECISION W
      
*     Geometrical properties of the beam                               *      
      DOUBLE PRECISION S,IZ,L
      
*     Material properties of the beam                                  *
      DOUBLE PRECISION RHO,NU
      COMPLEX*16 E

*     Bending theory                                                   *
      INTEGER TE
      
*     Timoshenko's theory   :   04/2021                                *
      DOUBLE PRECISION KY
      
*     Dynamic Stiffness Matrix                                         *
      COMPLEX*16 KW(6,6)

*     Traction and XY-Bending Dynamic Stiffness Matrices               *      
      COMPLEX*16 KT(2,2),KB(4,4)

      INTEGER I,J
      LOGICAL RET
      
*     Localisation vectors
      INTEGER LTRACTION(2),LBENDING(4)
      DATA LTRACTION,LBENDING/1,4,2,3,5,6/
      
*     Initialization of the Dynamic Stiffness Matrix
      DO I=1,6
          DO J=1,6
              KW(I,J)=0;    
          ENDDO
      ENDDO
            
*     Computation of traction and xy-bending dynamic stiffness matrices* 
      RET=TRACTION(W,S,L,RHO,E,KT)
      IF (TE.EQ.1) THEN
          RET=XYBENDING(W,S,IZ,L,RHO,E,KB)
      ELSEIF (TE.EQ.2) THEN
          RET=XYRAYLEIGHBENDING(W,S,IZ,L,RHO,E,KB)
      ELSEIF (TE.EQ.3) THEN
          RET=XYTIMOSHENKOBENDING(W,S,IZ,L,RHO,E,NU,KY,KB)
      ENDIF
      
*     Traction Components                                              * 
      DO I=1,2
          DO J=1,2
              KW(LTRACTION(I),LTRACTION(J))=KT(I,J)    
          ENDDO
      ENDDO
      
*     Bending Components                                               *
      DO I=1,4
          DO J=1,4
              KW(LBENDING(I),LBENDING(J))=KB(I,J)    
          ENDDO
      ENDDO
      
      PLANARBEAM=.TRUE.   
            
      END
