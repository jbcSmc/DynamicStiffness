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
*     This function computes the dynamic stiffness matrix for a        *
*     straight planar beam in a global basis. (xy) is the              *
*     plane of the beam. The first principal direction of inertia  Y  *
*     belongs to global xy-plane
*                                                                      *
*     This matrix relates displacement vector d and external force     *
*     vector f at the tips of the AB beam for a given circular         *
*     frequency w according to KW . d = f where                        *
*     d = (uA,vA,tA,uB,vB,tB)^T and f = (fxA,fyA,mA,fxB,fyB,mB)^T      *
*     uA, vA are displacements along x-axis and y-axis of tip A resp.  * 
*     tA is the rotation about z-axis of tip A                         *
*     uB, vB are displacements along x-axis and y-axis of tip B resp.  * 
*     tB is the rotation about z-axis of tip B                         *    
*                                                                      *
*     Input Args :                                                     * 
*          W : circular frequency                                      *
*          S : section area                                            *
*          IZ : quadratic moment of the section about Z-axis           *
*          L : length of the beam                                      *      
*          RHO : mass density                                          *
*          E : complex Young's modulus including structural damping    *
*          X : longitudinal direction of the beam.                     *
*          TE : bending theory                                         *
*                                                                      *
*     Output Args :                                                    *
*            KW : Computed dynamic stiffness matrix                    *
*                                                                      *      
*     Return value :                                                   *
*            unused logical error flag                                 *
************************************************************************      
      FUNCTION GLOBALPLANARBEAM(W,S,IZ,L,RHO,E,X,TE,KW)
      IMPLICIT NONE
      LOGICAL GLOBALPLANARBEAM

*     Local Dynamic Stiffness Martrix Function                         *
      LOGICAL PLANARBEAM
      
*     Circular frequency                                               *
      DOUBLE PRECISION W
      
*     Geometrical properties of the beam                               *      
      DOUBLE PRECISION S,IZ,L
      
*     Material properties of the beam                                  *
      DOUBLE PRECISION RHO
      COMPLEX*16 E

*     Direction of the beam                                            *
      DOUBLE PRECISION X(2)
      
*     Bending Theory                                                   *
      INTEGER TE
      
*     Global Dynamic Stiffness Matrix                                  *
      COMPLEX*16 KW(6,6)

*     Local Dynamic Stiffness Matrices                                 *      
      COMPLEX*16 LKW(6,6)

*     Rotation matrix and its transpose                                *      
      DOUBLE PRECISION P(6,6),PT(6,6)
      
      INTEGER I,J
      LOGICAL RET
      COMPLEX*16 MAT(6,6)
      
      DATA P/36*0/

*     Computation of the local dynamic stiffness matrix                *   
      RET=PLANARBEAM(W,S,IZ,L,RHO,E,TE,LKW)
     
*     Computation of the rotation matrix                               *
      P(1,1)=X(1)/L
      P(2,1)=X(2)/L
      P(1,2)=-P(2,1)
      P(2,2)=P(1,1)
      P(3,3)=1
      P(4,4)=P(1,1)
      P(5,4)=P(2,1)
      P(4,5)=P(1,2)
      P(5,5)=P(2,2)
      P(6,6)=1 
      
*     Basis change                                                     *
      KW=MATMUL(TRANSPOSE(P),MATMUL(LKW,P))
      
      GLOBALPLANARBEAM=.TRUE.   
            
      END