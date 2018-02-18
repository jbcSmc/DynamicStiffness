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
*     This function computes the XY-bending components of the dynamic  *
*     stiffness matrix for a straight beam in a local basis.           *
*                                                                      *
*     This matrix relates traction displacement vector D and           *
*     external force vector F at the tips of the AB beam for a given   *
*     frequency w according to KW . D = F where T = (VA,TA,VB,TB)^T    *
*     and F = (FA,MA,FB,MB)^T                                          *
*     VA is the vertical displacement along Y-axis of tip A            *
*     TA is the rotation about Z-axis of tip A                         *      
*     VB is the vertical displacement along Y-axis of tip B            *
*     TB is the rotation about Z-axis of tip B                         *
*     FA is the vertical external force along Y-axis applied on tip A  *      
*     FB is the vertical external force along Y-axis applied on tip B  *      
*     MA is the external moment about Z-axis applied on tip A          *
*     MB is the external moment about Z-axis applied on tip B          *     
*                                                                      *
*     Input Args :                                                     * 
*          W : circular frequency                                      *
*          S : section area                                            *
*          IZ : quadratic moment of the section about Z-axis           *
*          L : length of the beam                                      *      
*          RHO : mass density                                          *
*          E : complex Young's modulus including structural damping    *
*                                                                      *
*     Output Args :                                                    *
*            KW : Computed dynamic stiffness matrix                    *
*                                                                      *      
*     Return value :                                                   *
*            unused logical error flag                                 *
************************************************************************      
      FUNCTION XYBENDING(W,S,IZ,L,RHO,E,KW)
      IMPLICIT NONE
      LOGICAL XYBENDING
      
*     Circular frequency                                               *
      DOUBLE PRECISION W
      
*     Geometrical properties of the beam                               *      
      DOUBLE PRECISION S,IZ,L
      
*     Material properties of the beam                                  *
      DOUBLE PRECISION RHO
      COMPLEX*16 E
      
*     XY-Bending Dynamic Stiffness Matrix                              *      
      COMPLEX*16 KW(4,4)

*     Complex*16 hyperbolic functions                                  *
      COMPLEX*16 CDCOSH, CDSINH

*     Wave number      
      COMPLEX*16 K
      
      COMPLEX*16 FACT
      
      K=(W*W*RHO*S/E/IZ)**0.25
      FACT=K*E*IZ/(1-CDCOS(K*L)*CDCOSH(K*L))
            
      KW(1,1)=FACT*K*K*(CDCOS(K*L)*CDSINH(K*L)+CDSIN(K*L)*CDCOSH(K*L))
      KW(1,2)=FACT*K*CDSIN(K*L)*CDSINH(K*L)
      KW(1,3)=FACT*K*K*(-CDSIN(K*L)-CDSINH(K*L))
      KW(1,4)=FACT*K*(CDCOSH(K*L)-CDCOS(k*L))
      KW(2,1)=KW(1,2)
      KW(2,2)=FACT*(CDSIN(K*L)*CDCOSH(K*L)-CDCOS(K*L)*CDSINH(K*L))
      KW(2,3)=-KW(1,4)
      KW(2,4)=FACT*(CDSINH(K*L)-CDSIN(K*L))
      KW(3,1)=KW(1,3)
      KW(3,2)=KW(2,3)
      KW(3,3)=KW(1,1)
      KW(3,4)=-KW(1,2)
      KW(4,1)=KW(1,4)
      KW(4,2)=KW(2,4)
      KW(4,3)=KW(3,4)
      KW(4,4)=KW(2,2)
      XYBENDING=.TRUE.   
          
      END
