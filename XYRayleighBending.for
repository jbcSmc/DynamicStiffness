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
*     This function computes the Rayleigh XY-bending components of the *
*      dynamic stiffness matrix for a straight beam in a local basis.  *
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
      FUNCTION XYRAYLEIGHBENDING(W,S,IZ,L,RHO,E,KW)
      IMPLICIT NONE
      LOGICAL XYRAYLEIGHBENDING
      
*     Circular frequency                                               *
      DOUBLE PRECISION W
      
*     Geometrical properties of the beam                               *      
      DOUBLE PRECISION S,IZ,L
      
*     Material properties of the beam                                  *
      DOUBLE PRECISION RHO
      COMPLEX*16 E
      
*     XY-Rayleigh-Bending Dynamic Stiffness Matrix                     *      
      COMPLEX*16 KW(4,4)

*     Complex*16 hyperbolic functions                                  *
      COMPLEX*16 CDCOSH, CDSINH

*     Local variables                                                  *      
      COMPLEX*16 K,K1,K2,CO,SI,COH,SIH,FACT

* K1, K2 : wave numbers                                                *
      K=W*SQRT((RHO*W/E)**2+4*RHO*S/E/IZ)/2
      K1=CDSQRT(K+RHO*W*W/2/E)
      K2=CDSQRT(K-RHO*W*W/2/E)
    
      CO=CDCOS(K1*L)
      SI=CDSIN(K1*L)
      COH=CDCOSH(K2*L)
      SIH=CDSINH(K2*L)
      
      FACT=2*K1*K2*(1-CO*COH)+(K2**2-K1**2)*SI*SIH
      FACT=E*IZ/FACT
      
      KW(1,1)=K1*K2*(K1*K1+K2*K2)*(K2*CO*SIH+K1*SI*COH)*FACT
      KW(1,2)=(2*K1*K1*K2*K2*SI*SIH-K1*K2*(K2*K2-K1*K1)*(1-CO*COH))*FACT
      KW(1,3)=-K1*K2*(K1*K1+K2*K2)*(K1*SI+K2*SIH)*FACT
      KW(1,4)=K1*K2*(K1*K1+K2*K2)*(COH-CO)*FACT
      KW(2,1)=KW(1,2)
      KW(2,2)=(K1*K1+K2*K2)*(K2*SI*COH-K1*CO*SIH)*FACT
      KW(2,3)=K1*K2*(K1*K1+K2*K2)*(CO-COH)*FACT
      KW(2,4)=(K1*K2*(K2*SIH-K1*SI)+(K1**3*SIH-K2**3*SI))*FACT
      KW(3,1)=KW(1,3)
      KW(3,2)=KW(2,3)
      KW(3,3)=KW(1,1)
      KW(3,4)=-KW(1,2)
      KW(4,1)=KW(1,4)
      KW(4,2)=KW(2,4)
      KW(4,3)=KW(3,4)
      KW(4,4)=KW(2,2)
      
      XYRAYLEIGHBENDING=.TRUE.   
          
      END