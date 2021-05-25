************************************************************************
* This file is part of DynamicSiffness, a Fortran library that         * 
* implements the Dynamic Stiffness Method                              *
* Copyright (C) 2021  Tanguy BEVANCON,                                 * 
* Quartz Laboratory - Supmeca                                          *
* 3 rue Ferand Hainaut                                                 *
* 93407 SAINT-OUEN - FRANCE                                            *      
* tanguy.bevancon@edu.supmeca.fr                                       *
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
*     This function computes the Timoshenko XY-bending components of the*
*     dynamic stiffness matrix for a straight beam in a local basis.   *
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
*		   Nu : Poisson's ratio                                        *
*          kY : Timoshenko's Section Reduction
*                                                                      *
*     Output Args :                                                    *
*            KW : Computed dynamic stiffness matrix                    *
*                                                                      *      
*     Return value :                                                   *
*            unused logical error flag                                 *
************************************************************************

************************************************************************
*     Update for Timoshenko's theory                                   *
* 04/2021 by Tanguy BEVANCON                                           *
*tanguy.bevancon@supmeca.fr                                            *
************************************************************************
      
      FUNCTION XYTIMOSHENKOBENDING(W,S,IZ,L,RHO,E,NU,KY,KW)
      IMPLICIT NONE
      LOGICAL XYTIMOSHENKOBENDING
      
*     Circular frequency                                               *
      DOUBLE PRECISION W
      
*     Geometrical properties of the beam                               *      
      DOUBLE PRECISION S,IZ,L
      
*     Timoshenko property
      DOUBLE PRECISION KY
      
*     Material properties of the beam                                  *
      DOUBLE PRECISION RHO,NU
      COMPLEX*16 E

*     Intermediate matrices                                            *
      COMPLEX*16 KA(4,4),KB(4,4)
      
*     paramaters of zgetri                                             *
      COMPLEX*16 WORK(4)
      INTEGER IPIV(4)
      INTEGER INFO

*     XY-Bending Dynamic Stiffness Matrix                              *      
      COMPLEX*16 KW(4,4)

*     Complex*16 hyperbolic functions                                  *
      COMPLEX*16 CDCOSH, CDSINH

*     Local variables
      COMPLEX*16 G,A,B,C,DELTA,BETA,GAMMA,EPSI
      COMPLEX*16 K1,K2,CO,SI,CO2,SI2,COH,SIH,FACT
      Double Precision SIGNE

      G=E/2/(1+NU)
      A=E*IZ
      B=-RHO*IZ*(1+E/KY/G)*W**2
      C=RHO*W**2*(RHO*IZ*W**2/KY/G-S)
      DELTA=SQRT(B**2-4*A*C)

* K1, K2 : wave numbers                                                *
      K1=CDSQRT((DELTA-B)/2/E/IZ)
      K2=CDSQRT(CDABS(B+DELTA)/2/E/IZ)
    
      CO=CDCOS(K1*L)
      SI=CDSIN(K1*L)
      CO2=CDCOS(K2*L)
      SI2=CDSIN(K2*L)
      COH=CDCOSH(K2*L)
      SIH=CDSINH(K2*L)
      
      FACT=KY*G*S+E*IZ*RHO*W*W/KY/G
      FACT=FACT/(KY*G*S-RHO*IZ*W*W)
      BETA=E*IZ/(KY*G*S-RHO*IZ*W*W)
      GAMMA=KY*G*S*(1-FACT)
      EPSI=-BETA*KY*G*S
    
      SIGNE=(DELTA+B)/CDABS(DELTA+B)
      
      IF (SIGNE .LT. 0) THEN
      
          KA(1,1)=1
          KA(1,2)=0
          KA(1,3)=1
          KA(1,4)=0
          KA(2,1)=0
          KA(2,2)=K1*(FACT-BETA*K1**2)
          KA(2,3)=0
          KA(2,4)=K2*(FACT-BETA*K2**2)
          KA(3,1)=CO
          KA(3,2)=SI
          KA(3,3)=CO2
          KA(3,4)=SI2
          KA(4,1)=K1*(BETA*K1**2-FACT)*SI
          KA(4,2)=-K1*(BETA*K1**2-FACT)*CO
          KA(4,3)=K2*(BETA*K2**2-FACT)*SI2
          KA(4,4)=-K2*(BETA*K2**2-FACT)*CO2
          
          KB(1,1)=0
          KB(1,2)=K1*(EPSI*K1**2-GAMMA)
          KB(1,3)=0
          KB(1,4)=K2*(EPSI*K2**2-GAMMA)
          KB(2,1)=-E*IZ*K1**2*(BETA*K1**2-FACT)
          KB(2,2)=0
          KB(2,3)=-E*IZ*K2**2*(BETA*K2**2-FACT)
          KB(2,4)=0
          KB(3,1)=K1*(EPSI*K1**2-GAMMA)*SI
          KB(3,2)=-K1*(EPSI*K1**2-GAMMA)*CO
          KB(3,3)=K2*(EPSI*K2**2-GAMMA)*SI2
          KB(3,4)=-K2*(EPSI*K2**2-GAMMA)*CO2
          KB(4,1)=E*IZ*K1**2*(BETA*K1**2-FACT)*CO
          KB(4,2)=E*IZ*K1**2*(BETA*K1**2-FACT)*SI
          KB(4,3)=E*IZ*K2**2*(BETA*K2**2-FACT)*CO2
          KB(4,4)=E*IZ*K2**2*(BETA*K2**2-FACT)*SI2
      ELSE

          KA(1,1)=1
          KA(1,2)=0
          KA(1,3)=1
          KA(1,4)=0
          KA(2,1)=0
          KA(2,2)=K1*(FACT-BETA*K1**2)
          KA(2,3)=0
          KA(2,4)=K2*(FACT+BETA*K2**2)
          KA(3,1)=CO
          KA(3,2)=SI
          KA(3,3)=COH
          KA(3,4)=SIH
          KA(4,1)=K1*(BETA*K1**2-FACT)*SI
          KA(4,2)=-K1*(BETA*K1**2-FACT)*CO
          KA(4,3)=K2*(BETA*K2**2+FACT)*SIH
          KA(4,4)=K2*(BETA*K2**2+FACT)*COH
          
          KB(1,1)=0
          KB(1,2)=K1*(EPSI*K1**2-GAMMA)
          KB(1,3)=0
          KB(1,4)=-K2*(EPSI*K2**2+GAMMA)
          KB(2,1)=-E*IZ*K1**2*(BETA*K1**2-FACT)
          KB(2,2)=0
          KB(2,3)=-E*IZ*K2**2*(BETA*K2**2+FACT)
          KB(2,4)=0
          KB(3,1)=K1*(EPSI*K1**2-GAMMA)*SI
          KB(3,2)=-K1*(EPSI*K1**2-GAMMA)*CO
          KB(3,3)=K2*(EPSI*K2**2+GAMMA)*SIH
          KB(3,4)=K2*(EPSI*K2**2+GAMMA)*COH
          KB(4,1)=E*IZ*K1**2*(BETA*K1**2-FACT)*CO
          KB(4,2)=E*IZ*K1**2*(BETA*K1**2-FACT)*SI
          KB(4,3)=E*IZ*K2**2*(BETA*K2**2+FACT)*COH
          KB(4,4)=E*IZ*K2**2*(BETA*K2**2+FACT)*SIH
      END IF
      
      call zgetrf(4,4,KA,4,IPIV,INFO)
      call zgetri(4,KA,4,IPIV,WORK,4,INFO)
      KW=matmul(KB,KA)
      
      
      XYTIMOSHENKOBENDING=.TRUE.   
          
      END
