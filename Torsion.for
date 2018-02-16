************************************************************************
* This file is part of DynamicSiffness, a Fortran library that         * 
* implements the Dynamic Stiffness Method                              *
* Copyright (C) 2018  Jean-Baptiste CASIMIR,                           * 
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
*     This function computes the torsion components of the dynamic     *
*     stiffness matrix for a straight beam in a local basis.           *
*                                                                      *
*     This matrix relates traction displacement vector T and           *
*     external force vector M at the tips of the AB beam for a given   *
*     frequency w according to KW . T = M where T = (TA, TB)^T and     *
*     M = (MA, MB)^T                                                   *
*     UA is the longitudinal rotation of tip A                         *
*     UB is the longitudinal rotation of tip B                         *
*     MA is the longitudinal external moment applied on tip A          *
*     MB is the longitudinal external moment applied on tip B          *     
*                                                                      *
*     Input Args :                                                     * 
*          W : circular frequency                                      *
*          I0 : polar inertia of the section                           *
*          J : St-Venant's torsion modulus of the section              *       
*          L : length of the beam                                      *      
*          RHO : mass density                                          *
*          G : complex Coulomb's modulus including structural damping  *
*                                                                      *
*     Output Args :                                                    *
*            KW : Computed dynamic stiffness matrix                    *
*                                                                      *      
*     Return value :                                                   *
*            unused logical error flag                                 *
************************************************************************      
      FUNCTION TORSION(W,I0,J,L,RHO,G,KW)
      IMPLICIT NONE
      LOGICAL TORSION
      
*     Circular frequency                                               *
      DOUBLE PRECISION W
      
*     Geometrical properties of the beam                               *      
      DOUBLE PRECISION I0,J,L
      
*     Material properties of the beam                                  *
      DOUBLE PRECISION RHO
      COMPLEX*16 G

*     Torsion Dynamic Stiffness Matrix                                 *      
      COMPLEX*16 KW(2,2)

*     Wave number                                                      *      
      COMPLEX*16 K
      
      K=W*CDSQRT(RHO*I0/G/J)
      
      KW(1,2)=-G*J*K/L/CDSIN(K*L);
      KW(2,1)=KW(1,2)
      KW(1,1)=-KW(1,2)*CDCOS(K*L)
      KW(2,2)=KW(1,1)
      
      TORSION=.TRUE.   
      
      
      END