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

***********************************************************************
*     This function computes the hyperbolic cosine of a COMPLEX*16     * 
*     number                                                           *
*                                                                      *
*     Input Args :                                                     * 
*          Z : The COMPLEX*16 number                                   *
************************************************************************       
      FUNCTION CDCOSH(Z)
      IMPLICIT NONE
      COMPLEX*16 CDCOSH
      COMPLEX*16 Z
      
      CDCOSH=(CDEXP(Z)+CDEXP(-Z))/2
      
      END
  
************************************************************************
*     This function computes the hyperbolic sine of a COMPLEX*16       * 
*     number                                                           *
*                                                                      *
*     Input Args :                                                     * 
*          Z : The COMPLEX*16 number                                   *
************************************************************************     
      FUNCTION CDSINH(Z)
      IMPLICIT NONE
      COMPLEX*16 CDSINH
      COMPLEX*16 Z
      
      CDSINH=(CDEXP(Z)-CDEXP(-Z))/2
      
      END
      
      
