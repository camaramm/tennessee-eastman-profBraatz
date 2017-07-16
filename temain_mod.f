C
C               Tennessee Eastman Process Control Test Problem
C
C			Original codes written by
C
C                    James J. Downs and Ernest F. Vogel
C
C                  Process and Control Systems Engineering
C                        Tennessee Eastman Company
C                              P.O. Box 511
C                          Kingsport, Tennessee 37662
C
C--------------------------------------------------------------------
C
C  New version is a closed-loop plant-wide control scheme for 
C  the Tennessee Eastman Process Control Test Problem
C				
C  The modifications are by:
C
C	      Evan L. Russell, Leo H. Chiang and Richard D. Braatz
C
C		     Large Scale Systems Research Laboratory
C	                Department of Chemical Engineering
C		    University of Illinois at Urbana-Champaign
C	                 600 South Mathews Avenue, Box C-3
C			      Urbana, Illinois 61801
C		 	    http://brahms.scs.uiuc.edu
C
C The modified text is Copyright 1998-2002 by The Board of Trustees 
C of the University of Illinois.  All rights reserved.
C 
C Permission hereby granted, free of charge, to any person obtaining a copy
C of this software and associated documentation files (the "Software"), to
C deal with the Software without restriction, including without limitation
C the rights to use, copy, modify, merge, publish, distribute, sublicense,
C and/or sell copies of the Software, and to permit persons to whom the 
C Software is furnished to do so, subject to the following conditions:
C 		1. Redistributions of source code must retain the above copyright
C		   notice, this list of conditions and the following disclaimers.
C		2. Redistributions in binary form must reproduce the above 
C		   copyright notice, this list of conditions and the following 
C		   disclaimers in the documentation and/or other materials 
C		   provided with the distribution.
C		3. Neither the names of Large Scale Research Systems Laboratory,
C		   University of Illinois, nor the names of its contributors may
C		   be used to endorse or promote products derived from this 
C		   Software without specific prior written permission.
C
C THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
C OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
C FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
C THE CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
C OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
C ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
C DEALINGS IN THE SOFTWARE.
C----------------------------------------------------------------------
C
C  Users should cite the original code using the following references:
C
C    J.J. Downs and E.F. Vogel, "A plant-wide industrial process control 
C    problem." Presented at the AIChE 1990 Annual Meeting, Session on
C    Industrial Challenge Problems in Process Control, Paper #24a
C    Chicago, Illinois, November 14, 1990.
C
C    J.J. Downs and E.F. Vogel, "A plant-wide industrial process control 
C    problem," Computers and Chemical Engineering, 17:245-255 (1993).
C  
C  Users should cite the modified code using the following references:
C
C    E.L. Russell, L.H. Chiang, and R.D. Braatz. Data-driven Techniques 
C    for Fault Detection and Diagnosis in Chemical Processes, Springer-Verlag, 
C    London, 2000. 
C
C    L.H. Chiang, E.L. Russell, and R.D. Braatz. Fault Detection and 
C    Diagnosis in Industrial Systems, Springer-Verlag, London, 2001.  
C
C    L.H. Chiang, E.L. Russell, and R.D. Braatz. "Fault diagnosis in 
C    chemical processes using Fisher discriminant analysis, discriminant 
C    partial least squares, and principal component analysis," Chemometrics 
C    and Intelligent Laboratory Systems, 50:243-252, 2000. 
C
C    E.L. Russell, L.H. Chiang, and R.D. Braatz. "Fault detection in 
C    industrial processes using canonical variate analysis and dynamic 
C    principal component analysis," Chemometrics and Intelligent Laboratory 
C    Systems, 51:81-93, 2000. 
C
C
C  Main program for demonstrating application of the modified Tennessee Eastman
C  Process Control Test Problem
C
C  
C  Instructions for running the program
C  ====================================
C
C  1) Go to line 220, change NPTS to the number of data points to simulate. For each
C     minute of operation, 60 points are generated.
C
C  2) Go to line 226, change SSPTS to the number of data points to simulate in steady
C     state operation before implementing the disturbance.
C
C  3) Go to line 367, implement any of the 21 programmed disturbances. For example, to
C     implement disturbance 2, type IDV(2)=1 .
C
C  4) The program will generate 15 output files and all data are recorded every
C     180 seconds, see Table 1 for details.  The default path is the home directory.
C     To change the file name and path, modify lines 346-360 accordingly.  
C     To overwrite the files that already existed, change STATUS='new' to 
C     STATUS='old' from lines 346-360.
C 	     
C
C    		 Table 1: Content of the output files
C
C      File Name		        	Content
C      ---------			        -------
C    TE_data_inc.dat	   		  Time (in seconds) 
C    TE_data_mv1.dat	   Measurements for manipulated variables 1 to 4
C    TE_data_mv2.dat	   Measurements for manipulated variables 5 to 8
C    TE_data_mv3.dat	   Measurements for manipulated variables 9 to 12
C    TE_data_me01.dat	   Measurements for measurement variables 1 to 4
C    TE_data_me02.dat	   Measurements for measurement variables 5 to 8
C    TE_data_me03.dat	   Measurements for measurement variables 9 to 12
C    TE_data_me04.dat	   Measurements for measurement variables 13 to 16
C    TE_data_me05.dat	   Measurements for measurement variables 17 to 20
C    TE_data_me06.dat	   Measurements for measurement variables 21 to 24
C    TE_data_me07.dat	   Measurements for measurement variables 25 to 28
C    TE_data_me08.dat	   Measurements for measurement variables 29 to 32
C    TE_data_me09.dat	   Measurements for measurement variables 33 to 36
C    TE_data_me10.dat	   Measurements for measurement variables 37 to 40
C    TE_data_me11.dat	   Measurements for measurement variable 41 
C
C  5) To ensure the randomness of the measurement noises, the random number
C     G in the sub program (teprob.f, line 1187) has to be changed each time before
C     running 'temain_mod.f'.
C
C  6) Save the changes in 'temain_mod.f' and 'teprob.f' and compile the program in
C      unix by typing
C      f77 temain_mod.f teprob.f
C
C  7) Run the program by typing 
C      a.out
C
C
C=============================================================================
C
C
C  MEASUREMENT AND VALVE COMMON BLOCK
C
      DOUBLE PRECISION XMEAS, XMV
      COMMON/PV/ XMEAS(41), XMV(12)
C
C   DISTURBANCE VECTOR COMMON BLOCK
C
      INTEGER IDV
      COMMON/DVEC/ IDV(20)
C
C   CONTROLLER COMMON BLOCK
C
C      DOUBLE PRECISION SETPT, GAIN, TAUI, ERROLD, DELTAT
C      COMMON/CTRL/ SETPT, GAIN, TAUI, ERROLD, DELTAT
       DOUBLE PRECISION SETPT, DELTAT
       COMMON/CTRLALL/ SETPT(20), DELTAT
       INTEGER FLAG
       COMMON/FLAG6/ FLAG
C
      DOUBLE PRECISION GAIN1, ERROLD1
      COMMON/CTRL1/ GAIN1, ERROLD1
      DOUBLE PRECISION GAIN2, ERROLD2
      COMMON/CTRL2/ GAIN2, ERROLD2
      DOUBLE PRECISION GAIN3, ERROLD3
      COMMON/CTRL3/ GAIN3, ERROLD3
      DOUBLE PRECISION  GAIN4, ERROLD4
      COMMON/CTRL4/ GAIN4, ERROLD4
      DOUBLE PRECISION GAIN5, TAUI5, ERROLD5
      COMMON/CTRL5/ GAIN5, TAUI5, ERROLD5
      DOUBLE PRECISION GAIN6, ERROLD6
      COMMON/CTRL6/ GAIN6, ERROLD6
      DOUBLE PRECISION GAIN7, ERROLD7
      COMMON/CTRL7/  GAIN7, ERROLD7
      DOUBLE PRECISION GAIN8, ERROLD8
      COMMON/CTRL8/ GAIN8, ERROLD8
      DOUBLE PRECISION GAIN9, ERROLD9
      COMMON/CTRL9/ GAIN9, ERROLD9
      DOUBLE PRECISION GAIN10, TAUI10, ERROLD10
      COMMON/CTRL10/ GAIN10, TAUI10, ERROLD10
      DOUBLE PRECISION GAIN11, TAUI11, ERROLD11
      COMMON/CTRL11/ GAIN11, TAUI11, ERROLD11
      DOUBLE PRECISION GAIN13, TAUI13, ERROLD13
      COMMON/CTRL13/ GAIN13, TAUI13, ERROLD13
      DOUBLE PRECISION GAIN14, TAUI14, ERROLD14
      COMMON/CTRL14/ GAIN14, TAUI14, ERROLD14
      DOUBLE PRECISION GAIN15, TAUI15, ERROLD15
      COMMON/CTRL15/ GAIN15, TAUI15, ERROLD15
      DOUBLE PRECISION GAIN16, TAUI16, ERROLD16
      COMMON/CTRL16/ GAIN16, TAUI16, ERROLD16
      DOUBLE PRECISION GAIN17, TAUI17, ERROLD17
      COMMON/CTRL17/ GAIN17, TAUI17, ERROLD17
      DOUBLE PRECISION GAIN18, TAUI18, ERROLD18
      COMMON/CTRL18/ GAIN18, TAUI18, ERROLD18
      DOUBLE PRECISION GAIN19, TAUI19, ERROLD19
      COMMON/CTRL19/ GAIN19, TAUI19, ERROLD19
      DOUBLE PRECISION GAIN20, TAUI20, ERROLD20
      COMMON/CTRL20/ GAIN20, TAUI20, ERROLD20
      DOUBLE PRECISION GAIN22, TAUI22, ERROLD22
      COMMON/CTRL22/ GAIN22, TAUI22, ERROLD22
C
C  Local Variables
C
      INTEGER I, NN, NPTS, TEST, TEST1, TEST3, TEST4
C
      DOUBLE PRECISION TIME, YY(50), YP(50)
C
C  Set the number of differential equations (states).  The process has 50
C  states.  If the user wishes to integrate additional states, NN must be
C  increased by the number of additional differential equations.
C
      NN = 50
C
C  Set the number of points to simulate
C
      NPTS = 172800

C
C  Set the number of pints to simulate in steady state operation
C

      SSPTS = 3600 * 8


C
C  Integrator Step Size:  1 Second Converted to Hours
C
      DELTAT = 1. / 3600.
C
C  Initialize Process
C  (Sets TIME to zero)
C
      CALL TEINIT(NN,TIME,YY,YP)
C
C  Set Controller Parameters
C  Make a Stripper Level Set Point Change of +15%
C
CC      SETPT = XMEAS(15) + 15.0
CC      GAIN = 2.0
CC      TAUI = 5.0
CC      ERROLD = 0.0
      SETPT(1)=3664.0        
      GAIN1=1.0
      ERROLD1=0.0
      SETPT(2)=4509.3
      GAIN2=1.0
      ERROLD2=0.0
      SETPT(3)=.25052
      GAIN3=1.
      ERROLD3=0.0
      SETPT(4)=9.3477
      GAIN4=1.
      ERROLD4=0.0
      SETPT(5)=26.902
      GAIN5=-0.083 	   
      TAUI5=1./3600.   
      ERROLD5=0.0
      SETPT(6)=0.33712  
      GAIN6=1.22        	       
      ERROLD6=0.0
      SETPT(7)=50.0
      GAIN7=-2.06      
      ERROLD7=0.0
      SETPT(8)=50.0
      GAIN8=-1.62      
      ERROLD8=0.0
      SETPT(9)=230.31
      GAIN9=0.41          
      ERROLD9=0.0	
      SETPT(10)=94.599
      GAIN10= -0.156     * 10.
      TAUI10=1452./3600. 
      ERROLD10=0.0
      SETPT(11)=22.949    
      GAIN11=1.09	  
      TAUI11=2600./3600.
      ERROLD11=0.0
      SETPT(13)=32.188
      GAIN13=18.              
      TAUI13=3168./3600.   
      ERROLD13=0.0
      SETPT(14)=6.8820
      GAIN14=8.3	  
      TAUI14=3168.0/3600.
      ERROLD14=0.0
      SETPT(15)=18.776 	  		
      GAIN15=2.37	  	
      TAUI15=5069./3600.    
      ERROLD15=0.0
      SETPT(16)=65.731
      GAIN16=1.69	  / 10.
      TAUI16=236./3600.
      ERROLD16=0.0
      SETPT(17)=75.000
      GAIN17=11.1	/ 10.
      TAUI17=3168./3600.  
      ERROLD17=0.0	  
      SETPT(18)=120.40
      GAIN18=2.83	* 10.
      TAUI18=982./3600.
      ERROLD18=0.0
      SETPT(19)=13.823
      GAIN19=-83.2	  / 5. /3.  
      TAUI19=6336./3600. 
      ERROLD19=0.0
      SETPT(20)=0.83570  
      GAIN20=-16.3	 / 5.	   
      TAUI20=12408./3600.  
      ERROLD20=0.0
      SETPT(12)=2633.7
      GAIN22=-1.0	  * 5.	   
      TAUI22=1000./3600.  
      ERROLD22=0.0
C
C    Example Disturbance:
C    Change Reactor Cooling
C
 	XMV(1) = 63.053 + 0.
	XMV(2) = 53.980 + 0.
	XMV(3) = 24.644 + 0.    
	XMV(4) = 61.302 + 0.
	XMV(5) = 22.210 + 0.
	XMV(6) = 40.064 + 0.
	XMV(7) = 38.100 + 0.
	XMV(8) = 46.534 + 0.
	XMV(9) = 47.446 + 0.
	XMV(10)= 41.106 + 0.
	XMV(11)= 18.114 + 0.
C
C	SETPT(6)=SETPT(6) + 0.2
C
C  Set all Disturbance Flags to OFF
C
      DO 100 I = 1, 20
          IDV(I) = 0
 100  CONTINUE
C      IDV(20)=1	
C
C
C
C
	OPEN(UNIT=111,FILE='~/TE_data_inc.dat',STATUS='new')
	OPEN(UNIT=1111,FILE='~/TE_data_mv1.dat',STATUS='new')
	OPEN(UNIT=1112,FILE='~/TE_data_mv2.dat',STATUS='new')
	OPEN(UNIT=1113,FILE='~/TE_data_mv3.dat',STATUS='new')
	OPEN(UNIT=2111,FILE='~/TE_data_me01.dat',STATUS='new')
	OPEN(UNIT=2112,FILE='~/TE_data_me02.dat',STATUS='new')
	OPEN(UNIT=2113,FILE='~/TE_data_me03.dat',STATUS='new')
	OPEN(UNIT=2114,FILE='~/TE_data_me04.dat',STATUS='new')
	OPEN(UNIT=2115,FILE='~/TE_data_me05.dat',STATUS='new')
	OPEN(UNIT=2116,FILE='~/TE_data_me06.dat',STATUS='new')
	OPEN(UNIT=2117,FILE='~/TE_data_me07.dat',STATUS='new')
	OPEN(UNIT=2118,FILE='~/TE_data_me08.dat',STATUS='new')
	OPEN(UNIT=2119,FILE='~/TE_data_me09.dat',STATUS='new')
	OPEN(UNIT=2120,FILE='~/TE_data_me10.dat',STATUS='new')
	OPEN(UNIT=2121,FILE='~/TE_data_me11.dat',STATUS='new')
C
C
C  Simulation Loop
C
        DO 1000 I = 1, NPTS
         IF (I.GE.SSPTS) THEN
                 IDV(12)=1
          ENDIF
	  TEST=MOD(I,3)
	  IF (TEST.EQ.0) THEN
		CALL CONTRL1
	  	CALL CONTRL2
	  	CALL CONTRL3
	  	CALL CONTRL4
	  	CALL CONTRL5
	  	CALL CONTRL6
	  	CALL CONTRL7
	  	CALL CONTRL8
	  	CALL CONTRL9
	  	CALL CONTRL10
	  	CALL CONTRL11
	  	CALL CONTRL16
	  	CALL CONTRL17
	  	CALL CONTRL18
	  ENDIF
          TEST1=MOD(I,360)
	  IF (TEST1.EQ.0) THEN
	  	CALL CONTRL13
	  	CALL CONTRL14
	  	CALL CONTRL15
	  	CALL CONTRL19
	  ENDIF
	  TEST1=MOD(I,900)
	  IF (TEST1.EQ.0) CALL CONTRL20
	  TEST3=MOD(I,5000)       
	  IF (TEST3.EQ.0) THEN
		PRINT *, 'Simulation time (in seconds) = ', I
	
	ENDIF
C
 	TEST4=MOD(I,180)	
	IF (TEST4.EQ.0) THEN
		CALL OUTPUT
      		WRITE(111,111) I
 111  		FORMAT(1X,I6)
     	ENDIF
C
	  CALL INTGTR(NN,TIME,DELTAT,YY,YP)
C
 	  CALL CONSHAND
C
 1000 CONTINUE
		PRINT *, 'Simulation is done. '

C
 	CLOSE(UNIT=111)
	CLOSE(UNIT=1111)
	CLOSE(UNIT=1112)
	CLOSE(UNIT=1113)
	CLOSE(UNIT=2111)
	CLOSE(UNIT=2112)
	CLOSE(UNIT=2113)
	CLOSE(UNIT=2114)
	CLOSE(UNIT=2115)
	CLOSE(UNIT=2116)
	CLOSE(UNIT=2117)
	CLOSE(UNIT=2118)
	CLOSE(UNIT=2119)
	CLOSE(UNIT=2120)
  	CLOSE(UNIT=2121)
      STOP


      END
C
C=============================================================================
C
CC      SUBROUTINE CONTRL
C
C  Discrete control algorithms
C
C
C   MEASUREMENT AND VALVE COMMON BLOCK
C
CC      DOUBLE PRECISION XMEAS, XMV
CC      COMMON/PV/ XMEAS(41), XMV(12)
C
C   CONTROLLER COMMON BLOCK
C
CC      DOUBLE PRECISION SETPT, GAIN, TAUI, ERROLD, DELTAT
CC      COMMON/CTRL/ SETPT, GAIN, TAUI, ERROLD, DELTAT
C
CC      DOUBLE PRECISION ERR, DXMV
C
C  Example PI Controller:
C    Stripper Level Controller
C
C    Calculate Error
C
CC      ERR = SETPT - XMEAS(15)
C
C    Proportional-Integral Controller (Velocity Form)
C         GAIN = Controller Gain
C         TAUI = Reset Time (min)
C
CC      DXMV = GAIN * ( ( ERR - ERROLD ) + ERR * DELTAT * 60. / TAUI )
C
CC      XMV(8) = XMV(8) - DXMV
C
CC      ERROLD = ERR
C
CC      RETURN
CC      END
C
C=============================================================================
C
      SUBROUTINE CONTRL1
C
C  Discrete control algorithms
C
C
C   MEASUREMENT AND VALVE COMMON BLOCK
C
      DOUBLE PRECISION XMEAS, XMV
      COMMON/PV/ XMEAS(41), XMV(12)
C
C   CONTROLLER COMMON BLOCK
C
      DOUBLE PRECISION SETPT, DELTAT
      COMMON/CTRLALL/ SETPT(20), DELTAT
      DOUBLE PRECISION GAIN1, ERROLD1
      COMMON/CTRL1/ GAIN1, ERROLD1
C
      DOUBLE PRECISION ERR1, DXMV
C
C  Example PI Controller:
C    Stripper Level Controller
C
C    Calculate Error
C
      ERR1 = (SETPT(1) - XMEAS(2)) * 100. / 5811.
C
C    Proportional-Integral Controller (Velocity Form)
C         GAIN = Controller Gain
C         TAUI = Reset Time (min)
C
      DXMV = GAIN1 * ( ( ERR1 - ERROLD1 ) )
C
      XMV(1) = XMV(1) + DXMV
C
      ERROLD1 = ERR1
C
      RETURN
      END
C
C=============================================================================
C
      SUBROUTINE CONTRL2
C
C  Discrete control algorithms
C
C
C   MEASUREMENT AND VALVE COMMON BLOCK
C
      DOUBLE PRECISION XMEAS, XMV
      COMMON/PV/ XMEAS(41), XMV(12)
C
C   CONTROLLER COMMON BLOCK
C
      DOUBLE PRECISION SETPT, DELTAT
      COMMON/CTRLALL/ SETPT(20), DELTAT
      DOUBLE PRECISION GAIN2, ERROLD2
      COMMON/CTRL2/ GAIN2, ERROLD2
C
      DOUBLE PRECISION ERR2, DXMV
C
C  Example PI Controller:
C    Stripper Level Controller
C
C    Calculate Error
C
      ERR2 = (SETPT(2) - XMEAS(3)) * 100. / 8354. 
C
C    Proportional-Integral Controller (Velocity Form)
C         GAIN = Controller Gain
C         TAUI = Reset Time (min)
C
      DXMV = GAIN2 * ( ( ERR2 - ERROLD2 ) )
C
      XMV(2) = XMV(2) + DXMV
C
      ERROLD2 = ERR2
C
      RETURN
      END
C
C=============================================================================
C
      SUBROUTINE CONTRL3
C
C  Discrete control algorithms
C
C
C   MEASUREMENT AND VALVE COMMON BLOCK
C
      DOUBLE PRECISION XMEAS, XMV
      COMMON/PV/ XMEAS(41), XMV(12)
C
C   CONTROLLER COMMON BLOCK
C
      DOUBLE PRECISION SETPT, DELTAT
      COMMON/CTRLALL/ SETPT(20), DELTAT
      DOUBLE PRECISION GAIN3, ERROLD3
      COMMON/CTRL3/ GAIN3, ERROLD3
C
      DOUBLE PRECISION ERR3, DXMV
C
C  Example PI Controller:
C    Stripper Level Controller
C
C    Calculate Error
C
      ERR3 = (SETPT(3) - XMEAS(1)) * 100. / 1.017
C
C    Proportional-Integral Controller (Velocity Form)
C         GAIN = Controller Gain
C         TAUI = Reset Time (min)
C
      DXMV = GAIN3 * ( ( ERR3 - ERROLD3 ) )
C
      XMV(3) = XMV(3) + DXMV
C
      ERROLD3 = ERR3
C
      RETURN
      END
C
C=============================================================================
C
      SUBROUTINE CONTRL4
C
C  Discrete control algorithms
C
C
C   MEASUREMENT AND VALVE COMMON BLOCK
C
      DOUBLE PRECISION XMEAS, XMV
      COMMON/PV/ XMEAS(41), XMV(12)
C
C   CONTROLLER COMMON BLOCK
C
      DOUBLE PRECISION SETPT, DELTAT
      COMMON/CTRLALL/ SETPT(20), DELTAT
      DOUBLE PRECISION GAIN4, ERROLD4
      COMMON/CTRL4/ GAIN4, ERROLD4
C
      DOUBLE PRECISION ERR4, DXMV
C
C  Example PI Controller:
C    Stripper Level Controller
C
C    Calculate Error
C
      ERR4 = (SETPT(4) - XMEAS(4)) * 100. / 15.25
C
C    Proportional-Integral Controller (Velocity Form)
C         GAIN = Controller Gain
C         TAUI = Reset Time (min)
C
      DXMV = GAIN4 * ( ( ERR4 - ERROLD4 ) )
C
      XMV(4) = XMV(4) + DXMV
C
      ERROLD4 = ERR4
C
      RETURN
      END
C
C=============================================================================
C
      SUBROUTINE CONTRL5
C
C  Discrete control algorithms
C
C
C   MEASUREMENT AND VALVE COMMON BLOCK
C
      DOUBLE PRECISION XMEAS, XMV
      COMMON/PV/ XMEAS(41), XMV(12)
C
C   CONTROLLER COMMON BLOCK
C
      DOUBLE PRECISION SETPT, DELTAT
      COMMON/CTRLALL/ SETPT(20), DELTAT
      DOUBLE PRECISION GAIN5, TAUI5, ERROLD5
      COMMON/CTRL5/ GAIN5, TAUI5, ERROLD5
C
      DOUBLE PRECISION ERR5, DXMV
C
C  Example PI Controller:
C    Stripper Level Controller
C
C    Calculate Error
C
      ERR5 = (SETPT(5) - XMEAS(5))  * 100. / 53.
C
C    Proportional-Integral Controller (Velocity Form)
C         GAIN = Controller Gain
C         TAUI = Reset Time (min)
C
C 	PRINT *, 'GAIN5= ', GAIN5
C	PRINT *, 'TAUI5= ', TAUI5
C	PRINT *, 'ERR5= ', ERR5
C	PRINT *, 'ERROLD5= ', ERROLD5     
C
	DXMV = GAIN5 * ((ERR5 - ERROLD5)+ERR5*DELTAT*3./TAUI5)
C
      XMV(5) = XMV(5) + DXMV
C
      ERROLD5 = ERR5
C
      RETURN
      END
C
C=============================================================================
C
      SUBROUTINE CONTRL6
C
C  Discrete control algorithms
C
C
C   MEASUREMENT AND VALVE COMMON BLOCK
C
      DOUBLE PRECISION XMEAS, XMV
      COMMON/PV/ XMEAS(41), XMV(12)
	INTEGER FLAG
       COMMON/FLAG6/ FLAG
C
C   CONTROLLER COMMON BLOCK
C
      DOUBLE PRECISION SETPT, DELTAT
      COMMON/CTRLALL/ SETPT(20), DELTAT
      DOUBLE PRECISION GAIN6, ERROLD6
      COMMON/CTRL6/ GAIN6, ERROLD6
C
      DOUBLE PRECISION ERR6, DXMV
C
C  Example PI Controller:
C     Stripper Level Controller
	IF (XMEAS(13).GE.2950.0) THEN
		XMV(6)=100.0
		FLAG=1
	ELSEIF (FLAG.EQ.1.AND.XMEAS(13).GE.2633.7) THEN
		XMV(6)=100.0
	ELSEIF (FLAG.EQ.1.AND.XMEAS(13).LE.2633.7) THEN
		XMV(6)=40.060
		SETPT(6)=0.33712
		ERROLD6=0.0
 		FLAG=0
	ELSEIF (XMEAS(13).LE.2300.) THEN
		XMV(6)=0.0
		FLAG=2
	ELSEIF (FLAG.EQ.2.AND.XMEAS(13).LE.2633.7) THEN
		XMV(6)=0.0
	ELSEIF (FLAG.EQ.2.AND.XMEAS(13).GE.2633.7) THEN
		XMV(6)=40.060
		SETPT(6)=0.33712
		ERROLD6=0.0
		FLAG=0
	ELSE	
		FLAG=0
C
C    Calculate Error
C
       ERR6 = (SETPT(6) - XMEAS(10)) * 100. /1.
C
C    Proportional-Integral Controller (Velocity Form)
C         GAIN = Controller Gain
C         TAUI = Reset Time (min)
C
C	PRINT *, 'XMV(6)= ', XMV(6)
      DXMV = GAIN6 * ( ( ERR6 - ERROLD6 ) )
C
C 	PRINT *, 'GAIN6= ', GAIN6
C	PRINT *, 'SETPT(6)= ', SETPT(6)	
C	PRINT *, 'XMEAS(10)= ', XMEAS(10)     
	XMV(6) = XMV(6) + DXMV
C
C 	PRINT *, 'ERROLD6= ', ERROLD6     
C	PRINT *, 'ERR6= ', ERR6
C	PRINT *, 'XMV(6)== ', XMV(6)
      ERROLD6 = ERR6
      ENDIF
C
      RETURN
      END
C
C=============================================================================
C
      SUBROUTINE CONTRL7
C
C  Discrete control algorithms
C
C
C   MEASUREMENT AND VALVE COMMON BLOCK
C
      DOUBLE PRECISION XMEAS, XMV
      COMMON/PV/ XMEAS(41), XMV(12)
C
C   CONTROLLER COMMON BLOCK
C
      DOUBLE PRECISION SETPT, DELTAT
      COMMON/CTRLALL/ SETPT(20), DELTAT
      DOUBLE PRECISION GAIN7, ERROLD7
      COMMON/CTRL7/ GAIN7, ERROLD7
C
      DOUBLE PRECISION ERR7, DXMV
C
C  Example PI Controller:
C    Stripper Level Controller
C
C    Calculate Error
C
      ERR7 = (SETPT(7) - XMEAS(12)) * 100. / 70.
C
C    Proportional-Integral Controller (Velocity Form)
C         GAIN = Controller Gain
C         TAUI = Reset Time (min)
C
      DXMV = GAIN7 * ( ( ERR7 - ERROLD7 ) )
C
      XMV(7) = XMV(7) + DXMV
C
      ERROLD7 = ERR7
C
      RETURN
      END
C
C=============================================================================
C
      SUBROUTINE CONTRL8
C
C  Discrete control algorithms
C
C
C   MEASUREMENT AND VALVE COMMON BLOCK
C
      DOUBLE PRECISION XMEAS, XMV
      COMMON/PV/ XMEAS(41), XMV(12)
C
C   CONTROLLER COMMON BLOCK
C
      DOUBLE PRECISION SETPT, DELTAT
      COMMON/CTRLALL/ SETPT(20), DELTAT
      DOUBLE PRECISION GAIN8, ERROLD8
      COMMON/CTRL8/ GAIN8, ERROLD8
C
      DOUBLE PRECISION ERR8, DXMV
C
C  Example PI Controller:
C    Stripper Level Controller
C
C    Calculate Error
C
      ERR8 = (SETPT(8) - XMEAS(15)) * 100. / 70.
C
C    Proportional-Integral Controller (Velocity Form)
C         GAIN = Controller Gain
C         TAUI = Reset Time (min)
C
      DXMV =  GAIN8 * ( ( ERR8 - ERROLD8 ) )
C
      XMV(8) = XMV(8) + DXMV
C
      ERROLD8 = ERR8
C
      RETURN
      END
C
C=============================================================================
C
      SUBROUTINE CONTRL9
C
C  Discrete control algorithms
C
C
C   MEASUREMENT AND VALVE COMMON BLOCK
C
      DOUBLE PRECISION XMEAS, XMV
      COMMON/PV/ XMEAS(41), XMV(12)
C
C   CONTROLLER COMMON BLOCK
C
      DOUBLE PRECISION SETPT, DELTAT
      COMMON/CTRLALL/ SETPT(20), DELTAT
      DOUBLE PRECISION GAIN9, ERROLD9
      COMMON/CTRL9/ GAIN9, ERROLD9
C
      DOUBLE PRECISION ERR9, DXMV
C
C  Example PI Controller:
C    Stripper Level Controller
C
C    Calculate Error
C
      ERR9 = (SETPT(9) - XMEAS(19)) * 100. / 460. 
C
C    Proportional-Integral Controller (Velocity Form)
C         GAIN = Controller Gain
C         TAUI = Reset Time (min)
C
      DXMV = GAIN9 * ( ( ERR9 - ERROLD9 ) )
C
      XMV(9) = XMV(9) + DXMV
C
      ERROLD9 = ERR9
C
      RETURN
      END
C
C=============================================================================
C
      SUBROUTINE CONTRL10
C
C  Discrete control algorithms
C
C
C   MEASUREMENT AND VALVE COMMON BLOCK
C
      DOUBLE PRECISION XMEAS, XMV
      COMMON/PV/ XMEAS(41), XMV(12)
C
C   CONTROLLER COMMON BLOCK
C
      DOUBLE PRECISION SETPT, DELTAT
      COMMON/CTRLALL/ SETPT(20), DELTAT
      DOUBLE PRECISION GAIN10, TAUI10, ERROLD10
      COMMON/CTRL10/ GAIN10, TAUI10, ERROLD10
C
      DOUBLE PRECISION ERR10, DXMV
C
C  Example PI Controller:
C    Stripper Level Controller
C
C    Calculate Error
C
      ERR10 = (SETPT(10) - XMEAS(21)) * 100. / 150.
C
C    Proportional-Integral Controller (Velocity Form)
C         GAIN = Controller Gain
C         TAUI = Reset Time (min)
C
      DXMV = GAIN10*((ERR10 - ERROLD10)+ERR10*DELTAT*3./TAUI10)
C
      XMV(10) = XMV(10) + DXMV
C
      ERROLD10 = ERR10
C
      RETURN
      END
C
C=============================================================================
C
      SUBROUTINE CONTRL11
C
C  Discrete control algorithms
C
C
C   MEASUREMENT AND VALVE COMMON BLOCK
C
      DOUBLE PRECISION XMEAS, XMV
      COMMON/PV/ XMEAS(41), XMV(12)
C
C   CONTROLLER COMMON BLOCK
C
      DOUBLE PRECISION SETPT, DELTAT
      COMMON/CTRLALL/ SETPT(20), DELTAT
      DOUBLE PRECISION GAIN11, TAUI11, ERROLD11
      COMMON/CTRL11/ GAIN11, TAUI11, ERROLD11
C
      DOUBLE PRECISION ERR11, DXMV
C
C  Example PI Controller:
C    Stripper Level Controller
C
C    Calculate Error
C
      ERR11 = (SETPT(11) - XMEAS(17)) * 100. / 46.
C
C    Proportional-Integral Controller (Velocity Form)
C         GAIN = Controller Gain
C         TAUI = Reset Time (min)
C
      DXMV = GAIN11*((ERR11 - ERROLD11)+ERR11*DELTAT*3./TAUI11)
C
      XMV(11) = XMV(11) + DXMV
C
      ERROLD11 = ERR11
C
      RETURN
      END
C
C=============================================================================
C
      SUBROUTINE CONTRL13
C
C  Discrete control algorithms
C
C
C   MEASUREMENT AND VALVE COMMON BLOCK
C
      DOUBLE PRECISION XMEAS, XMV
      COMMON/PV/ XMEAS(41), XMV(12)
C
C   CONTROLLER COMMON BLOCK
C
      DOUBLE PRECISION SETPT, DELTAT
      COMMON/CTRLALL/ SETPT(20), DELTAT
      DOUBLE PRECISION GAIN13, TAUI13, ERROLD13
      COMMON/CTRL13/ GAIN13, TAUI13, ERROLD13
C
      DOUBLE PRECISION ERR13, DXMV
C
C  Example PI Controller:
C    Stripper Level Controller
C
C    Calculate Error
C
      ERR13 = (SETPT(13) - XMEAS(23)) * 100. / 100.
C
C    Proportional-Integral Controller (Velocity Form)
C         GAIN = Controller Gain
C         TAUI = Reset Time (min)
C
      DXMV = GAIN13 * ((ERR13 - ERROLD13)+ERR13*DELTAT*360./TAUI13)
C
      SETPT(3) = SETPT(3) + DXMV * 1.017 / 100.
C
      ERROLD13 = ERR13
C
      RETURN
      END
C
C=============================================================================
C
      SUBROUTINE CONTRL14
C
C  Discrete control algorithms
C
C
C   MEASUREMENT AND VALVE COMMON BLOCK
C
      DOUBLE PRECISION XMEAS, XMV
      COMMON/PV/ XMEAS(41), XMV(12)
C
C   CONTROLLER COMMON BLOCK
C
      DOUBLE PRECISION SETPT, DELTAT
      COMMON/CTRLALL/ SETPT(20), DELTAT
      DOUBLE PRECISION GAIN14, TAUI14, ERROLD14
      COMMON/CTRL14/ GAIN14, TAUI14, ERROLD14
C
      DOUBLE PRECISION ERR14, DXMV
C
C  Example PI Controller:
C    Stripper Level Controller
C
C    Calculate Error
C
      ERR14 = (SETPT(14) - XMEAS(26)) * 100. /100.
C
C    Proportional-Integral Controller (Velocity Form)
C         GAIN = Controller Gain
C         TAUI = Reset Time (min)
C
      DXMV = GAIN14*((ERR14 - ERROLD14)+ERR14*DELTAT*360./TAUI14)
C
      SETPT(1) = SETPT(1) + DXMV * 5811. / 100.
C
      ERROLD14 = ERR14
C
      RETURN
      END
C
C=============================================================================
C
      SUBROUTINE CONTRL15
C
C  Discrete control algorithms
C
C
C   MEASUREMENT AND VALVE COMMON BLOCK
C
      DOUBLE PRECISION XMEAS, XMV
      COMMON/PV/ XMEAS(41), XMV(12)
C
C   CONTROLLER COMMON BLOCK
C
      DOUBLE PRECISION SETPT, DELTAT
      COMMON/CTRLALL/ SETPT(20), DELTAT
      DOUBLE PRECISION GAIN15, TAUI15, ERROLD15
      COMMON/CTRL15/ GAIN15, TAUI15, ERROLD15
C
      DOUBLE PRECISION ERR15, DXMV
C
C  Example PI Controller:
C    Stripper Level Controller
C
C    Calculate Error
C
      ERR15 = (SETPT(15) - XMEAS(27)) * 100. / 100.
C
C    Proportional-Integral Controller (Velocity Form)
C         GAIN = Controller Gain
C         TAUI = Reset Time (min)
C
      DXMV = GAIN15 * ((ERR15 - ERROLD15)+ERR15*DELTAT*360./TAUI15)
C
      SETPT(2) = SETPT(2) + DXMV * 8354. / 100.
C
      ERROLD15 = ERR15
C
      RETURN
      END
C
C=============================================================================
C
      SUBROUTINE CONTRL16
C
C  Discrete control algorithms
C
C
C   MEASUREMENT AND VALVE COMMON BLOCK
C
      DOUBLE PRECISION XMEAS, XMV
      COMMON/PV/ XMEAS(41), XMV(12)
C
C   CONTROLLER COMMON BLOCK
C
      DOUBLE PRECISION SETPT, DELTAT
      COMMON/CTRLALL/ SETPT(20), DELTAT
      DOUBLE PRECISION GAIN16, TAUI16, ERROLD16
      COMMON/CTRL16/ GAIN16, TAUI16, ERROLD16
C
      DOUBLE PRECISION ERR16, DXMV
C
C  Example PI Controller:
C    Stripper Level Controller
C
C    Calculate Error
C
      ERR16 = (SETPT(16) - XMEAS(18)) * 100. / 130.
C
C    Proportional-Integral Controller (Velocity Form)
C         GAIN = Controller Gain
C         TAUI = Reset Time (min)
C
      DXMV = GAIN16 * ((ERR16 - ERROLD16)+ERR16*DELTAT*3./TAUI16)
C
      SETPT(9) = SETPT(9) + DXMV * 460. / 100.
C
      ERROLD16 = ERR16
C
      RETURN
      END
C
C=============================================================================
C
      SUBROUTINE CONTRL17
C
C  Discrete control algorithms
C
C
C   MEASUREMENT AND VALVE COMMON BLOCK
C
      DOUBLE PRECISION XMEAS, XMV
      COMMON/PV/ XMEAS(41), XMV(12)
C
C   CONTROLLER COMMON BLOCK
C
      DOUBLE PRECISION SETPT, DELTAT
      COMMON/CTRLALL/ SETPT(20), DELTAT
      DOUBLE PRECISION GAIN17, TAUI17, ERROLD17
      COMMON/CTRL17/ GAIN17, TAUI17, ERROLD17
C
      DOUBLE PRECISION ERR17, DXMV
C
C  Example PI Controller:
C    Stripper Level Controller
C
C    Calculate Error
C
      ERR17 = (SETPT(17) - XMEAS(8)) * 100. / 50.
C
C    Proportional-Integral Controller (Velocity Form)
C         GAIN = Controller Gain
C         TAUI = Reset Time (min)
C
      DXMV =GAIN17*((ERR17 - ERROLD17)+ERR17*DELTAT*3./TAUI17)
C
      SETPT(4) = SETPT(4) + DXMV * 15.25 / 100.
C
      ERROLD17 = ERR17
C
      RETURN
      END
C
C=============================================================================
C
      SUBROUTINE CONTRL18
C
C  Discrete control algorithms
C
C
C   MEASUREMENT AND VALVE COMMON BLOCK
C
      DOUBLE PRECISION XMEAS, XMV
      COMMON/PV/ XMEAS(41), XMV(12)
C
C   CONTROLLER COMMON BLOCK
C
      DOUBLE PRECISION SETPT, DELTAT
      COMMON/CTRLALL/ SETPT(20), DELTAT
      DOUBLE PRECISION GAIN18, TAUI18, ERROLD18
      COMMON/CTRL18/ GAIN18, TAUI18, ERROLD18
C
      DOUBLE PRECISION ERR18, DXMV
C
C  Example PI Controller:
C    Stripper Level Controller
C
C    Calculate Error
C
      ERR18 = (SETPT(18) - XMEAS(9)) * 100. / 150. 
C
C    Proportional-Integral Controller (Velocity Form)
C         GAIN = Controller Gain
C         TAUI = Reset Time (min)
C
      DXMV = GAIN18 * ((ERR18 - ERROLD18)+ERR18*DELTAT*3./TAUI18)
C
      SETPT(10) = SETPT(10) + DXMV * 150. / 100.
C
      ERROLD18 = ERR18
C
      RETURN
      END
C
C=============================================================================
C
      SUBROUTINE CONTRL19
C
C  Discrete control algorithms
C
C
C   MEASUREMENT AND VALVE COMMON BLOCK
C
      DOUBLE PRECISION XMEAS, XMV
      COMMON/PV/ XMEAS(41), XMV(12)
C
C   CONTROLLER COMMON BLOCK
C
      DOUBLE PRECISION SETPT, DELTAT
      COMMON/CTRLALL/ SETPT(20), DELTAT
      DOUBLE PRECISION GAIN19, TAUI19, ERROLD19
      COMMON/CTRL19/ GAIN19, TAUI19, ERROLD19
C
      DOUBLE PRECISION ERR19, DXMV
C
C  Example PI Controller:
C    Stripper Level Controller
C
C    Calculate Error
C
      ERR19 = (SETPT(19) - XMEAS(30)) * 100. / 26.
C      PRINT *, 'ERROLD19= ', ERROLD19
C
C    Proportional-Integral Controller (Velocity Form)
C         GAIN = Controller Gain
C         TAUI = Reset Time (min)
C
      DXMV = GAIN19*((ERR19 - ERROLD19)+ERR19*DELTAT*360./TAUI19)
C
      SETPT(6) = SETPT(6) + DXMV * 1. / 100.
C      PRINT *, 'SETPT(6)= ', SETPT(6)
C
      ERROLD19 = ERR19
C
      RETURN
      END
C
C=============================================================================
C
      SUBROUTINE CONTRL20
C
C  Discrete control algorithms
C
C
C   MEASUREMENT AND VALVE COMMON BLOCK
C
      DOUBLE PRECISION XMEAS, XMV
      COMMON/PV/ XMEAS(41), XMV(12)
C
C   CONTROLLER COMMON BLOCK
C
      DOUBLE PRECISION SETPT, DELTAT
      COMMON/CTRLALL/ SETPT(20), DELTAT
      DOUBLE PRECISION GAIN20, TAUI20, ERROLD20
      COMMON/CTRL20/  GAIN20, TAUI20, ERROLD20
C    
      DOUBLE PRECISION ERR20, DXMV
C
C  Example PI Controller:
C    Stripper Level Controller
C
C    Calculate Error
C
      ERR20 = (SETPT(20) - XMEAS(38)) * 100. / 1.6
C
C    Proportional-Integral Controller (Velocity Form)
C         GAIN = Controller Gain
C         TAUI = Reset Time (min)
C
      DXMV = GAIN20*((ERR20 - ERROLD20)+ERR20*DELTAT*900./TAUI20)
C
      SETPT(16) = SETPT(16) + DXMV  * 130. / 100.
C
      ERROLD20 = ERR20
C
      RETURN
      END
C
C=============================================================================
C
      SUBROUTINE CONTRL22
C
C  Discrete control algorithms
C
C
C   MEASUREMENT AND VALVE COMMON BLOCK
C
      DOUBLE PRECISION XMEAS, XMV
      COMMON/PV/ XMEAS(41), XMV(12)
C
C   CONTROLLER COMMON BLOCK
C
      DOUBLE PRECISION SETPT, DELTAT
      COMMON/CTRLALL/ SETPT(20), DELTAT
      DOUBLE PRECISION GAIN22, TAUI22, ERROLD22
      COMMON/CTRL22/  GAIN22, TAUI22, ERROLD22
C    
      DOUBLE PRECISION ERR22, DXMV
C
C  Example PI Controller:
C    Stripper Level Controller
C
C    Calculate Error
C
      ERR22 = SETPT(12) - XMEAS(13)
C
C    Proportional-Integral Controller (Velocity Form)
C         GAIN = Controller Gain
C         TAUI = Reset Time (min)
C
      DXMV = GAIN22*((ERR22 - ERROLD22)+ERR22*DELTAT*3./TAUI22)
C
      XMV(6) = XMV(6) + DXMV
C
      ERROLD22 = ERR22
C
      RETURN
      END
C
C=============================================================================
C
      SUBROUTINE OUTPUT
C
C
C   MEASUREMENT AND VALVE COMMON BLOCK
C
      DOUBLE PRECISION XMEAS, XMV
      COMMON/PV/ XMEAS(41), XMV(12)
C
        WRITE(1111,100) XMV(1), XMV(2), XMV(3), XMV(4)
      	WRITE(1112,100) XMV(5), XMV(6), XMV(7), XMV(8)
      	WRITE(1113,100) XMV(9), XMV(10), XMV(11), XMV(12)
      	WRITE(2111,100) XMEAS(1), XMEAS(2), XMEAS(3), XMEAS(4)
      	WRITE(2112,100) XMEAS(5), XMEAS(6), XMEAS(7), XMEAS(8)
      	WRITE(2113,100) XMEAS(9), XMEAS(10), XMEAS(11), XMEAS(12)
     	WRITE(2114,100) XMEAS(13), XMEAS(14), XMEAS(15), XMEAS(16)
      	WRITE(2115,100) XMEAS(17), XMEAS(18), XMEAS(19), XMEAS(20)
      	WRITE(2116,100) XMEAS(21), XMEAS(22), XMEAS(23), XMEAS(24)
      	WRITE(2117,100) XMEAS(25), XMEAS(26), XMEAS(27), XMEAS(28)
      	WRITE(2118,100) XMEAS(29), XMEAS(30), XMEAS(31), XMEAS(32)
      	WRITE(2119,100) XMEAS(33), XMEAS(34), XMEAS(35), XMEAS(36)
      	WRITE(2120,100) XMEAS(37), XMEAS(38), XMEAS(39), XMEAS(40)
      	WRITE(2121,300) XMEAS(41)
 100  FORMAT(1X,E13.5,2X,E13.5,2X,E13.5,2X,E13.5)
 200  FORMAT(1X,E13.5,2X,E13.5,2X,E13.5)
 300  FORMAT(1X,E13.5)
C
      RETURN
      END
C
C=============================================================================
C
      SUBROUTINE INTGTR(NN,TIME,DELTAT,YY,YP)
C
C  Euler Integration Algorithm
C
C
      INTEGER I, NN
C
      DOUBLE PRECISION TIME, DELTAT, YY(NN), YP(NN)
C
      CALL TEFUNC(NN,TIME,YY,YP)
C
      TIME = TIME + DELTAT
C
      DO 100 I = 1, NN
C
          YY(I) = YY(I) + YP(I) * DELTAT 
C
 100  CONTINUE
C
      RETURN
      END
C
C=============================================================================
C
      SUBROUTINE CONSHAND
C
C  Euler Integration Algorithm
C
C
      DOUBLE PRECISION XMEAS, XMV
      COMMON/PV/ XMEAS(41), XMV(12)
C
	INTEGER I
C	
	DO 100 I=1, 11
		IF (XMV(I).LE.0.0) XMV(I)=0.
 		IF (XMV(I).GE.100.0) XMV(I)=100.
 100    CONTINUE
C
      RETURN
      END

