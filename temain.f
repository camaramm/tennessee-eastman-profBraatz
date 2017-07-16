C               Tennessee Eastman Process Control Test Problem
C
C                    James J. Downs and Ernest F. Vogel
C
C                  Process and Control Systems Engineering
C                        Tennessee Eastman Company
C                              P.O. Box 511
C                          Kingsport, TN  37662
C
C
C  Reference:
C    "A Plant-Wide Industrial Process Control Problem"
C    Presented at the AIChE 1990 Annual Meeting
C    Industrial Challenge Problems in Process Control, Paper #24a
C    Chicago, Illinois, November 14, 1990
C
C    "A Plant-Wide Industrial Process Control Problem"
C    Computers and Chemical Engineering, Vol. 17, No. 3, pp. 245-255
C    (1993).
C    
C
C  Main program for demonstrating application of the Tennessee Eastman
C  Process Control Test Problem
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
      DOUBLE PRECISION SETPT, GAIN, TAUI, ERROLD, DELTAT
      COMMON/CTRL/ SETPT, GAIN, TAUI, ERROLD, DELTAT
C
C  Local Variables
C
      INTEGER I, NN, NPTS
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
      NPTS = 1000
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
      SETPT = XMEAS(15) + 15.0
      GAIN = 2.0
      TAUI = 5.0
      ERROLD = 0.0
C
C  Example Disturbance:
C    Change Reactor Cooling
C
      XMV(10) = 38.
C
C  Set all Disturbance Flags to OFF
C
      DO 100 I = 1, 20
          IDV(I) = 0
 100  CONTINUE
C
C  Simulation Loop
C
      DO 1000 I = 1, NPTS
C
          CALL CONTRL
C
          CALL OUTPUT
C
          CALL INTGTR(NN,TIME,DELTAT,YY,YP)
C
 1000 CONTINUE
C
      STOP
      END
C
C=============================================================================
C
      SUBROUTINE CONTRL
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
      DOUBLE PRECISION SETPT, GAIN, TAUI, ERROLD, DELTAT
      COMMON/CTRL/ SETPT, GAIN, TAUI, ERROLD, DELTAT
C
      DOUBLE PRECISION ERR, DXMV
C
C  Example PI Controller:
C    Stripper Level Controller
C
C    Calculate Error
C
      ERR = SETPT - XMEAS(15)
C
C    Proportional-Integral Controller (Velocity Form)
C         GAIN = Controller Gain
C         TAUI = Reset Time (min)
C
      DXMV = GAIN * ( ( ERR - ERROLD ) + ERR * DELTAT * 60. / TAUI )
C
      XMV(8) = XMV(8) - DXMV
C
      ERROLD = ERR
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
      WRITE(6,100) XMEAS(9), XMEAS(15), XMV(8)
 100  FORMAT(1X,'Reac Temp = ',F6.2,
     .       2X,'Stripper Lev = ',F6.2,
     .       2X,'Sripper Underflow = ',F6.2)
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
