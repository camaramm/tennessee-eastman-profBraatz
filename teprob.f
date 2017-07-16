C
C               Tennessee Eastman Process Control Test Problem
C
C                    James J. Downs and Ernest F. Vogel
C
C                  Process and Control Systems Engineering
C                        Tennessee Eastman Company
C                              P.O. Box 511
C                          Kingsport,TN  37662
C
C  Reference:
C    "A Plant-Wide Industrial Process Control Problem"
C    Presented at the AIChE 1990 Annual Meeting
C    Industrial Challenge Problems in Process Control,Paper #24a
C    Chicago,Illinois,November 14,1990
C
C  Revised 4-4-91 to correct error in documentation of manipulated variables
C
C  Subroutines:
C
C    TEFUNC - Function evaluator to be called by integrator
C    TEINIT - Initialization
C    TESUBi - Utility subroutines, i=1,2,..,8
C
C
C  The process simulation has 50 states (NN=50).  If the user wishes to
C  integrate additional states, NN must be increased accordingly in the
C  calling program.  The additional states should be appended to the end
C  of the YY vector, e.g. YY(51),...  The additional derivatives should
C  be appended to the end of the YP vector, e.g. YP(51),...  To initialize
C  the new states and to calculate derivatives for them, we suggest
C  creating new function evaluator and initialization routines as follows.
C
C          C-----------------------------------------------
C          C
C                SUBROUTINE FUNC(NN,TIME,YY,YP)
C          C
C                INTEGER NN
C                DOUBLE PRECISION TIME, YY(NN), YP(NN)
C          C
C          C  Call the function evaluator for the process
C          C
C                CALL TEFUNC(NN,TIME,YY,YP)
C          C
C          C  Calculate derivatives for additional states
C          C
C                YP(51) = ....
C                YP(52) = ....
C                   .
C                   .
C                   .
C                YP(NN) = ....
C          C
C                RETURN
C                END
C          C
C          C-----------------------------------------------
C          C
C                SUBROUTINE INIT(NN,TIME,YY,YP)
C          C
C                INTEGER NN
C                DOUBLE PRECISION TIME, YY(NN), YP(NN)
C          C
C          C  Call the initialization for the process
C          C
C                CALL TEINIT(NN,TIME,YY,YP)
C          C
C          C  Initialize additional states
C          C
C                YY(51) = ....
C                YY(52) = ....
C                   .
C                   .
C                   .
C                YY(NN) = ....
C          C
C                RETURN
C                END
C          C
C          C-----------------------------------------------
C
C  Differences between the code and its description in the paper:
C
C  1.  Subroutine TEINIT has TIME in the argument list.  TEINIT sets TIME
C      to zero.
C
C  2.  There are 8 utility subroutines (TESUBi) rather than 5.
C
C  3.  Process disturbances 14 through 20 do NOT need to be used in
C      conjunction with another disturbance as stated in the paper.  All
C      disturbances can be used alone or in any combination.
C
C
C  Manipulated Variables
C
C    XMV(1)     D Feed Flow (stream 2)            (Corrected Order)
C    XMV(2)     E Feed Flow (stream 3)            (Corrected Order)
C    XMV(3)     A Feed Flow (stream 1)            (Corrected Order)
C    XMV(4)     A and C Feed Flow (stream 4)
C    XMV(5)     Compressor Recycle Valve
C    XMV(6)     Purge Valve (stream 9)
C    XMV(7)     Separator Pot Liquid Flow (stream 10)
C    XMV(8)     Stripper Liquid Product Flow (stream 11)
C    XMV(9)     Stripper Steam Valve
C    XMV(10)    Reactor Cooling Water Flow
C    XMV(11)    Condenser Cooling Water Flow
C    XMV(12)    Agitator Speed
C
C  Continuous Process Measurements
C
C    XMEAS(1)   A Feed  (stream 1)                    kscmh
C    XMEAS(2)   D Feed  (stream 2)                    kg/hr
C    XMEAS(3)   E Feed  (stream 3)                    kg/hr
C    XMEAS(4)   A and C Feed  (stream 4)              kscmh
C    XMEAS(5)   Recycle Flow  (stream 8)              kscmh
C    XMEAS(6)   Reactor Feed Rate  (stream 6)         kscmh
C    XMEAS(7)   Reactor Pressure                      kPa gauge
C    XMEAS(8)   Reactor Level                         %
C    XMEAS(9)   Reactor Temperature                   Deg C
C    XMEAS(10)  Purge Rate (stream 9)                 kscmh
C    XMEAS(11)  Product Sep Temp                      Deg C
C    XMEAS(12)  Product Sep Level                     %
C    XMEAS(13)  Prod Sep Pressure                     kPa gauge
C    XMEAS(14)  Prod Sep Underflow (stream 10)        m3/hr
C    XMEAS(15)  Stripper Level                        %
C    XMEAS(16)  Stripper Pressure                     kPa gauge
C    XMEAS(17)  Stripper Underflow (stream 11)        m3/hr
C    XMEAS(18)  Stripper Temperature                  Deg C
C    XMEAS(19)  Stripper Steam Flow                   kg/hr
C    XMEAS(20)  Compressor Work                       kW
C    XMEAS(21)  Reactor Cooling Water Outlet Temp     Deg C
C    XMEAS(22)  Separator Cooling Water Outlet Temp   Deg C
C
C  Sampled Process Measurements
C
C    Reactor Feed Analysis (Stream 6)
C        Sampling Frequency = 0.1 hr
C        Dead Time = 0.1 hr
C        Mole %
C    XMEAS(23)   Component A
C    XMEAS(24)   Component B
C    XMEAS(25)   Component C
C    XMEAS(26)   Component D
C    XMEAS(27)   Component E
C    XMEAS(28)   Component F
C
C    Purge Gas Analysis (Stream 9)
C        Sampling Frequency = 0.1 hr
C        Dead Time = 0.1 hr
C        Mole %
C    XMEAS(29)   Component A
C    XMEAS(30)   Component B
C    XMEAS(31)   Component C
C    XMEAS(32)   Component D
C    XMEAS(33)   Component E
C    XMEAS(34)   Component F
C    XMEAS(35)   Component G
C    XMEAS(36)   Component H
C
C    Product Analysis (Stream 11)
C        Sampling Frequency = 0.25 hr
C        Dead Time = 0.25 hr
C        Mole %
C    XMEAS(37)   Component D
C    XMEAS(38)   Component E
C    XMEAS(39)   Component F
C    XMEAS(40)   Component G
C    XMEAS(41)   Component H
C
C  Process Disturbances
C
C    IDV(1)   A/C Feed Ratio, B Composition Constant (Stream 4)          Step
C    IDV(2)   B Composition, A/C Ratio Constant (Stream 4)               Step
C    IDV(3)   D Feed Temperature (Stream 2)                              Step
C    IDV(4)   Reactor Cooling Water Inlet Temperature                    Step
C    IDV(5)   Condenser Cooling Water Inlet Temperature                  Step
C    IDV(6)   A Feed Loss (Stream 1)                                     Step
C    IDV(7)   C Header Pressure Loss - Reduced Availability (Stream 4)   Step
C    IDV(8)   A, B, C Feed Composition (Stream 4)            Random Variation
C    IDV(9)   D Feed Temperature (Stream 2)                  Random Variation
C    IDV(10)  C Feed Temperature (Stream 4)                  Random Variation
C    IDV(11)  Reactor Cooling Water Inlet Temperature        Random Variation
C    IDV(12)  Condenser Cooling Water Inlet Temperature      Random Variation
C    IDV(13)  Reaction Kinetics                                    Slow Drift
C    IDV(14)  Reactor Cooling Water Valve                            Sticking
C    IDV(15)  Condenser Cooling Water Valve                          Sticking
C    IDV(16)  Unknown
C    IDV(17)  Unknown
C    IDV(18)  Unknown
C    IDV(19)  Unknown
C    IDV(20)  Unknown
C
C
C=============================================================================
C
      SUBROUTINE TEFUNC(NN,TIME,YY,YP)
C
C       Function Evaluator
C
C         Inputs:
C
C           NN   = Number of differential equations
C           Time = Current time(hrs)
C           YY   = Current state values
C
C         Outputs:
C
C           YP   = Current derivative values
C
      DOUBLE PRECISION XMEAS,XMV
      COMMON/PV/XMEAS(41),XMV(12)
      INTEGER IDV
      COMMON/DVEC/IDV(20)
      DOUBLE PRECISION
     .UCLR,UCVR,UTLR,UTVR,
     .XLR,XVR,ETR,ESR,
     .TCR,TKR,DLR,
     .VLR,VVR,VTR,
     .PTR,PPR,
     .CRXR,RR,RH,
     .FWR,TWR,QUR,HWR,UAR,
     .UCLS,UCVS,UTLS,UTVS,
     .XLS,XVS,ETS,ESS,
     .TCS,TKS,DLS,
     .VLS,VVS,VTS,
     .PTS,PPS,
     .FWS,TWS,QUS,HWS,
     .UCLC,UTLC,XLC,
     .ETC,ESC,TCC,DLC,
     .VLC,VTC,QUC,
     .UCVV,UTVV,XVV,
     .ETV,ESV,TCV,TKV,
     .VTV,PTV,
     .VCV,VRNG,VTAU,
     .FTM,
     .FCM,XST,XMWS,
     .HST,TST,SFR,
     .CPFLMX,CPPRMX,CPDH,
     .TCWR,TCWS,
     .HTR,AGSP,
     .XDEL,XNS,
     .TGAS,TPROD,VST
      INTEGER
     .IVST
      COMMON/TEPROC/
     .UCLR(8),UCVR(8),UTLR,UTVR,
     .XLR(8),XVR(8),ETR,ESR,
     .TCR,TKR,DLR,
     .VLR,VVR,VTR,
     .PTR,PPR(8),
     .CRXR(8),RR(4),RH,
     .FWR,TWR,QUR,HWR,UAR,
     .UCLS(8),UCVS(8),UTLS,UTVS,
     .XLS(8),XVS(8),ETS,ESS,
     .TCS,TKS,DLS,
     .VLS,VVS,VTS,
     .PTS,PPS(8),
     .FWS,TWS,QUS,HWS,
     .UCLC(8),UTLC,XLC(8),
     .ETC,ESC,TCC,DLC,
     .VLC,VTC,QUC,
     .UCVV(8),UTVV,XVV(8),
     .ETV,ESV,TCV,TKV,
     .VTV,PTV,
     .VCV(12),VRNG(12),VTAU(12),
     .FTM(13),
     .FCM(8,13),XST(8,13),XMWS(13),
     .HST(13),TST(13),SFR(8),
     .CPFLMX,CPPRMX,CPDH,
     .TCWR,TCWS,
     .HTR(3),AGSP,
     .XDEL(41),XNS(41),
     .TGAS,TPROD,VST(12),IVST(12)
      INTEGER IDVWLK
      DOUBLE PRECISION
     .ADIST,
     .BDIST,
     .CDIST,
     .DDIST,
     .TLAST,
     .TNEXT,
     .HSPAN,
     .HZERO,
     .SSPAN,
     .SZERO,
     .SPSPAN
      COMMON/WLK/
     .ADIST(12),
     .BDIST(12),
     .CDIST(12),
     .DDIST(12),
     .TLAST(12),
     .TNEXT(12),
     .HSPAN(12),
     .HZERO(12),
     .SSPAN(12),
     .SZERO(12),
     .SPSPAN(12),
     .IDVWLK(12)
      DOUBLE PRECISION
     .AVP,BVP,CVP,
     .AH,BH,CH,
     .AG,BG,CG,
     .AV,
     .AD,BD,CD,
     .XMW
      COMMON/CONST/
     .AVP(8),BVP(8),CVP(8),
     .AH(8),BH(8),CH(8),
     .AG(8),BG(8),CG(8),
     .AV(8),
     .AD(8),BD(8),CD(8),
     .XMW(8)
      INTEGER NN,I,ISD
      DOUBLE PRECISION RG,
     .VPR,
     .FIN(8),
     .YY(NN),
     .YP(NN),
     .TIME,
     .FLMS,
     .DLP,
     .PR,
     .FLCOEF,
     .UAS,
     .UAC,
     .VOVRL,
     .UARLEV,
     .VPOS(12),
     .XMNS,
     .XCMP(41),
     .TMPFAC,
     .R1F,
     .R2F,
     .HWLK,
     .SWLK,
     .SPWLK,
     .TESUB7,
     .TESUB8
      DO 500 I=1,20
      IF(IDV(I).GT.0)THEN
      IDV(I)=1
      ELSE
      IDV(I)=0
      ENDIF
 500  CONTINUE
      IDVWLK(1)=IDV(8)
      IDVWLK(2)=IDV(8)
      IDVWLK(3)=IDV(9)
      IDVWLK(4)=IDV(10)
      IDVWLK(5)=IDV(11)
      IDVWLK(6)=IDV(12)
      IDVWLK(7)=IDV(13)
      IDVWLK(8)=IDV(13)
      IDVWLK(9)=IDV(16)
      IDVWLK(10)=IDV(17)
      IDVWLK(11)=IDV(18)
      IDVWLK(12)=IDV(20)
      DO 900 I=1,9
      IF(TIME.GE.TNEXT(I))THEN
      HWLK=TNEXT(I)-TLAST(I)
      SWLK=ADIST(I)+HWLK*(BDIST(I)+HWLK*
     .(CDIST(I)+HWLK*DDIST(I)))
      SPWLK=BDIST(I)+HWLK*
     .(2.D0*CDIST(I)+3.D0*HWLK*DDIST(I))
      TLAST(I)=TNEXT(I)
      CALL TESUB5(SWLK,SPWLK,ADIST(I),BDIST(I),CDIST(I),
     .DDIST(I),TLAST(I),TNEXT(I),HSPAN(I),HZERO(I),
     .SSPAN(I),SZERO(I),SPSPAN(I),IDVWLK(I))
      ENDIF
  900 CONTINUE
      DO 910 I=10,12
      IF(TIME.GE.TNEXT(I))THEN
      HWLK=TNEXT(I)-TLAST(I)
      SWLK=ADIST(I)+HWLK*(BDIST(I)+HWLK*
     .(CDIST(I)+HWLK*DDIST(I)))
      SPWLK=BDIST(I)+HWLK*
     .(2.D0*CDIST(I)+3.D0*HWLK*DDIST(I))
      TLAST(I)=TNEXT(I)
      IF(SWLK.GT.0.1D0)THEN
      ADIST(I)=SWLK
      BDIST(I)=SPWLK
      CDIST(I)=-(3.D0*SWLK+0.2D0*SPWLK)/0.01D0
      DDIST(I)=(2.D0*SWLK+0.1D0*SPWLK)/0.001D0
      TNEXT(I)=TLAST(I)+0.1D0
      ELSE
      ISD=-1
      HWLK=HSPAN(I)*TESUB7(ISD)+HZERO(I)
      ADIST(I)=0.D0
      BDIST(I)=0.D0
      CDIST(I)=DBLE(IDVWLK(I))/HWLK**2
      DDIST(I)=0.D0
      TNEXT(I)=TLAST(I)+HWLK
      ENDIF
      ENDIF
  910 CONTINUE
      IF(TIME.EQ.0.D0)THEN
      DO 950 I=1,12
      ADIST(I)=SZERO(I)
      BDIST(I)=0.D0
      CDIST(I)=0.D0
      DDIST(I)=0.D0
      TLAST(I)=0.0D0
      TNEXT(I)=0.1D0
  950 CONTINUE
      END IF
      XST(1,4)=TESUB8(1,TIME)-IDV(1)*0.03D0
     .-IDV(2)*2.43719D-3
      XST(2,4)=TESUB8(2,TIME)+IDV(2)*0.005D0
      XST(3,4)=1.D0-XST(1,4)-XST(2,4)
      TST(1)=TESUB8(3,TIME)+IDV(3)*5.D0
      TST(4)=TESUB8(4,TIME)
      TCWR=TESUB8(5,TIME)+IDV(4)*5.D0
      TCWS=TESUB8(6,TIME)+IDV(5)*5.D0
      R1F=TESUB8(7,TIME)
      R2F=TESUB8(8,TIME)
      DO 1010 I=1,3
      UCVR(I)=YY(I)
      UCVS(I)=YY(I+9)
      UCLR(I)=0.0
      UCLS(I)=0.0
 1010 CONTINUE
      DO 1020 I=4,8
      UCLR(I)=YY(I)
      UCLS(I)=YY(I+9)
 1020 CONTINUE
      DO 1030 I=1,8
      UCLC(I)=YY(I+18)
      UCVV(I)=YY(I+27)
 1030 CONTINUE
      ETR=YY(9)
      ETS=YY(18)
      ETC=YY(27)
      ETV=YY(36)
      TWR=YY(37)
      TWS=YY(38)
      DO 1035 I=1,12
      VPOS(I)=YY(I+38)
 1035 CONTINUE
      UTLR=0.0
      UTLS=0.0
      UTLC=0.0
      UTVV=0.0
      DO 1040 I=1,8
      UTLR=UTLR+UCLR(I)
      UTLS=UTLS+UCLS(I)
      UTLC=UTLC+UCLC(I)
      UTVV=UTVV+UCVV(I)
 1040 CONTINUE
      DO 1050 I=1,8
      XLR(I)=UCLR(I)/UTLR
      XLS(I)=UCLS(I)/UTLS
      XLC(I)=UCLC(I)/UTLC
      XVV(I)=UCVV(I)/UTVV
 1050 CONTINUE
      ESR=ETR/UTLR
      ESS=ETS/UTLS
      ESC=ETC/UTLC
      ESV=ETV/UTVV
      CALL TESUB2(XLR,TCR,ESR,0)
      TKR=TCR+273.15
      CALL TESUB2(XLS,TCS,ESS,0)
      TKS=TCS+273.15
      CALL TESUB2(XLC,TCC,ESC,0)
      CALL TESUB2(XVV,TCV,ESV,2)
      TKV=TCV+273.15
      CALL TESUB4(XLR,TCR,DLR)
      CALL TESUB4(XLS,TCS,DLS)
      CALL TESUB4(XLC,TCC,DLC)
      VLR=UTLR/DLR
      VLS=UTLS/DLS
      VLC=UTLC/DLC
      VVR=VTR-VLR
      VVS=VTS-VLS
      RG=998.9
      PTR=0.0
      PTS=0.0
      DO 1110 I=1,3
      PPR(I)=UCVR(I)*RG*TKR/VVR
      PTR=PTR+PPR(I)
      PPS(I)=UCVS(I)*RG*TKS/VVS
      PTS=PTS+PPS(I)
 1110 CONTINUE
      DO 1120 I=4,8
      VPR=DEXP(AVP(I)+BVP(I)/(TCR+CVP(I)))
      PPR(I)=VPR*XLR(I)
      PTR=PTR+PPR(I)
      VPR=DEXP(AVP(I)+BVP(I)/(TCS+CVP(I)))
      PPS(I)=VPR*XLS(I)
      PTS=PTS+PPS(I)
 1120 CONTINUE
      PTV=UTVV*RG*TKV/VTV
      DO 1130 I=1,8
      XVR(I)=PPR(I)/PTR
      XVS(I)=PPS(I)/PTS
 1130 CONTINUE
      UTVR=PTR*VVR/RG/TKR
      UTVS=PTS*VVS/RG/TKS
      DO 1140 I=4,8
      UCVR(I)=UTVR*XVR(I)
      UCVS(I)=UTVS*XVS(I)
 1140 CONTINUE
      RR(1)=DEXP(31.5859536-40000.0/1.987/TKR)*R1F
      RR(2)=DEXP(3.00094014-20000.0/1.987/TKR)*R2F
      RR(3)=DEXP(53.4060443-60000.0/1.987/TKR)
      RR(4)=RR(3)*0.767488334D0
      IF(PPR(1).GT.0.0.AND.PPR(3).GT.0.0)THEN
      R1F=PPR(1)**1.1544
      R2F=PPR(3)**0.3735
      RR(1)=RR(1)*R1F*R2F*PPR(4)
      RR(2)=RR(2)*R1F*R2F*PPR(5)
      ELSE
      RR(1)=0.0
      RR(2)=0.0
      ENDIF
      RR(3)=RR(3)*PPR(1)*PPR(5)
      RR(4)=RR(4)*PPR(1)*PPR(4)
      DO 1200 I=1,4
      RR(I)=RR(I)*VVR
 1200 CONTINUE
      CRXR(1)=-RR(1)-RR(2)-RR(3)
      CRXR(3)=-RR(1)-RR(2)
      CRXR(4)=-RR(1)-1.5D0*RR(4)
      CRXR(5)=-RR(2)-RR(3)
      CRXR(6)=RR(3)+RR(4)
      CRXR(7)=RR(1)
      CRXR(8)=RR(2)
      RH=RR(1)*HTR(1)+RR(2)*HTR(2)
      XMWS(1)=0.0
      XMWS(2)=0.0
      XMWS(6)=0.0
      XMWS(8)=0.0
      XMWS(9)=0.0
      XMWS(10)=0.0
      DO 2010 I=1,8
      XST(I,6)=XVV(I)
      XST(I,8)=XVR(I)
      XST(I,9)=XVS(I)
      XST(I,10)=XVS(I)
      XST(I,11)=XLS(I)
      XST(I,13)=XLC(I)
      XMWS(1)=XMWS(1)+XST(I,1)*XMW(I)
      XMWS(2)=XMWS(2)+XST(I,2)*XMW(I)
      XMWS(6)=XMWS(6)+XST(I,6)*XMW(I)
      XMWS(8)=XMWS(8)+XST(I,8)*XMW(I)
      XMWS(9)=XMWS(9)+XST(I,9)*XMW(I)
      XMWS(10)=XMWS(10)+XST(I,10)*XMW(I)
 2010 CONTINUE
      TST(6)=TCV
      TST(8)=TCR
      TST(9)=TCS
      TST(10)=TCS
      TST(11)=TCS
      TST(13)=TCC
      CALL TESUB1(XST(1,1),TST(1),HST(1),1)
      CALL TESUB1(XST(1,2),TST(2),HST(2),1)
      CALL TESUB1(XST(1,3),TST(3),HST(3),1)
      CALL TESUB1(XST(1,4),TST(4),HST(4),1)
      CALL TESUB1(XST(1,6),TST(6),HST(6),1)
      CALL TESUB1(XST(1,8),TST(8),HST(8),1)
      CALL TESUB1(XST(1,9),TST(9),HST(9),1)
      HST(10)=HST(9)
      CALL TESUB1(XST(1,11),TST(11),HST(11),0)
      CALL TESUB1(XST(1,13),TST(13),HST(13),0)
      FTM(1)=VPOS(1)*VRNG(1)/100.0
      FTM(2)=VPOS(2)*VRNG(2)/100.0
      FTM(3)=VPOS(3)*(1.D0-IDV(6))*VRNG(3)/100.0
      FTM(4)=VPOS(4)*(1.D0-IDV(7)*0.2D0)
     .*VRNG(4)/100.0+1.D-10
      FTM(11)=VPOS(7)*VRNG(7)/100.0
      FTM(13)=VPOS(8)*VRNG(8)/100.0
      UAC=VPOS(9)*VRNG(9)*(1.D0+TESUB8(9,TIME))/100.0
      FWR=VPOS(10)*VRNG(10)/100.0
      FWS=VPOS(11)*VRNG(11)/100.0
      AGSP=(VPOS(12)+150.0)/100.0
      DLP=PTV-PTR
      IF(DLP.LT.0.0)DLP=0.0
      FLMS=1937.6D0*DSQRT(DLP)
      FTM(6)=FLMS/XMWS(6)
      DLP=PTR-PTS
      IF(DLP.LT.0.0)DLP=0.0
      FLMS=4574.21D0*DSQRT(DLP)
     .*(1.D0-0.25D0*TESUB8(12,TIME))
      FTM(8)=FLMS/XMWS(8)
      DLP=PTS-760.0
      IF(DLP.LT.0.0)DLP=0.0
      FLMS=VPOS(6)*0.151169D0*DSQRT(DLP)
      FTM(10)=FLMS/XMWS(10)
      PR=PTV/PTS
      IF(PR.LT.1.0)PR=1.0
      IF(PR.GT.CPPRMX)PR=CPPRMX
      FLCOEF=CPFLMX/1.197D0
      FLMS=CPFLMX+FLCOEF*(1.0-PR**3)
      CPDH=FLMS*(TCS+273.15D0)*1.8D-6*1.9872D0
     .*(PTV-PTS)/(XMWS(9)*PTS)
      DLP=PTV-PTS
      IF(DLP.LT.0.0)DLP=0.0
      FLMS=FLMS-VPOS(5)*53.349D0*DSQRT(DLP)
      IF(FLMS.LT.1.D-3)FLMS=1.D-3
      FTM(9)=FLMS/XMWS(9)
      HST(9)=HST(9)+CPDH/FTM(9)
      DO 5020 I=1,8
      FCM(I,1)=XST(I,1)*FTM(1)
      FCM(I,2)=XST(I,2)*FTM(2)
      FCM(I,3)=XST(I,3)*FTM(3)
      FCM(I,4)=XST(I,4)*FTM(4)
      FCM(I,6)=XST(I,6)*FTM(6)
      FCM(I,8)=XST(I,8)*FTM(8)
      FCM(I,9)=XST(I,9)*FTM(9)
      FCM(I,10)=XST(I,10)*FTM(10)
      FCM(I,11)=XST(I,11)*FTM(11)
      FCM(I,13)=XST(I,13)*FTM(13)
 5020 CONTINUE
      IF(FTM(11).GT.0.1)THEN
      IF(TCC.GT.170.)THEN
      TMPFAC=TCC-120.262
      ELSEIF(TCC.LT.5.292)THEN
      TMPFAC=0.1
      ELSE
      TMPFAC=363.744/(177.-TCC)-2.22579488
      ENDIF
      VOVRL=FTM(4)/FTM(11)*TMPFAC
      SFR(4)=8.5010*VOVRL/(1.0+8.5010*VOVRL)
      SFR(5)=11.402*VOVRL/(1.0+11.402*VOVRL)
      SFR(6)=11.795*VOVRL/(1.0+11.795*VOVRL)
      SFR(7)=0.0480*VOVRL/(1.0+0.0480*VOVRL)
      SFR(8)=0.0242*VOVRL/(1.0+0.0242*VOVRL)
      ELSE
      SFR(4)=0.9999
      SFR(5)=0.999
      SFR(6)=0.999
      SFR(7)=0.99
      SFR(8)=0.98
      END IF
      DO 6010 I=1,8
      FIN(I)=0.0
      FIN(I)=FIN(I)+FCM(I,4)
      FIN(I)=FIN(I)+FCM(I,11)
 6010 CONTINUE
      FTM(5)=0.0
      FTM(12)=0.0
      DO 6020 I=1,8
      FCM(I,5)=SFR(I)*FIN(I)
      FCM(I,12)=FIN(I)-FCM(I,5)
      FTM(5)=FTM(5)+FCM(I,5)
      FTM(12)=FTM(12)+FCM(I,12)
 6020 CONTINUE
      DO 6030 I=1,8
      XST(I,5)=FCM(I,5)/FTM(5)
      XST(I,12)=FCM(I,12)/FTM(12)
 6030 CONTINUE
      TST(5)=TCC
      TST(12)=TCC
      CALL TESUB1(XST(1,5),TST(5),HST(5),1)
      CALL TESUB1(XST(1,12),TST(12),HST(12),0)
      FTM(7)=FTM(6)
      HST(7)=HST(6)
      TST(7)=TST(6)
      DO 6130 I=1,8
      XST(I,7)=XST(I,6)
      FCM(I,7)=FCM(I,6)
 6130 CONTINUE
      IF(VLR/7.8.GT.50.0)THEN
      UARLEV=1.0
      ELSEIF(VLR/7.8.LT.10.0)THEN
      UARLEV=0.0
      ELSE
      UARLEV=0.025*VLR/7.8-0.25
      ENDIF
      UAR=UARLEV*(-0.5*AGSP**2
     .+2.75*AGSP-2.5)*855490.D-6
      QUR=UAR*(TWR-TCR)
     .*(1.D0-0.35D0*TESUB8(10,TIME))
      UAS=0.404655*(1.0-1.0/(1.0+(FTM(8)/3528.73)**4))
      QUS=UAS*(TWS-TST(8))
     .*(1.D0-0.25D0*TESUB8(11,TIME))
      QUC=0.D0
      IF(TCC.LT.100.)QUC=UAC*(100.0-TCC)
      XMEAS(1)=FTM(3)*0.359/35.3145
      XMEAS(2)=FTM(1)*XMWS(1)*0.454
      XMEAS(3)=FTM(2)*XMWS(2)*0.454
      XMEAS(4)=FTM(4)*0.359/35.3145
      XMEAS(5)=FTM(9)*0.359/35.3145
      XMEAS(6)=FTM(6)*0.359/35.3145
      XMEAS(7)=(PTR-760.0)/760.0*101.325
      XMEAS(8)=(VLR-84.6)/666.7*100.0
      XMEAS(9)=TCR
      XMEAS(10)=FTM(10)*0.359/35.3145
      XMEAS(11)=TCS
      XMEAS(12)=(VLS-27.5)/290.0*100.0
      XMEAS(13)=(PTS-760.0)/760.0*101.325
      XMEAS(14)=FTM(11)/DLS/35.3145
      XMEAS(15)=(VLC-78.25)/VTC*100.0
      XMEAS(16)=(PTV-760.0)/760.0*101.325
      XMEAS(17)=FTM(13)/DLC/35.3145
      XMEAS(18)=TCC
      XMEAS(19)=QUC*1.04D3*0.454
      XMEAS(20)=CPDH*0.0003927D6
      XMEAS(20)=CPDH*0.29307D3
      XMEAS(21)=TWR
      XMEAS(22)=TWS
      ISD=0
      IF(XMEAS(7).GT.3000.0)ISD=1
      IF(VLR/35.3145.GT.24.0)ISD=1
      IF(VLR/35.3145.LT.2.0)ISD=1
      IF(XMEAS(9).GT.175.0)ISD=1
      IF(VLS/35.3145.GT.12.0)ISD=1
      IF(VLS/35.3145.LT.1.0)ISD=1
      IF(VLC/35.3145.GT.8.0)ISD=1
      IF(VLC/35.3145.LT.1.0)ISD=1
      IF(TIME.GT.0.0.AND.ISD.EQ.0)THEN
      DO 6500 I=1,22
      CALL TESUB6(XNS(I),XMNS)
      XMEAS(I)=XMEAS(I)+XMNS
 6500 CONTINUE
      ENDIF
      XCMP(23)=XST(1,7)*100.0
      XCMP(24)=XST(2,7)*100.0
      XCMP(25)=XST(3,7)*100.0
      XCMP(26)=XST(4,7)*100.0
      XCMP(27)=XST(5,7)*100.0
      XCMP(28)=XST(6,7)*100.0
      XCMP(29)=XST(1,10)*100.0
      XCMP(30)=XST(2,10)*100.0
      XCMP(31)=XST(3,10)*100.0
      XCMP(32)=XST(4,10)*100.0
      XCMP(33)=XST(5,10)*100.0
      XCMP(34)=XST(6,10)*100.0
      XCMP(35)=XST(7,10)*100.0
      XCMP(36)=XST(8,10)*100.0
      XCMP(37)=XST(4,13)*100.0
      XCMP(38)=XST(5,13)*100.0
      XCMP(39)=XST(6,13)*100.0
      XCMP(40)=XST(7,13)*100.0
      XCMP(41)=XST(8,13)*100.0
      IF(TIME.EQ.0.D0)THEN
      DO 7010 I=23,41
      XDEL(I)=XCMP(I)
      XMEAS(I)=XCMP(I)
 7010 CONTINUE
      TGAS=0.1
      TPROD=0.25
      ENDIF
      IF(TIME.GE.TGAS)THEN
      DO 7020 I=23,36
      XMEAS(I)=XDEL(I)
      CALL TESUB6(XNS(I),XMNS)
      XMEAS(I)=XMEAS(I)+XMNS
      XDEL(I)=XCMP(I)
 7020 CONTINUE
      TGAS=TGAS+0.1
      ENDIF
      IF(TIME.GE.TPROD)THEN
      DO 7030 I=37,41
      XMEAS(I)=XDEL(I)
      CALL TESUB6(XNS(I),XMNS)
      XMEAS(I)=XMEAS(I)+XMNS
      XDEL(I)=XCMP(I)
 7030 CONTINUE
      TPROD=TPROD+0.25
      ENDIF
      DO 9010 I=1,8
      YP(I)=FCM(I,7)-FCM(I,8)+CRXR(I)
      YP(I+9)=FCM(I,8)-FCM(I,9)-
     .FCM(I,10)-FCM(I,11)
      YP(I+18)=FCM(I,12)-FCM(I,13)
      YP(I+27)=FCM(I,1)+FCM(I,2)+
     .FCM(I,3)+FCM(I,5)+
     .FCM(I,9)-FCM(I,6)
 9010 CONTINUE
      YP(9)=HST(7)*FTM(7)-
     .HST(8)*FTM(8)+RH+QUR
      YP(18)=HST(8)*FTM(8)-
     .HST(9)*FTM(9)-
     .HST(10)*FTM(10)-
     .HST(11)*FTM(11)+
     .QUS
      YP(27)=HST(4)*FTM(4)+
     .HST(11)*FTM(11)-
     .HST(5)*FTM(5)-
     .HST(13)*FTM(13)+
     .QUC
      YP(36)=HST(1)*FTM(1)+
     .HST(2)*FTM(2)+
     .HST(3)*FTM(3)+
     .HST(5)*FTM(5)+
     .HST(9)*FTM(9)-
     .HST(6)*FTM(6)
      YP(37)=(FWR*500.53*
     .(TCWR-TWR)-QUR*1.D6/1.8)/HWR
      YP(38)=(FWS*500.53*
     .(TCWS-TWS)-QUS*1.D6/1.8)/HWS
      IVST(10)=IDV(14)
      IVST(11)=IDV(15)
      IVST(5)=IDV(19)
      IVST(7)=IDV(19)
      IVST(8)=IDV(19)
      IVST(9)=IDV(19)
      DO 9020 I=1,12
      IF(TIME.EQ.0.D0 .OR.
     .DABS(VCV(I)-XMV(I)).GT.VST(I)*IVST(I))
     .VCV(I)=XMV(I)
      IF(VCV(I).LT.0.0)VCV(I)=0.0
      IF(VCV(I).GT.100.0)VCV(I)=100.0
      YP(I+38)=(VCV(I)-VPOS(I))/VTAU(I)
 9020 CONTINUE
      IF(ISD.NE.0)THEN
      DO 9030 I=1,NN
      YP(I)=0.0
 9030 CONTINUE
      ENDIF
      RETURN
      END
C
C=============================================================================
C
      SUBROUTINE TEINIT(NN,TIME,YY,YP)
C
C       Initialization
C
C         Inputs:
C
C           NN   = Number of differential equations
C
C         Outputs:
C
C           Time = Current time(hrs)
C           YY   = Current state values
C           YP   = Current derivative values
C
      DOUBLE PRECISION XMEAS,XMV
      COMMON/PV/XMEAS(41),XMV(12)
      INTEGER IDV
      COMMON/DVEC/IDV(20)
      DOUBLE PRECISION G
      COMMON/RANDSD/G
      DOUBLE PRECISION
     .UCLR,UCVR,UTLR,UTVR,
     .XLR,XVR,ETR,ESR,
     .TCR,TKR,DLR,
     .VLR,VVR,VTR,
     .PTR,PPR,
     .CRXR,RR,RH,
     .FWR,TWR,QUR,HWR,UAR,
     .UCLS,UCVS,UTLS,UTVS,
     .XLS,XVS,ETS,ESS,
     .TCS,TKS,DLS,
     .VLS,VVS,VTS,
     .PTS,PPS,
     .FWS,TWS,QUS,HWS,
     .UCLC,UTLC,XLC,
     .ETC,ESC,TCC,DLC,
     .VLC,VTC,QUC,
     .UCVV,UTVV,XVV,
     .ETV,ESV,TCV,TKV,
     .VTV,PTV,
     .VCV,VRNG,VTAU,
     .FTM,
     .FCM,XST,XMWS,
     .HST,TST,SFR,
     .CPFLMX,CPPRMX,CPDH,
     .TCWR,TCWS,
     .HTR,AGSP,
     .XDEL,XNS,
     .TGAS,TPROD,VST
      INTEGER
     .IVST
      COMMON/TEPROC/
     .UCLR(8),UCVR(8),UTLR,UTVR,
     .XLR(8),XVR(8),ETR,ESR,
     .TCR,TKR,DLR,
     .VLR,VVR,VTR,
     .PTR,PPR(8),
     .CRXR(8),RR(4),RH,
     .FWR,TWR,QUR,HWR,UAR,
     .UCLS(8),UCVS(8),UTLS,UTVS,
     .XLS(8),XVS(8),ETS,ESS,
     .TCS,TKS,DLS,
     .VLS,VVS,VTS,
     .PTS,PPS(8),
     .FWS,TWS,QUS,HWS,
     .UCLC(8),UTLC,XLC(8),
     .ETC,ESC,TCC,DLC,
     .VLC,VTC,QUC,
     .UCVV(8),UTVV,XVV(8),
     .ETV,ESV,TCV,TKV,
     .VTV,PTV,
     .VCV(12),VRNG(12),VTAU(12),
     .FTM(13),
     .FCM(8,13),XST(8,13),XMWS(13),
     .HST(13),TST(13),SFR(8),
     .CPFLMX,CPPRMX,CPDH,
     .TCWR,TCWS,
     .HTR(3),AGSP,
     .XDEL(41),XNS(41),
     .TGAS,TPROD,VST(12),IVST(12)
      INTEGER IDVWLK
      DOUBLE PRECISION
     .ADIST,
     .BDIST,
     .CDIST,
     .DDIST,
     .TLAST,
     .TNEXT,
     .HSPAN,
     .HZERO,
     .SSPAN,
     .SZERO,
     .SPSPAN
      COMMON/WLK/
     .ADIST(12),
     .BDIST(12),
     .CDIST(12),
     .DDIST(12),
     .TLAST(12),
     .TNEXT(12),
     .HSPAN(12),
     .HZERO(12),
     .SSPAN(12),
     .SZERO(12),
     .SPSPAN(12),
     .IDVWLK(12)
      DOUBLE PRECISION
     .AVP,BVP,CVP,
     .AH,BH,CH,
     .AG,BG,CG,
     .AV,
     .AD,BD,CD,
     .XMW
      COMMON/CONST/
     .AVP(8),BVP(8),CVP(8),
     .AH(8),BH(8),CH(8),
     .AG(8),BG(8),CG(8),
     .AV(8),
     .AD(8),BD(8),CD(8),
     .XMW(8)
      INTEGER I,NN
      DOUBLE PRECISION YY(NN),
     .YP(NN),
     .TIME
      XMW(1)=2.0
      XMW(2)=25.4
      XMW(3)=28.0
      XMW(4)=32.0
      XMW(5)=46.0
      XMW(6)=48.0
      XMW(7)=62.0
      XMW(8)=76.0
      AVP(1)=0.0
      AVP(2)=0.0
      AVP(3)=0.0
      AVP(4)=15.92
      AVP(5)=16.35
      AVP(6)=16.35
      AVP(7)=16.43
      AVP(8)=17.21
      BVP(1)=0.0
      BVP(2)=0.0
      BVP(3)=0.0
      BVP(4)=-1444.0
      BVP(5)=-2114.0
      BVP(6)=-2114.0
      BVP(7)=-2748.0
      BVP(8)=-3318.0
      CVP(1)=0.0
      CVP(2)=0.0
      CVP(3)=0.0
      CVP(4)=259.0
      CVP(5)=265.5
      CVP(6)=265.5
      CVP(7)=232.9
      CVP(8)=249.6
      AD(1)=1.0
      AD(2)=1.0
      AD(3)=1.0
      AD(4)=23.3
      AD(5)=33.9
      AD(6)=32.8
      AD(7)=49.9
      AD(8)=50.5
      BD(1)=0.0
      BD(2)=0.0
      BD(3)=0.0
      BD(4)=-0.0700
      BD(5)=-0.0957
      BD(6)=-0.0995
      BD(7)=-0.0191
      BD(8)=-0.0541
      CD(1)=0.0
      CD(2)=0.0
      CD(3)=0.0
      CD(4)=-0.0002
      CD(5)=-0.000152
      CD(6)=-0.000233
      CD(7)=-0.000425
      CD(8)=-0.000150
      AH(1)=1.0D-6
      AH(2)=1.0D-6
      AH(3)=1.0D-6
      AH(4)=0.960D-6
      AH(5)=0.573D-6
      AH(6)=0.652D-6
      AH(7)=0.515D-6
      AH(8)=0.471D-6
      BH(1)=0.0
      BH(2)=0.0
      BH(3)=0.0
      BH(4)=8.70D-9
      BH(5)=2.41D-9
      BH(6)=2.18D-9
      BH(7)=5.65D-10
      BH(8)=8.70D-10
      CH(1)=0.0
      CH(2)=0.0
      CH(3)=0.0
      CH(4)=4.81D-11
      CH(5)=1.82D-11
      CH(6)=1.94D-11
      CH(7)=3.82D-12
      CH(8)=2.62D-12
      AV(1)=1.0D-6
      AV(2)=1.0D-6
      AV(3)=1.0D-6
      AV(4)=86.7D-6
      AV(5)=160.D-6
      AV(6)=160.D-6
      AV(7)=225.D-6
      AV(8)=209.D-6
      AG(1)=3.411D-6
      AG(2)=0.3799D-6
      AG(3)=0.2491D-6
      AG(4)=0.3567D-6
      AG(5)=0.3463D-6
      AG(6)=0.3930D-6
      AG(7)=0.170D-6
      AG(8)=0.150D-6
      BG(1)=7.18D-10
      BG(2)=1.08D-9
      BG(3)=1.36D-11
      BG(4)=8.51D-10
      BG(5)=8.96D-10
      BG(6)=1.02D-9
      BG(7)=0.D0
      BG(8)=0.D0
      CG(1)=6.0D-13
      CG(2)=-3.98D-13
      CG(3)=-3.93D-14
      CG(4)=-3.12D-13
      CG(5)=-3.27D-13
      CG(6)=-3.12D-13
      CG(7)=0.D0
      CG(8)=0.D0
      YY(1)=10.40491389
      YY(2)=4.363996017
      YY(3)=7.570059737
      YY(4)=0.4230042431
      YY(5)=24.15513437
      YY(6)=2.942597645
      YY(7)=154.3770655
      YY(8)=159.1865960
      YY(9)=2.808522723
      YY(10)=63.75581199
      YY(11)=26.74026066
      YY(12)=46.38532432
      YY(13)=0.2464521543
      YY(14)=15.20484404
      YY(15)=1.852266172
      YY(16)=52.44639459
      YY(17)=41.20394008
      YY(18)=0.5699317760
      YY(19)=0.4306056376
      YY(20)=7.9906200783D-03
      YY(21)=0.9056036089
      YY(22)=1.6054258216D-02
      YY(23)=0.7509759687
      YY(24)=8.8582855955D-02
      YY(25)=48.27726193
      YY(26)=39.38459028
      YY(27)=0.3755297257
      YY(28)=107.7562698
      YY(29)=29.77250546
      YY(30)=88.32481135
      YY(31)=23.03929507
      YY(32)=62.85848794
      YY(33)=5.546318688
      YY(34)=11.92244772
      YY(35)=5.555448243
      YY(36)=0.9218489762
      YY(37)=94.59927549
      YY(38)=77.29698353
      YY(39)=63.05263039
      YY(40)=53.97970677
      YY(41)=24.64355755
      YY(42)=61.30192144
      YY(43)=22.21000000
      YY(44)=40.06374673
      YY(45)=38.10034370
      YY(46)=46.53415582
      YY(47)=47.44573456
      YY(48)=41.10581288
      YY(49)=18.11349055
      YY(50)=50.00000000
      DO 200 I=1,12
      XMV(I)=YY(I+38)
      VCV(I)=XMV(I)
      VST(I)=2.0D0
      IVST(I)=0
 200  CONTINUE
      VRNG(1)=400.00
      VRNG(2)=400.00
      VRNG(3)=100.00
      VRNG(4)=1500.00
      VRNG(7)=1500.00
      VRNG(8)=1000.00
      VRNG(9)=0.03
      VRNG(10)=1000.
      VRNG(11)=1200.0
      VTR=1300.0
      VTS=3500.0
      VTC=156.5
      VTV=5000.0
      HTR(1)=0.06899381054D0
      HTR(2)=0.05D0
      HWR=7060.
      HWS=11138.
      SFR(1)=0.99500
      SFR(2)=0.99100
      SFR(3)=0.99000
      SFR(4)=0.91600
      SFR(5)=0.93600
      SFR(6)=0.93800
      SFR(7)=5.80000D-02
      SFR(8)=3.01000D-02
      XST(1,1)=0.0
      XST(2,1)=0.0001
      XST(3,1)=0.0
      XST(4,1)=0.9999
      XST(5,1)=0.0
      XST(6,1)=0.0
      XST(7,1)=0.0
      XST(8,1)=0.0
      TST(1)=45.
      XST(1,2)=0.0
      XST(2,2)=0.0
      XST(3,2)=0.0
      XST(4,2)=0.0
      XST(5,2)=0.9999
      XST(6,2)=0.0001
      XST(7,2)=0.0
      XST(8,2)=0.0
      TST(2)=45.
      XST(1,3)=0.9999
      XST(2,3)=0.0001
      XST(3,3)=0.0
      XST(4,3)=0.0
      XST(5,3)=0.0
      XST(6,3)=0.0
      XST(7,3)=0.0
      XST(8,3)=0.0
      TST(3)=45.
      XST(1,4)=0.4850
      XST(2,4)=0.0050
      XST(3,4)=0.5100
      XST(4,4)=0.0
      XST(5,4)=0.0
      XST(6,4)=0.0
      XST(7,4)=0.0
      XST(8,4)=0.0
      TST(4)=45.
      CPFLMX=280275.
      CPPRMX=1.3
      VTAU(1)=8.
      VTAU(2)=8.
      VTAU(3)=6.
      VTAU(4)=9.
      VTAU(5)=7.
      VTAU(6)=5.
      VTAU(7)=5.
      VTAU(8)=5.
      VTAU(9)=120.
      VTAU(10)=5.
      VTAU(11)=5.
      VTAU(12)=5.
      DO 300 I=1,12
      VTAU(I)=VTAU(I)/3600.
 300  CONTINUE
       G=4651207995.D0
C	d00_tr_new: G=5687912315.D0       
C      original: G=1431655765.D0
C        d00_tr: G=4243534565.D0
C        d01_tr: G=7854912354.D0
C        d02_tr: G=3456432354.D0
C        d03_tr: G=1731738903.D0
C        d04_tr: G=4346024432.D0
C        d05_tr: G=5784921734.D0
C        d06_tr: G=6678322168.D0
C        d07_tr: G=7984782901.D0
C        d08_tr: G=8934302332.D0
C        d09_tr: G=9873223412.D0
C        d10_tr: G=1089278833.D0
C        d11_tr: G=1940284333.D0
C        d12_tr: G=2589274931.D0
C        d13_tr: G=3485834345.D0
C        d14_tr: G=4593493842.D0
C        d15_tr: G=5683213434.D0
C        d16_tr: G=6788343442.D0
C        d17_tr: G=1723234455.D0
C        d18_tr: G=8943243993.D0
C       dd18_tr: G=1234567890.D0

C        d19_tr: G=9445382439.D0
C        d20_tr: G=9902234324.D0
C        d21_tr: G=2144342545.D0
C        d22_tr: G=3433249064.D0
C        d23_tr: G=4356565463.D0
C        d24_tr: G=8998485332.D0
C        d25_tr: G=7654534567.D0
C        d26_tr: G=5457789234.D0
C
C        d00_te: G=1254545354.D0
C        d01_te: G=2994833239.D0
C        d02_te: G=2891123453.D0
C        d03_te: G=3420494299.D0
C        d04_te: G=4598956239.D0
C        d05_te: G=5658678765.D0
C        d06_te: G=6598593453.D0
C        d07_te: G=7327843434.D0
C        d08_te: G=8943242344.D0
C        d09_te: G=9343430004.D0
C        d10_te: G=1039839281.D0
C        d11_te: G=1134345551.D0
C        d12_te: G=2232323236.D0
C        d13_te: G=3454354353.D0
C        d14_te: G=4545445883.D0
C        d15_te: G=5849489384.D0
C        d16_te: G=6284545932.D0
C        d17_te: G=4342232344.D0
C        d18_te: G=5635346588.D0
C        d19_te: G=9090909232.DO
C        d20_te: G=8322308324.D0
C        d21_te: G=2132432423.D0
C        d22_te: G=5454589923.D0
C        d23_te: G=6923255678.D0
C        d24_te: G=8493323434.D0
C        d25_te: G=9338398429.D0
C        d26_te: G=1997072199.D0









      XNS(1)=0.0012D0
      XNS(2)=18.000D0
      XNS(3)=22.000D0
      XNS(4)=0.0500D0
      XNS(5)=0.2000D0
      XNS(6)=0.2100D0
      XNS(7)=0.3000D0
      XNS(8)=0.5000D0
      XNS(9)=0.0100D0
      XNS(10)=0.0017D0
      XNS(11)=0.0100D0
      XNS(12)=1.0000D0
      XNS(13)=0.3000D0
      XNS(14)=0.1250D0
      XNS(15)=1.0000D0
      XNS(16)=0.3000D0
      XNS(17)=0.1150D0
      XNS(18)=0.0100D0
      XNS(19)=1.1500D0
      XNS(20)=0.2000D0
      XNS(21)=0.0100D0
      XNS(22)=0.0100D0
      XNS(23)=0.250D0
      XNS(24)=0.100D0
      XNS(25)=0.250D0
      XNS(26)=0.100D0
      XNS(27)=0.250D0
      XNS(28)=0.025D0
      XNS(29)=0.250D0
      XNS(30)=0.100D0
      XNS(31)=0.250D0
      XNS(32)=0.100D0
      XNS(33)=0.250D0
      XNS(34)=0.025D0
      XNS(35)=0.050D0
      XNS(36)=0.050D0
      XNS(37)=0.010D0
      XNS(38)=0.010D0
      XNS(39)=0.010D0
      XNS(40)=0.500D0
      XNS(41)=0.500D0
      DO 500 I=1,20
      IDV(I)=0
 500  CONTINUE
      HSPAN(1)=0.2D0
      HZERO(1)=0.5D0
      SSPAN(1)=0.03D0
      SZERO(1)=0.485D0
      SPSPAN(1)=0.D0
      HSPAN(2)=0.7D0
      HZERO(2)=1.0D0
      SSPAN(2)=.003D0
      SZERO(2)=.005D0
      SPSPAN(2)=0.D0
      HSPAN(3)=0.25D0
      HZERO(3)=0.5D0
      SSPAN(3)=10.D0
      SZERO(3)=45.D0
      SPSPAN(3)=0.D0
      HSPAN(4)=0.7D0
      HZERO(4)=1.0D0
      SSPAN(4)=10.D0
      SZERO(4)=45.D0
      SPSPAN(4)=0.D0
      HSPAN(5)=0.15D0
      HZERO(5)=0.25D0
      SSPAN(5)=10.D0
      SZERO(5)=35.D0
      SPSPAN(5)=0.D0
      HSPAN(6)=0.15D0
      HZERO(6)=0.25D0
      SSPAN(6)=10.D0
      SZERO(6)=40.D0
      SPSPAN(6)=0.D0
      HSPAN(7)=1.D0
      HZERO(7)=2.D0
      SSPAN(7)=0.25D0
      SZERO(7)=1.0D0
      SPSPAN(7)=0.D0
      HSPAN(8)=1.D0
      HZERO(8)=2.D0
      SSPAN(8)=0.25D0
      SZERO(8)=1.0D0
      SPSPAN(8)=0.D0
      HSPAN(9)=0.4D0
      HZERO(9)=0.5D0
      SSPAN(9)=0.25D0
      SZERO(9)=0.0D0
      SPSPAN(9)=0.D0
      HSPAN(10)=1.5D0
      HZERO(10)=2.0D0
      SSPAN(10)=0.0D0
      SZERO(10)=0.0D0
      SPSPAN(10)=0.D0
      HSPAN(11)=2.0D0
      HZERO(11)=3.0D0
      SSPAN(11)=0.0D0
      SZERO(11)=0.0D0
      SPSPAN(11)=0.D0
      HSPAN(12)=1.5D0
      HZERO(12)=2.0D0
      SSPAN(12)=0.0D0
      SZERO(12)=0.0D0
      SPSPAN(12)=0.D0
      DO 550 I=1,12
      TLAST(I)=0.D0
      TNEXT(I)=0.1D0
      ADIST(I)=SZERO(I)
      BDIST(I)=0.D0
      CDIST(I)=0.D0
      DDIST(I)=0.D0
  550 CONTINUE
      TIME=0.0
      CALL TEFUNC(NN,TIME,YY,YP)
      RETURN
      END
C
C=============================================================================
C
      SUBROUTINE TESUB1(Z,T,H,ITY)
      DOUBLE PRECISION
     .AVP,BVP,CVP,
     .AH,BH,CH,
     .AG,BG,CG,
     .AV,
     .AD,BD,CD,
     .XMW
      COMMON/CONST/
     .AVP(8),BVP(8),CVP(8),
     .AH(8),BH(8),CH(8),
     .AG(8),BG(8),CG(8),
     .AV(8),
     .AD(8),BD(8),CD(8),
     .XMW(8)
      DOUBLE PRECISION Z(8),R,T,H,HI
      INTEGER ITY,I
      IF(ITY.EQ.0)THEN
      H=0.0D0
      DO 100 I=1,8
      HI=T*(AH(I)+BH(I)*T/2.D0+CH(I)*T**2/3.D0)
      HI=1.8D0*HI
      H=H+Z(I)*XMW(I)*HI
 100  CONTINUE
      ELSE
      H=0.0D0
      DO 200 I=1,8
      HI=T*(AG(I)+BG(I)*T/2.D0+
     .CG(I)*T**2/3.D0)
      HI=1.8D0*HI
      HI=HI+AV(I)
      H=H+Z(I)*XMW(I)*HI
 200  CONTINUE
      END IF
      IF(ITY.EQ.2)THEN
      R=3.57696D0/1.D6
      H=H-R*(T+273.15)
      ENDIF
      RETURN
      END
      SUBROUTINE TESUB2(Z,T,H,ITY)
      DOUBLE PRECISION
     .AVP,BVP,CVP,
     .AH,BH,CH,
     .AG,BG,CG,
     .AV,
     .AD,BD,CD,
     .XMW
      COMMON/CONST/
     .AVP(8),BVP(8),CVP(8),
     .AH(8),BH(8),CH(8),
     .AG(8),BG(8),CG(8),
     .AV(8),
     .AD(8),BD(8),CD(8),
     .XMW(8)
      INTEGER ITY,J
      DOUBLE PRECISION Z(8),T,H,TIN,HTEST,ERR,DH,DT
      TIN=T
      DO 250 J=1,100
      CALL TESUB1(Z,T,HTEST,ITY)
      ERR=HTEST-H
      CALL TESUB3(Z,T,DH,ITY)
      DT=-ERR/DH
      T=T+DT
 250  IF(DABS(DT).LT.1.D-12)GO TO 300
      T=TIN
 300  RETURN
      END
      SUBROUTINE TESUB3(Z,T,DH,ITY)
      DOUBLE PRECISION
     .AVP,BVP,CVP,
     .AH,BH,CH,
     .AG,BG,CG,
     .AV,
     .AD,BD,CD,
     .XMW
      COMMON/CONST/
     .AVP(8),BVP(8),CVP(8),
     .AH(8),BH(8),CH(8),
     .AG(8),BG(8),CG(8),
     .AV(8),
     .AD(8),BD(8),CD(8),
     .XMW(8)
      INTEGER ITY,I
      DOUBLE PRECISION Z(8),R,T,DH,DHI
      IF(ITY.EQ.0)THEN
      DH=0.0D0
      DO 100 I=1,8
      DHI=AH(I)+BH(I)*T+CH(I)*T**2
      DHI=1.8D0*DHI
      DH=DH+Z(I)*XMW(I)*DHI
 100  CONTINUE
      ELSE
      DH=0.0D0
      DO 200 I=1,8
      DHI=AG(I)+BG(I)*T+CG(I)*T**2
      DHI=1.8D0*DHI
      DH=DH+Z(I)*XMW(I)*DHI
 200  CONTINUE
      END IF
      IF(ITY.EQ.2)THEN
      R=3.57696D0/1.D6
      DH=DH-R
      ENDIF
      RETURN
      END
      SUBROUTINE TESUB4(X,T,R)
      DOUBLE PRECISION
     .AVP,BVP,CVP,
     .AH,BH,CH,
     .AG,BG,CG,
     .AV,
     .AD,BD,CD,
     .XMW
      COMMON/CONST/
     .AVP(8),BVP(8),CVP(8),
     .AH(8),BH(8),CH(8),
     .AG(8),BG(8),CG(8),
     .AV(8),
     .AD(8),BD(8),CD(8),
     .XMW(8)
      DOUBLE PRECISION V,R,X(8),T
      INTEGER I
      V=0.0
      DO 10 I=1,8
      V=V+X(I)*XMW(I)/
     .(AD(I)+(BD(I)+CD(I)*T)*T)
   10 CONTINUE
      R=1.0/V
      RETURN
      END
      SUBROUTINE TESUB5(S,SP,ADIST,BDIST,CDIST,DDIST,TLAST,
     .TNEXT,HSPAN,HZERO,SSPAN,SZERO,SPSPAN,IDVFLAG)
      DOUBLE PRECISION
     .S,
     .SP,
     .H,
     .S1,
     .S1P,
     .ADIST,
     .BDIST,
     .CDIST,
     .DDIST,
     .TLAST,
     .TNEXT,
     .HSPAN,
     .HZERO,
     .SSPAN,
     .SZERO,
     .SPSPAN,
     .TESUB7
      INTEGER  I,IDVFLAG
      I=-1
      H=HSPAN*TESUB7(I)+HZERO
      S1=SSPAN*TESUB7(I)*IDVFLAG+SZERO
      S1P=SPSPAN*TESUB7(I)*IDVFLAG
      ADIST=S
      BDIST=SP
      CDIST=(3.D0*(S1-S)-H*(S1P+2.D0*SP))/H**2
      DDIST=(2.D0*(S-S1)+H*(S1P+SP))/H**3
      TNEXT=TLAST+H
      RETURN
      END
      SUBROUTINE TESUB6(STD,X)
      INTEGER I
      DOUBLE PRECISION STD,X,TESUB7
      X=0.D0
      DO 2 I=1,12
    2 X=X+TESUB7(I)
      X=(X-6.D0)*STD
      RETURN
      END
      DOUBLE PRECISION FUNCTION TESUB7(I)
      INTEGER I
      DOUBLE PRECISION G,DMOD
      COMMON/RANDSD/G
      G=DMOD(G*9228907.D0,4294967296.D0)
      IF(I.GE.0)TESUB7=G/4294967296.D0
      IF(I.LT.0)TESUB7=2.D0*G/4294967296.D0-1.D0
      RETURN
      END
      DOUBLE PRECISION FUNCTION TESUB8(I,T)
      INTEGER  I
      DOUBLE PRECISION  H,T
      INTEGER IDVWLK
      DOUBLE PRECISION
     .ADIST,
     .BDIST,
     .CDIST,
     .DDIST,
     .TLAST,
     .TNEXT,
     .HSPAN,
     .HZERO,
     .SSPAN,
     .SZERO,
     .SPSPAN
      COMMON/WLK/
     .ADIST(12),
     .BDIST(12),
     .CDIST(12),
     .DDIST(12),
     .TLAST(12),
     .TNEXT(12),
     .HSPAN(12),
     .HZERO(12),
     .SSPAN(12),
     .SZERO(12),
     .SPSPAN(12),
     .IDVWLK(12)
      H=T-TLAST(I)
      TESUB8=ADIST(I)+H*(BDIST(I)+H*(CDIST(I)+H*DDIST(I)))
      RETURN
      END

