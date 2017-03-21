C----------------------------------------------------------------------
C                      ** ERZSOL3 **
C
C         CALCULATION OF SYNTHETIC SEISMOGRAM FOR
C         A GENERAL POINT SOURCE SPECIFIED BY AN ARITRARY
C         MOMENT TENSOR BURIED IN A WHOLE OR HALF SPACE
C         COMPOSED OF A STACK OF SOLID UNIFORM LAYERS
C
C         The calculation scheme is described and illustrated in :
C
C         B.L.N. KENNETT  1980  Seismic waves in a stratified
C         half space  II - Theoretical seismograms
C         Geophys J R astr Soc  61, 1-10
C
C
C         The program is designed to run in three steps
C         1)   Construction of response in Transform domain
C         2)   Integration step
C         3)   Plotting step (external)
C
C
C++++++  INPUT from command file
C
C
C  "SOLTEST-dr2  "                         Title
C  "dr2.tx.z"                              File for T-X seismogram output
C  "dr2.mod"                               Velocity model file
C  "HS"                                    Surface Condition (HS,H1,WF,WS)
C    10                                    Number of slownesses (<1200)
C    0.0001                                Minimum slowness
C    0.10                                  Maximum slowness
C  0                                       Slowness taper plo (n samples)
C  0                                       Slowness taper phi (n samples)
C  "WA"                                    Wavelet input or Ricker (WA/RI)
C  "dr2.wav"                               Wavelet file
C  "YE"                                    Exponential damping? (YE/NO)
C    2048                                  Number of time points
C    0.004                                 Time step
C     5.0   10.0                           Frequency taper (low)
C    60.0   75.0                           Frequency taper (high)
C     25.0                                 Dominant frequency      [RI]
C     0.00      0.00       0.00            Moment tensor Components
C     0.00      0.00       0.00
C     0.00      0.00       1.00
C  "dr2.dst"                               Range and azimuth file
C    0.0                                   Reduction slowness
C    0.000                                 Start time (reduced)
C  "NO"                                    Debug/frequency-wavenumber (YE/NO)
C  "NO"                                    Debug/waveform (YE/NO)
C   
C NOTE:
C------
C  The seismic wave field is evaluated at a set of stations
C  with specified ranges and azimuths
C  Three-component seismograms are produced in ZST format
C
C VARIABLES : 
C----------
C   
C  The action of the program is controlled by an internal
C  set of 15 switches and parameters - ISW
C                                                                  Aliases
C
C           ISW(1)      -      NUMBER OF LAYERS                        NL
C
C           ISW(2)      -      NUMBER OF POINTS IN WAVEFORM           NDT
C
C           ISW(3)      -      NUMBER OF STATIONS                     NPX
C
C           ISW(4)     =  0      NEAR AND FAR FIELD TERMS             NNF
C                      =  1      FAR FIELD (Asymptotic forms of
C                                  Bessel Functions)
C
C           ISW(5)     =  0      EXPONENTIAL DAMPING IN TIME          NRS
C                      =  1      NO DAMPING IN TIME
C
C           ISW(6)     =  0      HALF SPACE - FREE SURFACE       (HS) NFS
C                      =  1      HALF SPACE - ONLY FIRST ORDER   (H1)
C                                             SURFACE REFLECTIONS
C                      =  2      WHOLE SPACE -DISPLACEMENT AS AT (WF)
C                                             FREE SURFACE
C                      =  3      WHOLE SPACE                     (WS)
C
C           ISW(7)     =  0      FULL ELASTIC CALCULATION
C
C           ISW(8)     =  n      Taper over n slownesses at lower
C                                end of slowness window
C
C           ISW(9)     =  n      Taper over n slownesses at upper
C                                end of slowness window
C
C           ISW(10)    =  2      T-X  OUTPUT                    (TX)
C
C           ISW(14)    =  0      NO FREQUENCY/DEBUG OUTPUT
C                      =  1      FREQUENCY/DEBUG OUTPUT
C
C           ISW(15)    =  0      NO WAVEFORM OUTPUT
C                      =  1      WAVEFORM OUTPUT
C
C           ISW(12) - ISW(13)   FREE
C
C
C*       TIMING
C             NT - TOTAL NUMBER OF TIME POINTS IN SERIES
C             NTI - INDEX (NT = 2**NTI)
C             DELT - TIME STEP IN TIME SERIES
C
C*       SOURCE DEPTH
C             JS - INDEX OF SOURCE LAYER
C             HSS - DEPTH OF SOURCE (from surface)
C        (it is important that these source parameters are consistent)
C
C*       MOMENT TENSOR COMPONENTS OF SOURCE IN ORDER:
C             MXX, MXY, MXZ
C             MYX, MYY, MYZ
C             MZX, MZY, MZZ
C           For an explosive pressure source specify
C           MXX=MYY=MZZ= 1.0 ,  all others zero
C
C        FREQUENCY WINDOW
C             FL1 - LOWER LIMIT OF FREQUENCY WINDOW
C             FL2 - COSINE TAPER UP FROM FL1 TO FL2
C             FU1 - COSINE TAPER DOWN FROM FU1 TO FU2
C             FU2 - UPPER LIMIT OF FREQUENCY WINDOW
C
C        SLOWNESS WINDOW
C             NRP - NUMBER OF RAY PARAMETERS CONSIDERED
C                    (currently limited by array size to 1200)
C             PMN - MINIMUM SLOWNESS
C             PMX - MAXIMUM SLOWNESS
C
C        STATION POSITIONS (NPX VALUES)
C            XS(J) - RANGE IN KM
C            AZ(J) - AZIMUTH FROM NORTH (X-axis)
C
C        WAVEFORM
C            F - WAVEFORM TIME SERIES (NDT POINTS)
C            <VALUES ENTERED ARE COMPLEX IE TREAT AS 4(F10.5,10X)>
C
C        REDUCED TIME INFORMATION
C            PR - REDUCTION SLOWNESS
C            STMIN - STARTING REDUCED TIME
C
C        VELOCITY MODEL (read in from specified file)
C         For each layer we have to specify
C             NR    -  RAY INDEX
C             ALF   -  P WAVE VELOCITY
C             BET   -  S WAVE VELOCITY
C             RHO   -  DENSITY
C             HL    -  LAYER THICKNESS
C             QAI   -  1.0/QA (ATTENUATION FOR P)
C             QBI   -  1.0/QB (ATTENUATION FOR S)
C
C        IF QAI,QBI, ARE LEFT UNSET THE MEDIUM IS TREATED AS PERFECTLY
C        ELASTIC  (Unwise if integration is to be carried into the region of
C        surface wave poles  P .GT. (beta (max)) ** -1  )
C
C      The ray index NR controls the way in which the layer is treated
C      when the reflection and transmission properties are built up
C           NR   =   0  TRANSMISSION THROUGH  LAYER
C                =   1  PRIMARY PATH IN LAYER
C                       (excludes all internal multiples in layer)
C                =   2  SOME INTERNAL REFLECTIONS IN LAYER
C                =   3  ALL POSSIBLE REFLECTIONS IN LAYER
C
C        The possible ray paths are compounded when the reflectivity
C        of the layering is calculated by a recursive scheme in
C        which the effect of a layer is added at the top of the
C        stack - it is therefore difficult to predict exactly which
C        internal reflections will be present if NR=2 is used.
C        Generally NR= 1 or 3 are most useful.
C
C----------------------------------------------------------------------
C*************
C
C       B.L.N. KENNETT
C       CAMBRIDGE UNIVERSITY       FEBRUARY 1983
C       RSES ANU                   JULY 1987/MARCH 1991
C                                  OCTOBER 1999
C
C       A.P. PURCELL
C       RSES ANU                   DECEMBER 1997
C*************
C**
      COMMON F(4096),GU(4096),GV(4096),GW(4096),WR(2500)
      COMMON /PRESS/ RU(5,2500,600),RV(5,2500,600),RW(5,2500,600)
      COMMON /STRUCT/
     &     ALSQ(500),BESQ(500),
     &     RHO(500),HL(500),NR(500)
      COMMON /PARE/
     &     DELT,DELX,DELF,FL1,FL2,FU1,FU2,
     &     ISW(15),NT,NTI
      COMMON /INTEG/
     &     PG(2500),DP(2500),PMX,PMN,NRP
      COMMON /SPOT/
     &     XS(100),AZ(100),PR,STMIN,NPX
      COMPLEX*16    ALSQ,BESQ,CQA,CQB
      COMPLEX       F,RU,RV,RW,GU,GV,GW
      REAL          WR
C**
      COMMON /SOURCE/
     #     SOURC(15),ALFSSQ,BETSSQ,QAS,QBS,YAS,
     #     CMA,CMB,CMC,DMC,CMD,CME,CMF,DMF,CMG,DMG,
     #     RHOS,TMXX,TMXY,TMXZ,TMYX,TMYY,TMYZ,TMZX,TMZY,TMZZ,
     #     HAS,HBS,HAR,HBR,ERX,ESX,JS,NRX,NSX
      COMPLEX*16
     #     SOURC,ALFSSQ,BETSSQ,QAS,QBS,YAS,
     #     PU,SU,HU
      COMPLEX
     #     CMA,CMB,CMC,DMC,CMD,CME,CMF,DMF,CMG,DMG
      DIMENSION PU(5),SU(5),HU(5)
      EQUIVALENCE (SOURC(1),PU(1)),(SOURC(6),SU(1)),(SOURC(11),HU(1))
C**
      COMMON /COEFF/
     %     HP(500),HS(500),
     %     RDPPR(500),RDPSR(500),RDSPR(500),RDSSR(500),
     %     RDPPI(500),RDPSI(500),RDSPI(500),RDSSI(500),
     %     RUPPR(500),RUPSR(500),RUSPR(500),RUSSR(500),
     %     RUPPI(500),RUPSI(500),RUSPI(500),RUSSI(500),
     %     TDPPR(500),TDPSR(500),TDSPR(500),TDSSR(500),
     %     TDPPI(500),TDPSI(500),TDSPI(500),TDSSI(500),
     %     TUPPR(500),TUPSR(500),TUSPR(500),TUSSR(500),
     %     TUPPI(500),TUPSI(500),TUSPI(500),TUSSI(500),
     %     RDHHR(500),RDHHI(500),RUHHR(500),RUHHI(500),
     %     TDHHR(500),TDHHI(500),TUHHR(500),TUHHI(500)
      COMPLEX*16
     %     HP,HS
      REAL*8
     %     RDPPR,RDPSR,RDSPR,RDSSR, RDPPI,RDPSI,RDSPI,RDSSI,
     %     RUPPR,RUPSR,RUSPR,RUSSR, RUPPI,RUPSI,RUSPI,RUSSI,
     %     TDPPR,TDPSR,TDSPR,TDSSR, TDPPI,TDPSI,TDSPI,TDSSI,
     %     TUPPR,TUPSR,TUSPR,TUSSR, TUPPI,TUPSI,TUSPI,TUSSI,
     %     RDHHR,RDHHI,RUHHR,RUHHI, TDHHR,TDHHI,TUHHR,TUHHI
      COMMON /STACK/
     %     RPP,RPS,RSP,RSS,RHH,TPP,TPS,TSP,TSS,THH,
     %     P,PQ,QAJF,QBJF,QMJF,AMUF,W
      COMPLEX*16
     %     RPP,RPS,RSP,RSS,RHH,TPP,TPS,TSP,TSS,THH,
     %     P,PQ,QAJF,QBJF,QMJF,AMUF,W
C**
      COMPLEX CZERO,YI
      character*30 velfil,txsfil,rtitle,rangfil,wavfil
      character*24 cstring
      character*2 iexp,ifs,idf,idw,itxp
      character*2 iwav
      real fwav(512),rif, degrad
C++
      CZERO = (0.0,0.0)
      YI = (0.0,1.0)
      HSR = 0.0
      degrad = atan(1.0)/45.0
C                                           INPUT OF PARAMETERS
      READ(5,*) rtitle
C
      READ(5,*) txsfil
      ISW(10) = 2
      open(11,file=txsfil,form='unformatted')
      open(12,file='Zdat')
      open(312,file='Zhead')
      open(13,file='Rdat')
      open(313,file='Rhead')
      open(14,file='Tdat')
      open(314,file='Thead')
C                                   Control parameters
      ISW(4) = 0
      ISW(11) = 0
      ISW(7) = 0
C
      READ(5,*) velfil
      open(7,file=velfil)
C
      READ(5,*) ifs
      IF(ifs.eq.'HS') then
        ISW(6) = 0
      ELSEIF(ifs.eq.'H1') then
        ISW(6) = 1
      ELSEIF(ifs.eq.'WF') then
        ISW(6) = 2
      ELSEIF(ifs.eq.'WS') then
        ISW(6) = 3
      ELSE
        ISW(6) = 0
      ENDIF
C                                    Slowness parameters
      READ(5,*) NRP
      READ(5,*) PMN
      READ(5,*) PMX
C
      READ(5,*) ISW(8)
      READ(5,*) ISW(9)
C
C*                                       Time,filter,source waveform
      READ(5,*) iwav
      READ(5,*) wavfil
C                                        Exponential Damping
      READ(5,*) iexp
      IF(iexp.eq.'YE') then
        ISW(5) = 0
      ELSE 
        ISW(5) = 1
      ENDIF
      NRS = ISW(5)+1
C                                    Timing
      READ(5,*) NT
      NTI = INT(ALOG(FLOAT(NT))/ALOG(2.0))
      READ(5,*) DELT
      DELF = 1.0/(DELT*NT)
C
C                                    Frequency Filter settings
      READ(5,*) FL1,FL2
      READ(5,*) FU1,FU2
C                                          dominant frequency (Ricker)
      READ(5,*) FDOM
C
      READ(5,*) TMXX,TMXY,TMXZ
      READ(5,*) TMYX,TMYY,TMYZ
      READ(5,*) TMZX,TMZY,TMZZ
C                                      - combinations of moment tensor
C                                        components which appear in the
C                                        excitation of P,SV and SH waves
      CMA = 0.5*(TMXX+TMYY-2.0*TMZZ)
      CMB = 0.5*YI*(TMXY-TMYX)
      CMC =  TMXZ-YI*TMYZ
      DMC = -TMXZ-YI*TMYZ
      CMD = 0.5*YI*(TMZY-TMYZ)
      CME = 0.5*(TMXZ-TMZX)
      CMF = 0.5*(-TMYZ-YI*TMXZ)
      DMF = 0.5*( TMYZ-YI*TMXZ)
      CMG = 0.25*(-(TMXX-TMYY)+YI*(TMXY+TMYX))
      DMG = 0.25*( (TMXX-TMYY)+YI*(TMXY+TMYX))
C
C                                    Source Depth
      JS = 2
      READ(5,*) HSS
C                                    Distances and azimuths
      READ(5,*) rangfil
C
      open(17,file=rangfil,form='formatted')
        read(17,*) NPX
        read(17,*) (XS(JX),AZ(JX),JX=1,NPX)
      close(17)
C
C                                            Calculate integration parameters
      CMX = 999.999
      IF(PMN.GT.0.0) CMX = 1.0/PMN
      CMN = 1.0/PMX
      RP = FLOAT(NRP)
      DPC = (PMX-PMN)/RP
      IF (PMN.EQ.0.0) PMN=0.0001
      PG(1) = PMN
      PC = PMN+DPC
      IF (ISW(10).NE.3) then
        Q0 = ASIN(PMN-0.2)
        Q1 = ASIN(PMX-0.2)
        QMN = 0.5*Q0 + 0.25*SIN(2.0*Q0)
        QMX = 0.5*Q1 + 0.25*SIN(2.0*Q1)
        PQR = (PMX-PMN)/(QMX-QMN)
        DP(1) = DPC*PQR*SQRT(1.0 - (PMN-0.2)**2)
        DO 70 MK=2,NRP
          Q = ASIN(PC-0.2)
          QP = 0.5*Q + 0.25*SIN(2.0*Q)
          PG(MK) = PMN + (QP-QMN)*PQR
          DP(MK) = PG(MK)-PG(MK-1)
          PC = PC+DPC
 70     CONTINUE
      ELSEIF(ISW(10).EQ.3) then
        DP(1) = DPC
        DO 71 MK=2,NRP
          PG(MK) = PC
          DP(MK) = PG(MK)-PG(MK-1)
          PC = PC+DPC
 71     CONTINUE
      ENDIF
C
      PI = 4.0*ATAN(1.0)
      FD = FU2-FL1
      FR = 0.0
      IF(iwav.eq.'WA') then
        open(16,file=wavfil,form='formatted')
        READ(16,*) NDT
        READ(16,*) (fwav(L),L=1,NDT)
        CLOSE(16)
        do 73 L=1,NDT
          F(L) = cmplx(fwav(L),0.)
 73     continue
        ISW(2) = NDT
        NDA = NDT+1
        DO 74 L=NDA,NT
          F(L) = CZERO
 74     CONTINUE
        CALL FCOOLR(NTI,F,1.0)
        FD = FU2-FL1
        FR = 0.0
        DO 75 L=1,NT
          IF(FR.LT.FL1) F(L)= CZERO
          IF(FR.LT.FL2) F(L) = F(L)*SIN(0.5*PI*(FR-FL1)/(FL2-FL1))
          IF(FR.GT.FU1) F(L) = F(L)*COS(0.5*PI*(FR-FU1)/(FU2-FU1))
          IF(FR.GT.FU2) F(L) = CZERO
          FR = FR+DELF
 75     CONTINUE
        IF(FL1.EQ.0.0) FL1 = DELF
      ELSEIF(iwav.eq.'RI') then
        DO 76 L=1,NT
          RIF = 2.*FR**2*EXP(-FR**2/FDOM**2)/SQRT(PI)/FDOM**3
          F(L) = cmplx(RIF,0.)
          IF(FR.LT.FL1) F(L) = CZERO
          IF(FR.GT.FU2) F(L) = CZERO
          FR = FR+DELF
 76     CONTINUE
      ENDIF
C
C                                    Reduced Time window
      READ(5,*) PR
      READ(5,*) STMIN
C                                   Debug options
      READ(5,*) idf
      IF(idf.eq.'YE') then
        ISW(14) = 1
      ELSE
        ISW(14) = 0
      ENDIF
C
      READ(5,*) idw
      IF(idw.eq.'YE') then
        ISW(15) = 1
      ELSE
        ISW(15) = 0
      ENDIF
C                                           VELOCITY DISTRIBUTION
C                                           INPUT AND OUTPUT
      WRITE(6,120) rtitle
      WRITE(6,110)
C                                    Read velocity file
      READ(7,*) cstring
      READ(7,*) nla,ndt
      NL = nla+1
      ISW(1) = NL
      DO 20 J=2,NL
        READ(7, * )  NR(J),ALF,BET,RHO(J),HL(J),QAI,QBI
        if(j.eq.2) then
          NR(1) = NR(2)
          RHO(1) = RHO(2)
          HL(1) = 0.0
 	  WRITE(6,111) NR(1),ALF,BET,RHO(1),HL(1),QAI,QBI
   	endif
        WRITE(6,111) NR(J),ALF,BET,RHO(J),HL(J),QAI,QBI
        CQA = 1.0-0.5*YI*QAI
       	CQB = 1.0-0.5*YI*QBI
        ALSQ(J) = ALF*ALF*CQA*CQA
        BESQ(J) = BET*BET*CQB*CQB
 20   CONTINUE
      ALSQ(1) = ALSQ(2)
      BESQ(1) = BESQ(2)
C                                           Source level parameters
      Z1 = 0.0
      DO 21 J=1,NL
        Z2 = Z1+HL(J)
        IF(HSS.GE.Z1 .AND. HSS.LE.Z2) THEN
          JS = J
          HAS = HSS-Z1
          HBS = Z2-HSS
        ENDIF
        Z1 = Z2
 21   CONTINUE
      ALFSSQ = ALSQ(JS)
      BETSSQ = BESQ(JS)
      RHOS = RHO(JS)
      NSA = NL-JS
      JSA = JS-1
C                                           PARAMETER OUTPUT
      WRITE(6,113) (ISW(J),J=1,15)
      WRITE(6,114) NT,NTI,DELT
      WRITE(6,130) JS,HSS,NSA,HBS,JSA,HAS
      WRITE(6,131) TMXX,TMXY,TMXZ,TMYX,TMYY,TMYZ,TMZX,TMZY,TMZZ
c     write(6,*) CMA,CMG,DMG
      WRITE(6,116) FL1,FL2,FU1,FU2
      WRITE(6,117) DELF
      WRITE(6,122) NRP,CMX,CMN
      WRITE(6,123) PMN,PMX
      WRITE(6,118) PR,STMIN
c     write(6,*) NDA,NDT
      WRITE(6,260) (F(L),L=1,NDT)
      WRITE(6,261)
      WRITE(6,262) (XS(JX),AZ(JX),JX=1,NPX)
C
C     Azimuths to radians
      DO JX=1,NPX
        AZ(JX) = AZ(JX)*degrad
      ENDDO      
C
C                                   CALCULATE MEDIUM RESPONSE
      CALL RESPON
      WRITE(6,137)
C
C                                   INTEGRATION
      IF(ISW(10).LE.2) CALL SEISB
      WRITE(6,138)
C
 110       FORMAT(/,1X,'LAYER STRUCTURE',/,
     &3X,' NR     ALPHA     BETA       RHO      THICK    QAI       QBI'
     &,/,3X,' NR     KM/S      KM/S      MG/M3      KM  ',/)
 111       FORMAT(1X,I4,4F10.3,2F10.5)
 113       FORMAT(1X,/,1X,'SWITCHES',/,1X,5I6,/,1X,5I6,/,1X,5I6,/)
 114       FORMAT(4X,'NT:',I10,5X,'NTI:',I10,4X,'DELT:',F10.5)
 116       FORMAT(1X,'FREQUENCY WINDOW',4F10.3,'  HZ')
 117       FORMAT(2X,'DELF:',F10.4)
 118       FORMAT(1X,/,1X,'PR:',F10.2,3X,'STMIN:',F10.2)
 120       FORMAT(1H1,/,1X,'**  ERZSOL3 **',//,
     1     1X,'SYNTHETIC SEISMOGRAMS FROM A GENERAL POINT SOURCE',/,
     2     1X,'AT DEPTH IN A MULTILAYERED ELASTIC MEDIUM ',/,
     3     1X,'WITH ALLOWANCE FOR ATTENUATION AND CHOICE OF ',/,
     4     1X,'INTERNAL MULTIPLES',/,
     5     1X,'RUN :',A,/)
 122       FORMAT(1X,'PHASE VELOCITY WINDOW',/
     1          3X,'NRP:',I10,5X,'CMX:',F10.5,5X,'CMN:',F10.5,/)
 123       FORMAT(1X,'INTEGRATION PARAMETERS',/
     1          4X,'PMN:',F10.5,5X,'PMX:',F10.5,/)
 130       FORMAT(1X,'SOURCE PARAMETERS:',/
     1          3X,' JS:',I10,5X,'HSS:',F10.3,4X,/,
     2          1X,'BELOW SOURCE : RDSL',/,
     3          3X,'NSA:',I10,5X,'HBS:',F10.3,/,
     4          1X,'ABOVE SOURCE : RUFS',/,
     5          3X,'JSA:',I10,5X,'HAS:',F10.3,/)
 131       FORMAT(1X,//,11X,'MOMENT TENSOR:',/
     1          11X,3F15.4,/11X,3F15.4,/11X,3F15.4,/)
 137       FORMAT(//,1X,'** TRANSFORMS CALCULATED **'/)
 138       FORMAT(//,1X,'** INTEGRATION COMPLETED **'/)
 261       FORMAT(1X,/,1X,'STATIONS:',/,1X,'RANGE',6X,'AZIMUTH')
 262       FORMAT(1X,2F10.3)
 260       FORMAT(1X,/,1X,'WAVEFORM:',/,1X,8F10.5)
C
 99   STOP
      END

      SUBROUTINE RESPON
C-----------------------------------------------------------
C     **  ZERT  ** :  SUBROUTINE RESPON
C                      CALCULATES THE FREQUENCY SPECTRUM OF
C                      SURFACE DISPLACEMENT GENERATED BY
C                      AN ARBITRARY POINT SOURCE IN A LAYERED
C                      ELASTIC HALF SPACE
C
C          W    -    2.0*PI*FREQUENCY
C          P    -    HORIZONTAL SLOWNESS (RAY PARAMETER)
C          QA.  -    VERTICAL SLOWNESS FOR  P  WAVES
C          QB.  -    VERTICAL SLOWNESS FOR  S  WAVES
C          AMUF -    SURFACE RIGIDITY
C
C        REFLECTION COEFFICIENTS FROM BELOW SOURCE
C          RDSLPP,RDSLPS,RDSLSP,RDSLSS,RDSLHH
C        REFLECTION COEFFICIENTS FROM ABOVE SOURCE
C          RUFSPP,RUFSPS,RUFSSP,RUFSSS,RUFSHH
C       DISPLACEMENT TRANSFER COEFFICIENTS TO SURFACE FROM
C       UPGOING WAVE AT SOURCE
C          YMFSUP,YMFSUS,YMFSVP,YMFSVS,YMFSWH
C       where   YMFS = (MUF + MDF*RF)*( I - RDRS*RF)**-1 *TURS
C       in terms of displacement transforms MU,MD
C       and surface reflection matrix RF
C
C-------------------------------------------------------------
C**
C     B.L.N. KENNETT      FEBRUARY 1979
C**
      COMMON F(4096),GU(4096),GV(4096),GW(4096),WR(2500)
      COMMON /PRESS/ RU(5,2500,600),RV(5,2500,600),RW(5,2500,600)
      COMMON /STRUCT/
     &     ALSQ(500),BESQ(500),
     &     RHO(500),HL(500),NR(500)
      COMMON /PARE/
     &     DELT,DELX,DELF,FL1,FL2,FU1,FU2,
     &     ISW(15),NT,NTI
      COMMON /INTEG/
     &     PG(2500),DP(2500),PMX,PMN,NRP
      COMMON /SPOT/
     &     XS(100),AZ(100),PR,STMIN,NPX
      COMPLEX*16    ALSQ,BESQ,CQA,CQB
      COMPLEX       F,RU,RV,RW,GU,GV,GW
      REAL          WR
C**
      COMMON /SOURCE/
     #     SOURC(15),ALFSSQ,BETSSQ,QAS,QBS,YAS,
     #     CMA,CMB,CMC,DMC,CMD,CME,CMF,DMF,CMG,DMG,
     #     RHOS,TMXX,TMXY,TMXZ,TMYX,TMYY,TMYZ,TMZX,TMZY,TMZZ,
     #     HAS,HBS,HAR,HBR,ERX,ESX,JS,NRX,NSX
      COMPLEX*16
     #     SOURC,ALFSSQ,BETSSQ,QAS,QBS,YAS,
     #     PU,SU,HU,PD,SD,HD
      COMPLEX
     #     CMA,CMB,CMC,DMC,CMD,CME,CMF,DMF,CMG,DMG
      DIMENSION PU(5),SU(5),HU(5)
      EQUIVALENCE (SOURC(1),PU(1)),(SOURC(6),SU(1)),(SOURC(11),HU(1))
C**
      COMMON /COEFF/
     %     HP(500),HS(500),
     %     RDPPR(500),RDPSR(500),RDSPR(500),RDSSR(500),
     %     RDPPI(500),RDPSI(500),RDSPI(500),RDSSI(500),
     %     RUPPR(500),RUPSR(500),RUSPR(500),RUSSR(500),
     %     RUPPI(500),RUPSI(500),RUSPI(500),RUSSI(500),
     %     TDPPR(500),TDPSR(500),TDSPR(500),TDSSR(500),
     %     TDPPI(500),TDPSI(500),TDSPI(500),TDSSI(500),
     %     TUPPR(500),TUPSR(500),TUSPR(500),TUSSR(500),
     %     TUPPI(500),TUPSI(500),TUSPI(500),TUSSI(500),
     %     RDHHR(500),RDHHI(500),RUHHR(500),RUHHI(500),
     %     TDHHR(500),TDHHI(500),TUHHR(500),TUHHI(500)
      COMPLEX*16
     %     HP,HS
      REAL*8
     %     RDPPR,RDPSR,RDSPR,RDSSR, RDPPI,RDPSI,RDSPI,RDSSI,
     %     RUPPR,RUPSR,RUSPR,RUSSR, RUPPI,RUPSI,RUSPI,RUSSI,
     %     TDPPR,TDPSR,TDSPR,TDSSR, TDPPI,TDPSI,TDSPI,TDSSI,
     %     TUPPR,TUPSR,TUSPR,TUSSR, TUPPI,TUPSI,TUSPI,TUSSI,
     %     RDHHR,RDHHI,RUHHR,RUHHI, TDHHR,TDHHI,TUHHR,TUHHI
      COMMON /STACK/
     %     RPP,RPS,RSP,RSS,RHH,YUP,YUS,YVP,YVS,YWH,
     %     P,PQ,QAJF,QBJF,QMJF,AMUF,W
      COMPLEX*16
     %     RPP,RPS,RSP,RSS,RHH,YUP,YUS,YVP,YVS,YWH,
     %     P,PQ,QAJF,QBJF,QMJF,AMUF,W
C**
      COMPLEX YI
      COMPLEX*16
     #  RP,RS,RH,YIW,DET,
     #  AIJS,BIJS,PQA,PQB,YA,PHPP,PHPS,PHSS,
     #  UPP,UPS,USP,USS,UHH,
     #  WPP,WPS,WSP,WSS,WHH,
     #  ZUP,ZUS,ZVP,ZVS,ZWH,
     #  RDSLPP,RDSLSP,RDSLPS,RDSLSS,RDSLHH,
     #  RUFSPP,RUFSPS,RUFSSP,RUFSSS,RUFSHH,
     #  YMFSUP,YMFSUS,YMFSVP,YMFSVS,YMFSWH
      DIMENSION XL(5)
      DATA XL/-1.0,1.0,1.0,-1.0,-1.0/
      REAL*8 QQA(2),QQR,QQI
      COMPLEX*16 QQC,CDZERO,CDONE
      EQUIVALENCE (QQC,QQA(1),QQR),(QQA(2),QQI)
C**
      CDONE = (1.0D0,0.0D0)
      CDZERO = (0.0D0,0.0D0)
      YI = (0.0,1.0)
C
      NL  = ISW(1)
      NPW = ISW(3)
      NFS = ISW(6)+1
      NWF = ISW(14)
      EPS = 4.0*DELF
      IF (ISW(5) .GT. 0) EPS = 0.0
C
C                                INTEGRATION PARAMETERS
      PI2 = 8.0*ATAN(1.0)
      DW = PI2*DELF
      NLO = IFIX(FL1/DELF+0.1)+1
      NLP = NLO-1
      NUP = IFIX(FU2/DELF+0.1)+1
      NUQ = NUP+1
      NTM = NT/2+1
      NW = NUP-NLO+1
C
      WRITE(6,110) NLO,NUP,NW,NW,NRP
 110   FORMAT(/,10X,'** RESPON**',//,11X,'FREQUENCY COUNTERS',/,
     1      13X,' NLO:',I10,5X,'NUP:',I10,6X,'NW :',I10/,
     2    //,11X,'DIMENSIONS OF TRANSFORM ARRAY ',/,
     3    14X,'NW :',I10,/,13X,' NRP :',I10,//)
C
C 
      AMUF = RHO(1)*BESQ(1)
      AIJS = 1.0/ALFSSQ
      BIJS = 1.0/BETSSQ
C#
C# # #              # # # # # # # # # # # # # #   SLOWNESS LOOP
C#
c     write (6,*) TMXX,TMYY,TMZZ,CMA,CMG,DMG
c
      DO 2000 LP = 1,NRP
       P = PG(LP)
       PQ = P*P
C
       QAS = CDSQRT(AIJS-PQ)
       QBS = CDSQRT(BIJS-PQ)
       YAS = -2.0*PQ+BIJS
C
C                               Calculate frequency independent
C                               reflection and transmission
C                               coefficients for all interfaces
       CALL RFACE
C
C                               Calculate frequency independent
C                               part of source excitation
C
       CALL MOMTEN
C
      IF(LP.EQ.101) THEN
        WRITE(6,119)
        WRITE(6,120) PU
        WRITE(6,121) SU
        WRITE(6,122) HU
 119    FORMAT(//,11X,'SOURCE TERMS in order m=0,+1,-1,+2,-2 ',/)
 120    FORMAT(11X,' ARRAY PU :',/,11X,10F10.4)
 121    FORMAT(11X,' ARRAY SU :',/,11X,10F10.4)
 122    FORMAT(11X,' ARRAY HU :',/,11X,10F10.4)
 125    FORMAT(10F10.4)
      ENDIf
C
      W = PI2*FL1 + YI*EPS
C
      LW = 1
C* * *                    * * * * * * * * * * *  FREQUENCY LOOP
      DO 3000 L=NLO,NUP
      IF( NWF .EQ.1) WRITE(6,115) L,LW,W
 115  FORMAT(2I10,2F10.4)
C
C                               Calculate reflection coefficients
C                               from below source
        CALL RTDN
C
C                               include phase terms in source layer
C                               (limiting decay in evanescence to
C                                avoid underflows)
        YIW = YI*W
        QQC = YIW*HBS*QAS
        IF(QQR.GT.-32.0D0) GO TO 1320
        PHPP = CDZERO
        GO TO 1330
 1320   PHPP = CDEXP(QQC)
 1330   QQC = YIW*HBS*QBS
        IF(QQR.GT.-32.0D0) GO TO 1340
        PHSS = CDZERO
        GO TO 1350
 1340   PHSS = CDEXP(QQC)
 1350   CONTINUE
        PHPS = PHPP*PHSS
        PHPP = PHPP*PHPP
        PHSS = PHSS*PHSS
        RDSLPP = RPP*PHPP
        RDSLPS = RPS*PHPS
        RDSLSP = RSP*PHPS
        RDSLSS = RSS*PHSS
        RDSLHH = RHH*PHSS
        IF(NWF.EQ.1) WRITE(6,125) RDSLPP,RDSLPS,RDSLSP,RDSLSS,RDSLHH
C
C                               Calculate reflection and
C                               displacement transfer
C                               for region above source
C                               (including effect of free surface
C                                - if present)
        CALL RYFUP
C
C                               include phase term in source layer
C                               (limiting decay in evanescence to
C                                avoid underflows)
        QQC = YIW*HAS*QAS
        IF(QQR.GT.-32.0D0) GO TO 1220
        PHPP = CDZERO
        GO TO 1230
 1220   PHPP = CDEXP(QQC)
 1230   QQC = YIW*HAS*QBS
        IF(QQR.GT.-32.0D0) GO TO 1240
        PHSS = CDZERO
        GO TO 1250
 1240   PHSS = CDEXP(QQC)
 1250   CONTINUE
        YMFSUP = YUP*PHPP
        YMFSUS = YUS*PHSS
        YMFSVP = YVP*PHPP
        YMFSVS = YVS*PHSS
        YMFSWH = YWH*PHSS
        PHPS = PHPP*PHSS
        PHPP = PHPP*PHPP
        PHSS = PHSS*PHSS
        RUFSPP = RPP*PHPP
        RUFSPS = RPS*PHPS
        RUFSSP = RSP*PHPS
        RUFSSS = RSS*PHSS
        RUFSHH = RHH*PHSS
        IF(NWF.EQ.1) WRITE(6,125) RUFSPP,RUFSPS,RUFSSP,RUFSSS,RUFSHH
C
C                               STACK REFLECTION COEFFICIENTS
C                               CALCULATED
C
C                               NOW CALCULATE SURFACE RESPONSE
        PQA = 2.0*P*QAJF
        PQB = 2.0*P*QBJF
        YA = -2.0*PQ+CDONE/BESQ(1)
C                               Full stack reverberations
C                                  ( I - RDSL*RUFS ) ** -1
C                               determines secular function
        UPP = RDSLPP*RUFSPP+RDSLPS*RUFSSP
        UPS = RDSLPP*RUFSPS+RDSLPS*RUFSSS
        USP = RDSLSP*RUFSPP+RDSLSS*RUFSSP
        USS = RDSLSP*RUFSPS+RDSLSS*RUFSSS
        UHH = RDSLHH*RUFSHH
C
        GO TO (5050,5100,5050,5050), NFS
 5050   UPP = CDONE-UPP
        USS = CDONE-USS
        DET = UPP*USS-USP*UPS
        WPP = USS/DET
        WPS = UPS/DET
        WSP = USP/DET
        WSS = UPP/DET
        UHH = CDONE-UHH
        WHH = CDONE/UHH
        GO TO 5200
C                               Only first term in expansion if ISW(6).eq.1
C                                 ( I + RDSL*RUFS )
 5100   WPP = CDONE+UPP
        WSS = CDONE+USS
        WPS = UPS
        WSP = USP
        WHH = CDONE+UHH
        GO TO 5200
C
C                               Include displacement transfer to surface
C                               -allows for near receiver reverberations
 5200   ZUP = YMFSUP*WPP+YMFSUS*WSP
        ZUS = YMFSUP*WPS+YMFSUS*WSS
        ZVP = YMFSVP*WPP+YMFSVS*WSP
        ZVS = YMFSVP*WPS+YMFSVS*WSS
        ZWH = YMFSWH*WHH
C
        IF(NWF.EQ.1)
     &      WRITE(6,125) ZUP,ZUS,ZVP,ZVS,ZWH
C
C                               Source terms
C                               -arranged to allow for reflection
C                                below source
C                        @ @ @ @ @ @ @ @ @   AZIMUTHAL ORDER LOOP
5500   DO 1400 MS=1,5
         MS1 = MS-1
         RP = XL(MS)*(RDSLPP*PU(MS)+RDSLPS*SU(MS))-PU(MS)
         RS = XL(MS)*(RDSLSP*PU(MS)+RDSLSS*SU(MS))-SU(MS)
         RH = -XL(MS)*RDSLHH*HU(MS)-HU(MS)
         IF( NWF .EQ.1) WRITE(6,125) RP,RS,RH

C                                 If far field waveform for source is used
C                                 factor YI*W  otherwise W*W
C                                 Combine medium response with specific
C                                 source terms
C
         RU(MS,LP,LW) = (ZUP*RP+ZUS*RS)*YIW
         RV(MS,LP,LW) = (ZVP*RP+ZVS*RS)*YIW
         RW(MS,LP,LW) = ZWH*RH*YIW
C
1400   CONTINUE
C                        @ @ @ @ @ @ @ @ @ END OF AZIMUTHAL ORDER LOOP
         IF(NWF.EQ.1) WRITE(6,125) RP,RS,RH
C
        W = W+DW
        LW = LW+1
 3000   CONTINUE
C* * *                   * * * * * * * * * END OF FREQUENCY LOOP
C     
 2000   CONTINUE
C
C# # #                   # # # # # # # # # END OF SLOWNESS LOOP
C
      RETURN
      END

      SUBROUTINE RFACE
C -----------------------------------------------------------------
C      **  ZERT  ** :   SUBROUTINE RFACE
C                       INTERFACE REFLECTION AND
C                       TRANSMISSION COEFFICIENTS
C                         - AT FIXED SLOWNESS P
C-------------------------------------------------------------------
C**
C      B.L.N. KENNETT      OCTOBER 1978 :: FEBRUARY 1983
C**
      COMMON F(4096),GU(4096),GV(4096),GW(4096),WR(2500)
      COMMON /PRESS/ RU(5,2500,600),RV(5,2500,600),RW(5,2500,600)
      COMMON /STRUCT/
     &     ALSQ(500),BESQ(500),
     &     RHO(500),HL(500),NR(500)
      COMMON /PARE/
     &     DELT,DELX,DELF,FL1,FL2,FU1,FU2,
     &     ISW(15),NT,NTI
      COMMON /INTEG/
     &     PG(2500),DP(2500),PMX,PMN,NRP
      COMMON /SPOT/
     &     XS(100),AZ(100),PR,STMIN,NPX
      COMPLEX*16    ALSQ,BESQ,CQA,CQB
      COMPLEX       F,RU,RV,RW,GU,GV,GW
      REAL          WR
C**
      COMMON /SOURCE/
     #     SOURC(15),ALFSSQ,BETSSQ,QAS,QBS,YAS,
     #     CMA,CMB,CMC,DMC,CMD,CME,CMF,DMF,CMG,DMG,
     #     RHOS,TMXX,TMXY,TMXZ,TMYX,TMYY,TMYZ,TMZX,TMZY,TMZZ,
     #     HAS,HBS,HAR,HBR,ERX,ESX,JS,NRX,NSX
      COMPLEX*16
     #     SOURC,ALFSSQ,BETSSQ,QAS,QBS,YAS,
     #     PU,SU,HU,PD,SD,HD
      COMPLEX
     #     CMA,CMB,CMC,DMC,CMD,CME,CMF,DMF,CMG,DMG
      DIMENSION PU(5),SU(5),HU(5)
      EQUIVALENCE (SOURC(1),PU(1)),(SOURC(6),SU(1)),(SOURC(11),HU(1))
C**
      COMMON /COEFF/
     %     HP(500),HS(500),
     %     RDPPR(500),RDPSR(500),RDSPR(500),RDSSR(500),
     %     RDPPI(500),RDPSI(500),RDSPI(500),RDSSI(500),
     %     RUPPR(500),RUPSR(500),RUSPR(500),RUSSR(500),
     %     RUPPI(500),RUPSI(500),RUSPI(500),RUSSI(500),
     %     TDPPR(500),TDPSR(500),TDSPR(500),TDSSR(500),
     %     TDPPI(500),TDPSI(500),TDSPI(500),TDSSI(500),
     %     TUPPR(500),TUPSR(500),TUSPR(500),TUSSR(500),
     %     TUPPI(500),TUPSI(500),TUSPI(500),TUSSI(500),
     %     RDHHR(500),RDHHI(500),RUHHR(500),RUHHI(500),
     %     TDHHR(500),TDHHI(500),TUHHR(500),TUHHI(500)
      COMPLEX*16
     %     HP,HS
      REAL*8
     %     RDPPR,RDPSR,RDSPR,RDSSR, RDPPI,RDPSI,RDSPI,RDSSI,
     %     RUPPR,RUPSR,RUSPR,RUSSR, RUPPI,RUPSI,RUSPI,RUSSI,
     %     TDPPR,TDPSR,TDSPR,TDSSR, TDPPI,TDPSI,TDSPI,TDSSI,
     %     TUPPR,TUPSR,TUSPR,TUSSR, TUPPI,TUPSI,TUSPI,TUSSI,
     %     RDHHR,RDHHI,RUHHR,RUHHI, TDHHR,TDHHI,TUHHR,TUHHI
      COMMON /STACK/
     %     RPP,RPS,RSP,RSS,RHH,TPP,TPS,TSP,TSS,THH,
     %     P,PQ,QAJF,QBJF,QMJF,AMUF,W
      COMPLEX*16
     %     RPP,RPS,RSP,RSS,RHH,TPP,TPS,TSP,TSS,THH,
     %     P,PQ,QAJF,QBJF,QMJF,AMUF,W
C**
      COMPLEX*16
     #  FD,GD,RC,CJ,CJPQ,EJ,FJ,HJ,DJ,
     #  AIJ,AIJB,BIJ,BIJB,QAJ,QAJB,QBJ,QBJB,CDTWO
      REAL*8 QQA(2),QQR,QQI
      COMPLEX*16 QQC
      EQUIVALENCE (QQC,QQA(1),QQR),(QQA(2),QQI)
      DATA CDTWO/(2.0D0,0.0D0)/
C**
      NL = ISW(1)
       PQ = P*P
C                    < < < < < < < < < < LOOP OVER LAYERING TO CALCULATE
C                                        INTERFACE COEFFS.
      NLA = NL-1
      J = NL
      DO 1000 LK=1,NLA
      JB = J-1
      IF(J.NE.NL) GO TO 240
      ROJ = RHO(J)
      AIJ = 1.0/ALSQ(J)
      BIJ = 1.0/BESQ(J)
      QAJ = CDSQRT(AIJ-PQ)
      QBJ = CDSQRT(BIJ-PQ)
 240  DEJ = HL(J)
      ROJB = RHO(JB)
      AIJB = 1.0/ALSQ(JB)
      QAJB = CDSQRT(AIJB-PQ)
      BIJB = 1.0/BESQ(JB)
      QBJB = CDSQRT(BIJB-PQ)
C                                      CALCULATE INTERFACE REFLECTION
C                                      AND TRANSMISSION COEFFICIENTS
C                                          downward reflection
      CJ = CDTWO*(ROJB/BIJB-ROJ/BIJ)
      CJPQ = CJ*PQ
      EJ = CJPQ-ROJB
      FJ = CJPQ+ROJ
      HJ = EJ+ROJ
      RRJ = ROJ*ROJB
      FD = PQ*HJ*HJ+QAJ*(QBJ*EJ*EJ+RRJ*QBJB)
      GD = QAJB*(QBJB*(CJ*CJPQ*QAJ*QBJ+FJ*FJ)+RRJ*QBJ)
      RC = CDTWO*P *(HJ*FJ+CJ*EJ*QAJ*QBJ)
      DJ = GD+FD
C*
      QQC = (GD-FD)/DJ
      RDPPR(LK) = QQR
      RDPPI(LK) = QQI
      QQC = -QBJB*RC/DJ
      RDPSR(LK) = QQR
      RDPSI(LK) = QQI
      QQC = -QAJB*RC/DJ
      RDSPR(LK) = QQR
      RDSPI(LK) = QQI
      QQC = (FD-GD+CDTWO*RRJ*(QBJ*QAJB-QBJB*QAJ))/DJ
      RDSSR(LK) = QQR
      RDSSI(LK) = QQI
C*
C                                          transmission
      RC = CDTWO*(EJ*QBJ-FJ*QBJB)/DJ
      QQC = -ROJB*QAJB*RC
      TDPPR(LK) = QQR
      TDPPI(LK) = QQI
      QQC = -ROJ*QAJ*RC
      TUPPR(LK) = QQR
      TUPPI(LK) = QQI
      RC = CDTWO*(EJ*QAJ-FJ*QAJB)/DJ
      QQC = -ROJB*QBJB*RC
      TDSSR(LK) = QQR
      TDSSI(LK) = QQI
      QQC = -ROJ*QBJ*RC
      TUSSR(LK) = QQR
      TUSSI(LK) = QQI
      RC = CDTWO*P *(HJ+CJ*QAJ*QBJB)/DJ
      QQC =  ROJB*QAJB*RC
      TDSPR(LK) = QQR
      TDSPI(LK) = QQI
      QQC =  ROJ*QBJ*RC
      TUPSR(LK) = QQR
      TUPSI(LK) = QQI
      RC = CDTWO*P *(HJ+CJ*QBJ*QAJB)/DJ
      QQC = -ROJB*QBJB*RC
      TDPSR(LK) = QQR
      TDPSI(LK) = QQI
      QQC = -ROJ*QAJ*RC
      TUSPR(LK) = QQR
      TUSPI(LK) = QQI
C*
C                                          upward reflection
      FD = PQ*HJ*HJ+QAJB*(QBJB*FJ*FJ+RRJ*QBJ)
      GD = QAJ*(QBJ*(CJPQ*CJ*QAJB*QBJB+EJ*EJ)+RRJ*QBJB)
      DJ = GD+FD
      RC = CDTWO*P *(HJ*EJ+CJ*QAJB*QBJB*FJ)
      QQC = (GD-FD)/DJ
      RUPPR(LK) = QQR
      RUPPI(LK) = QQI
      QQC = -QBJ*RC/DJ
      RUPSR(LK) = QQR
      RUPSI(LK) = QQI
      QQC = -QAJ*RC/DJ
      RUSPR(LK) = QQR
      RUSPI(LK) = QQI
      QQC = (FD-GD-CDTWO*RRJ*(QAJB*QBJ-QAJ*QBJB))/DJ
      RUSSR(LK) = QQR
      RUSSI(LK) = QQI
C*
C                                       SH WAVE COEFFICIENTS
C                                 nb ruhh=-rdhh,tuhh*tdhh=1.-rdhh**2
      EJ = ROJ*QBJ/BIJ
      FJ = ROJB*QBJB/BIJB
      RC = EJ+FJ
      QQC = (FJ-EJ)/RC
      RDHHR(LK) = QQR
      RDHHI(LK) = QQI
      RUHHR(LK) = -QQR
      RUHHI(LK) = -QQI
      QQC = CDTWO*FJ/RC
      TDHHR(LK) = QQR
      TDHHI(LK) = QQI
      QQC = CDTWO*EJ/RC
      TUHHR(LK) = QQR
      TUHHI(LK) = QQI
C
C                                        PHASE FACTORS FOR LAYER
      HP(LK) = DEJ*QAJ
      HS(LK) = DEJ*QBJ
C
      QAJ = QAJB
      QBJ = QBJB
      BIJ = BIJB
      ROJ = ROJB
      J = J-1
 1000   CONTINUE
C                   < < < < < < < < < < < < < END OF LAYER LOOP ONE
C
      QAJF = QAJ
      QBJF = QBJ
      QMJF = FJ
C
      RETURN
      END
      SUBROUTINE MOMTEN
C -------------------------------------------------------------------
C          **  ZERT  **   : SUBROUTINE MOMTEN
C                            FREQUENCY INDEPENDENT
C                            SLOWNESS DEPENDENT
C                            PART OF SOURCE TERMS
C                            picks up combinations of
C                            moment tensor components
C                            from common /SOURCE/
C                            CMA = 0.5*(TMXX+TMYY-2.0*TMZZ)
C                            CMB = 0.5*YI*(TMXY-TMYX)
C                            CMC =  TMXZ-YI*TMYZ
C                            DMC = -TMXZ-YI*TMYZ
C                            CMD = 0.5*YI*(TMZY-TMYZ)
C                            CME = 0.5*(TMXZ-TMZX)
C                            CMF = 0.5*(-TMYZ-YI*TMXZ)
C                            DMF = 0.5*( TMYZ-YI*TMXZ)
C                            CMG = 0.25*(-(TMXX-TMYY)+YI*(TMXY+TMYX))
C                            DMG = 0.25*( (TMXX-TMYY)+YI*(TMXY+TMYX))
C
C --------------------------------------------------------------------
C**
C      B.L.N. KENNETT      OCTOBER 1978
C**
      COMMON F(4096),GU(4096),GV(4096),GW(4096),WR(2500)
      COMMON /PRESS/ RU(5,2500,600),RV(5,2500,600),RW(5,2500,600)
      COMMON /STRUCT/
     &     ALSQ(500),BESQ(500),
     &     RHO(500),HL(500),NR(500)
      COMMON /PARE/
     &     DELT,DELX,DELF,FL1,FL2,FU1,FU2,
     &     ISW(15),NT,NTI
      COMMON /INTEG/
     &     PG(2500),DP(2500),PMX,PMN,NRP
      COMMON /SPOT/
     &     XS(100),AZ(100),PR,STMIN,NPX
      COMPLEX*16    ALSQ,BESQ,CQA,CQB
      COMPLEX       F,RU,RV,RW,GU,GV,GW
      REAL          WR
C**
      COMMON /SOURCE/
     #     SOURC(15),ALFSSQ,BETSSQ,QAS,QBS,YAS,
     #     CMA,CMB,CMC,DMC,CMD,CME,CMF,DMF,CMG,DMG,
     #     RHOS,TMXX,TMXY,TMXZ,TMYX,TMYY,TMYZ,TMZX,TMZY,TMZZ,
     #     HAS,HBS,HAR,HBR,ERX,ESX,JS,NRX,NSX
      COMPLEX*16
     #     SOURC,ALFSSQ,BETSSQ,QAS,QBS,YAS,
     #     PU,SU,HU
      COMPLEX
     #     CMA,CMB,CMC,DMC,CMD,CME,CMF,DMF,CMG,DMG
      DIMENSION PU(5),SU(5),HU(5)
      EQUIVALENCE (SOURC(1),PU(1)),(SOURC(6),SU(1)),(SOURC(11),HU(1))
C**
      COMMON /COEFF/
     %     HP(500),HS(500),
     %     RDPPR(500),RDPSR(500),RDSPR(500),RDSSR(500),
     %     RDPPI(500),RDPSI(500),RDSPI(500),RDSSI(500),
     %     RUPPR(500),RUPSR(500),RUSPR(500),RUSSR(500),
     %     RUPPI(500),RUPSI(500),RUSPI(500),RUSSI(500),
     %     TDPPR(500),TDPSR(500),TDSPR(500),TDSSR(500),
     %     TDPPI(500),TDPSI(500),TDSPI(500),TDSSI(500),
     %     TUPPR(500),TUPSR(500),TUSPR(500),TUSSR(500),
     %     TUPPI(500),TUPSI(500),TUSPI(500),TUSSI(500),
     %     RDHHR(500),RDHHI(500),RUHHR(500),RUHHI(500),
     %     TDHHR(500),TDHHI(500),TUHHR(500),TUHHI(500)
      COMPLEX*16
     %     HP,HS
      REAL*8
     %     RDPPR,RDPSR,RDSPR,RDSSR, RDPPI,RDPSI,RDSPI,RDSSI,
     %     RUPPR,RUPSR,RUSPR,RUSSR, RUPPI,RUPSI,RUSPI,RUSSI,
     %     TDPPR,TDPSR,TDSPR,TDSSR, TDPPI,TDPSI,TDSPI,TDSSI,
     %     TUPPR,TUPSR,TUSPR,TUSSR, TUPPI,TUPSI,TUSPI,TUSSI,
     %     RDHHR,RDHHI,RUHHR,RUHHI, TDHHR,TDHHI,TUHHR,TUHHI
      COMMON /STACK/
     %     RPP,RPS,RSP,RSS,RHH,TPP,TPS,TSP,TSS,THH,
     %     P,PQ,QAJF,QBJF,QMJF,AMUF,W
      COMPLEX*16
     %     RPP,RPS,RSP,RSS,RHH,TPP,TPS,TSP,TSS,THH,
     %     P,PQ,QAJF,QBJF,QMJF,AMUF,W
      COMPLEX*16 YDI
C**
      PQ = P*P
      YDI = (0.0D0,1.0D0)
C                                     angular order m=0
      PU(1) = (PQ*CMA+TMZZ/ALFSSQ)
C     PD(1) = -PU(1)
      SU(1) = P*QBS*CMA
C     SD(1) = -SU(1)
      HU(1) = P*CMB
C     HD(1) = HU(1)
C                                     angular order m=+1
      PU(2) = -YDI*P*QAS*(CMC-CMD-CME)
C     PD(2) = PU(2)
      SU(2) = -YDI*(0.5*YAS*CMC+PQ*(CMD+CME))
C     SD(2) = SU(2)
      HU(2) = QBS*CMF
C     HD(2) = -HU(2)
C                                     angular order m=-1
      PU(3) = -YDI*P*QAS*(DMC-CMD+CME)
C     PD(3) = PU(3)
      SU(3) = -YDI*(0.5*YAS*DMC+PQ*(CMD-CME))
C     SD(3) = SU(3)
      HU(3) = QBS*DMF
C     HD(3) = -HU(3)
C                                     angular order m=+2
      PU(4) = PQ*CMG
C     PD(4) = -PU(4)
      SU(4) = P*QBS*CMG
C     SD(4) = -SU(4)
      HU(4) = -P*CMG
C     HD(4) =  HU(4)
C                                     angular order m=-2
      PU(5) = -PQ*DMG
C     PD(5) = -PU(5)
      SU(5) = -P*QBS*DMG
C     SD(5) = -SU(5)
      HU(5) = -P*DMG
C     HD(5) =  HU(5)
C
      EAS = 1.0/(2.0*RHOS*QAS)
      EBS = 1.0/(2.0*RHOS*QBS)
      DO 10 J=1,5
      PU(J) = PU(J)*EAS
      SU(J) = SU(J)*EBS
      HU(J) = HU(J)*EBS/BETSSQ
 10   CONTINUE
      RETURN
      END

       SUBROUTINE RTDN
C-----------------------------------------------------------------------------
C      **  ZERT  ** : SUBROUTINE RTDN
C
C                      CALCULATES THE REFLECTION (AND TRANSMISSION)
C                      COEFFICIENT MATRICES FOR A DOWNWARD
C                      INCIDENT WAVE ON A STACK OF UNIFORM
C                      LAYERS
C
C-----------------------------------------------------------------------------
C**
C        B.L.N. KENNETT      OCTOBER 1978
C**
      COMMON F(4096),GU(4096),GV(4096),GW(4096),WR(2500)
      COMMON /PRESS/ RU(5,2500,600),RV(5,2500,600),RW(5,2500,600)
      COMMON /STRUCT/
     &     ALSQ(500),BESQ(500),
     &     RHO(500),HL(500),NR(500)
      COMMON /PARE/
     &     DELT,DELX,DELF,FL1,FL2,FU1,FU2,
     &     ISW(15),NT,NTI
      COMMON /INTEG/
     &     PG(2500),DP(2500),PMX,PMN,NRP
      COMMON /SPOT/
     &     XS(100),AZ(100),PR,STMIN,NPX
      COMPLEX*16    ALSQ,BESQ,CQA,CQB
      COMPLEX       F,RU,RV,RW,GU,GV,GW
      REAL          WR
C**
      COMMON /SOURCE/
     #     SOURC(15),ALFSSQ,BETSSQ,QAS,QBS,YAS,
     #     CMA,CMB,CMC,DMC,CMD,CME,CMF,DMF,CMG,DMG,
     #     RHOS,TMXX,TMXY,TMXZ,TMYX,TMYY,TMYZ,TMZX,TMZY,TMZZ,
     #     HAS,HBS,HAR,HBR,ERX,ESX,JS,NRX,NSX
      COMPLEX*16
     #     SOURC,ALFSSQ,BETSSQ,QAS,QBS,YAS,
     #     PU,SU,HU,PD,SD,HD
      COMPLEX
     #     CMA,CMB,CMC,DMC,CMD,CME,CMF,DMF,CMG,DMG
      DIMENSION PU(5),SU(5),HU(5)
      EQUIVALENCE (SOURC(1),PU(1)),(SOURC(6),SU(1)),(SOURC(11),HU(1))
C**
      COMMON /COEFF/
     %     HP(500),HS(500),
     %     RDPPR(500),RDPSR(500),RDSPR(500),RDSSR(500),
     %     RDPPI(500),RDPSI(500),RDSPI(500),RDSSI(500),
     %     RUPPR(500),RUPSR(500),RUSPR(500),RUSSR(500),
     %     RUPPI(500),RUPSI(500),RUSPI(500),RUSSI(500),
     %     TDPPR(500),TDPSR(500),TDSPR(500),TDSSR(500),
     %     TDPPI(500),TDPSI(500),TDSPI(500),TDSSI(500),
     %     TUPPR(500),TUPSR(500),TUSPR(500),TUSSR(500),
     %     TUPPI(500),TUPSI(500),TUSPI(500),TUSSI(500),
     %     RDHHR(500),RDHHI(500),RUHHR(500),RUHHI(500),
     %     TDHHR(500),TDHHI(500),TUHHR(500),TUHHI(500)
      COMPLEX*16
     %     HP,HS
      REAL*8
     %     RDPPR,RDPSR,RDSPR,RDSSR, RDPPI,RDPSI,RDSPI,RDSSI,
     %     RUPPR,RUPSR,RUSPR,RUSSR, RUPPI,RUPSI,RUSPI,RUSSI,
     %     TDPPR,TDPSR,TDSPR,TDSSR, TDPPI,TDPSI,TDSPI,TDSSI,
     %     TUPPR,TUPSR,TUSPR,TUSSR, TUPPI,TUPSI,TUSPI,TUSSI,
     %     RDHHR,RDHHI,RUHHR,RUHHI, TDHHR,TDHHI,TUHHR,TUHHI
      COMMON /STACK/
     %     RPP,RPS,RSP,RSS,RHH,TPP,TPS,TSP,TSS,THH,
     %     P,PQ,QAJF,QBJF,QMJF,AMUF,W
      COMPLEX*16
     %     RPP,RPS,RSP,RSS,RHH,TPP,TPS,TSP,TSS,THH,
     %     P,PQ,QAJF,QBJF,QMJF,AMUF,W
C**
      COMPLEX*16
     #  YDIW,PHPP,PHPS,PHSS
      REAL*8
     #  RPPR,RPPI,RPSR,RPSI,RSPR,RSPI,RSSR,RSSI,
     #  TPPR,TPPI,TPSR,TPSI,TSPR,TSPI,TSSR,TSSI,
     #  UPPR,UPPI,UPSR,UPSI,USPR,USPI,USSR,USSI,
     #  VPPR,VPPI,VPSR,VPSI,VSPR,VSPI,VSSR,VSSI,
     #  WPPR,WPPI,WPSR,WPSI,WSPR,WSPI,WSSR,WSSI,
     #  UHHR,UHHI,VHHR,VHHI,RHHR,RHHI,THHR,THHI,
     #  WHHR,WHHI,DETT,DETR
      REAL*8 QQA(2),QQR,QQI,RT,DONE
      COMPLEX*16 QQC
      EQUIVALENCE (QQC,QQA(1),QQR),(QQA(2),QQI)
      COMPLEX*16 YDI,CDZERO
      DATA YDI,CDZERO,DONE/(0.0D0,1.0D0),(0.0D0,0.D0),1.0D0/
C**
      NL  = ISW(1)
      J   = NL-1
      NSA = NL-JS
C                                      INITIAL CONDITIONS FOR ITERATION
CC                                            transmission
       TPPR = TDPPR(1)
       TPPI = TDPPI(1)
       TPSR = TDPSR(1)
       TPSI = TDPSI(1)
       TSPR = TDSPR(1)
       TSPI = TDSPI(1)
       TSSR = TDSSR(1)
       TSSI = TDSSI(1)
       THHR = TDHHR(1)
       THHI = TDHHI(1)
C                                             reflection
      RPPR = RDPPR(1)
      RPPI = RDPPI(1)
      RPSR = RDPSR(1)
      RPSI = RDPSI(1)
      RSPR = RDSPR(1)
      RSPI = RDSPI(1)
      RSSR = RDSSR(1)
      RSSI = RDSSI(1)
      RHHR = RDHHR(1)
      RHHI = RDHHI(1)
      IF(NSA.LT.2) GO TO 1200
      DO 1100 LK = 2,NSA
C                                USE RECURSIVE SCHEME TO ADD
C                                A LAYER TO STACK FOR REFLECTION COEFFS
C
C                                          add in phase factors for layer
      YDIW = YDI*W
      QQC = YDIW*HP(LK)
      IF(QQR.GT.-32.0D0) GO TO 220
      PHPP = CDZERO
      GO TO 230
 220  PHPP = CDEXP(QQC)
 230  QQC = YDIW*HS(LK)
      IF(QQR.GT.-32.0D0) GO TO 240
      PHSS = CDZERO
      GO TO 250
 240  PHSS = CDEXP(QQC)
 250  CONTINUE
C                                                reflection
      PHPS = PHPP*PHSS
      PHPP = PHPP*PHPP
      PHSS = PHSS*PHSS
C
      QQR = RPPR
      QQI = RPPI
      QQC = PHPP*QQC
      RPPR = QQR
      RPPI = QQI
      QQR = RPSR
      QQI = RPSI
      QQC = PHPS*QQC
      RPSR = QQR
      RPSI = QQI
      QQR = RSPR
      QQI = RSPI
      QQC = PHPS*QQC
      RSPR = QQR
      RSPI = QQI
      QQR = RSSR
      QQI = RSSI
      QQC = PHSS*QQC
      RSSR = QQR
      RSSI = QQI
CH
      QQR = RHHR
      QQI = RHHI
      QQC = PHSS*QQC
      RHHR = QQR
      RHHI = QQI
CH
C                                COMPOSITION OF REFLECTIONS
C                                   transmission into layer
      IF (NR(J)-2) 300,400,400
 300  IF(NR(J)) 310,310,320
 310  RT = 0.0D0
      GO TO 330
 320  RT = DONE
 330  CONTINUE
      WPPR = TDPPR(LK)
      WPPI = TDPPI(LK)
      WPSR = TDPSR(LK)
      WPSI = TDPSI(LK)
      WSPR = TDSPR(LK)
      WSPI = TDSPI(LK)
      WSSR = TDSSR(LK)
      WSSI = TDSSI(LK)
      WHHR = TDHHR(LK)
      WHHI = TDHHI(LK)
      GO TO 750
C                                     layer reverberations
 400  RT = DONE
      UPPR = RUPPR(LK)*RPPR-RUPPI(LK)*RPPI+RUPSR(LK)*RSPR-RUPSI(LK)*RSPI
      UPPI = RUPPR(LK)*RPPI+RUPPI(LK)*RPPR+RUPSR(LK)*RSPI+RUPSI(LK)*RSPR
      UPSR = RUPPR(LK)*RPSR-RUPPI(LK)*RPSI+RUPSR(LK)*RSSR-RUPSI(LK)*RSSI
      UPSI = RUPPR(LK)*RPSI+RUPPI(LK)*RPSR+RUPSR(LK)*RSSI+RUPSI(LK)*RSSR
      USPR = RUSPR(LK)*RPPR-RUSPI(LK)*RPPI+RUSSR(LK)*RSPR-RUSSI(LK)*RSPI
      USPI = RUSPR(LK)*RPPI+RUSPI(LK)*RPPR+RUSSR(LK)*RSPI+RUSSI(LK)*RSPR
      USSR = RUSPR(LK)*RPSR-RUSPI(LK)*RPSI+RUSSR(LK)*RSSR-RUSSI(LK)*RSSI
      USSI = RUSPR(LK)*RPSI+RUSPI(LK)*RPSR+RUSSR(LK)*RSSI+RUSSI(LK)*RSSR
CH
      UHHR = RUHHR(LK)*RHHR-RUHHI(LK)*RHHI
      UHHI = RUHHR(LK)*RHHI+RUHHI(LK)*RHHR
CH
C*
      IF(NR(J)-2) 750,500,600
 500  UPPR = DONE+UPPR
      USSR = DONE+USSR
      UHHR = DONE+UHHR
      GO TO 700
C*
  600 VPPR = DONE-USSR
      VPPI = -USSI
      VSSR = DONE-UPPR
      VSSI = -UPPI
      VSPR = VPPR*VSSR-VPPI*VSSI-UPSR*USPR+UPSI*USPI
      VSPI = VPPI*VSSR+VPPR*VSSI-UPSI*USPR-UPSR*USPI
      DETT = VSPR*VSPR+VSPI*VSPI
      UPPR = (VPPR*VSPR+VPPI*VSPI)/DETT
      UPPI = (VPPI*VSPR-VPPR*VSPI)/DETT
      DETR = UPSR
      UPSR = (DETR*VSPR+UPSI*VSPI)/DETT
      UPSI = (UPSI*VSPR-DETR*VSPI)/DETT
      DETR = USPR
      USPR = (DETR*VSPR+USPI*VSPI)/DETT
      USPI = (USPI*VSPR-DETR*VSPI)/DETT
      USSR = (VSSR*VSPR+VSSI*VSPI)/DETT
      USSI = (VSSI*VSPR-VSSR*VSPI)/DETT
CH
      UHHR = DONE-UHHR
C     UHHI = UHHI
      DETT = UHHR*UHHR+UHHI*UHHI
      UHHR = UHHR/DETT
      UHHI = UHHI/DETT
CH
C*
  700 WPPR = UPPR*TDPPR(LK)-UPPI*TDPPI(LK)+UPSR*TDSPR(LK)-UPSI*TDSPI(LK)
      WPPI = UPPR*TDPPI(LK)+UPPI*TDPPR(LK)+UPSR*TDSPI(LK)+UPSI*TDSPR(LK)
      WPSR = UPPR*TDPSR(LK)-UPPI*TDPSI(LK)+UPSR*TDSSR(LK)-UPSI*TDSSI(LK)
      WPSI = UPPR*TDPSI(LK)+UPPI*TDPSR(LK)+UPSR*TDSSI(LK)+UPSI*TDSSR(LK)
      WSPR = USPR*TDPPR(LK)-USPI*TDPPI(LK)+USSR*TDSPR(LK)-USSI*TDSPI(LK)
      WSPI = USPR*TDPPI(LK)+USPI*TDPPR(LK)+USSR*TDSPI(LK)+USSI*TDSPR(LK)
      WSSR = USPR*TDPSR(LK)-USPI*TDPSI(LK)+USSR*TDSSR(LK)-USSI*TDSSI(LK)
      WSSI = USPR*TDPSI(LK)+USPI*TDPSR(LK)+USSR*TDSSI(LK)+USSI*TDSSR(LK)
      WHHR = UHHR*TDHHR(LK)-UHHI*TDHHI(LK)
      WHHI = UHHR*TDHHI(LK)+UHHI*TDHHR(LK)
C*
  750 VPPR = RPPR*WPPR-RPPI*WPPI+RPSR*WSPR-RPSI*WSPI
      VPPI = RPPR*WPPI+RPPI*WPPR+RPSR*WSPI+RPSI*WSPR
      VPSR = RPPR*WPSR-RPPI*WPSI+RPSR*WSSR-RPSI*WSSI
      VPSI = RPPR*WPSI+RPPI*WPSR+RPSR*WSSI+RPSI*WSSR
      VSPR = RSPR*WPPR-RSPI*WPPI+RSSR*WSPR-RSSI*WSPI
      VSPI = RSPR*WPPI+RSPI*WPPR+RSSR*WSPI+RSSI*WSPR
      VSSR = RSPR*WPSR-RSPI*WPSI+RSSR*WSSR-RSSI*WSSI
      VSSI = RSPR*WPSI+RSPI*WPSR+RSSR*WSSI+RSSI*WSSR
CH
      VHHR = RHHR*WHHR-RHHI*WHHI
      VHHI = RHHR*WHHI+RHHI*WHHR
CH
C                                    include transmission out of layer
C
C                                    Reflection
      RPPR = RDPPR(LK)*RT
     1   +TUPPR(LK)*VPPR-TUPPI(LK)*VPPI+TUPSR(LK)*VSPR-TUPSI(LK)*VSPI
      RPPI = RDPPI(LK)*RT
     1   +TUPPR(LK)*VPPI+TUPPI(LK)*VPPR+TUPSR(LK)*VSPI+TUPSI(LK)*VSPR
      RPSR = RDPSR(LK)*RT
     1   +TUPPR(LK)*VPSR-TUPPI(LK)*VPSI+TUPSR(LK)*VSSR-TUPSI(LK)*VSSI
      RPSI = RDPSI(LK)*RT
     1   +TUPPR(LK)*VPSI+TUPPI(LK)*VPSR+TUPSR(LK)*VSSI+TUPSI(LK)*VSSR
      RSPR = RDSPR(LK)*RT
     1   +TUSSR(LK)*VSPR-TUSSI(LK)*VSPI+TUSPR(LK)*VPPR-TUSPI(LK)*VPPI
      RSPI = RDSPI(LK)*RT
     1   +TUSSR(LK)*VSPI+TUSSI(LK)*VSPR+TUSPR(LK)*VPPI+TUSPI(LK)*VPPR
      RSSR = RDSSR(LK)*RT
     1   +TUSPR(LK)*VPSR-TUSPI(LK)*VPSI+TUSSR(LK)*VSSR-TUSSI(LK)*VSSI
      RSSI = RDSSI(LK)*RT
     1   +TUSPR(LK)*VPSI+TUSPI(LK)*VPSR+TUSSR(LK)*VSSI+TUSSI(LK)*VSSR
CH
      RHHR = RDHHR(LK)*RT  +  TUHHR(LK)*VHHR-TUHHI(LK)*VHHI
      RHHI = RDHHI(LK)*RT  +  TUHHR(LK)*VHHI+TUHHI(LK)*VHHR
CH
      J = J-1
 1100 CONTINUE
C                   > > > > > > > > > > > > END OF LAYER LOOP TWO
C                              return complex reflection and
C                              transmission coefficients
 1200  RPP = DCMPLX(RPPR,RPPI)
       RPS = DCMPLX(RPSR,RPSI)
       RSP = DCMPLX(RSPR,RSPI)
       RSS = DCMPLX(RSSR,RSSI)
       RHH = DCMPLX(RHHR,RHHI)
       TPP = DCMPLX(TPPR,TPPI)
       TPS = DCMPLX(TPSR,TPSI)
       TSP = DCMPLX(TSPR,TSPI)
       TSS = DCMPLX(TSSR,TSSI)
       THH = DCMPLX(THHR,THHI)
      RETURN
      END

       SUBROUTINE RYFUP
C-----------------------------------------------------------------------------
C      **  ZERT  ** : SUBROUTINE RYFUP
C
C                      CALCULATES THE REFLECTION (R) AND
C                      DISPLACEMENT TRANSFER (Y)
C                      MATRICES FOR A UPWARD INCIDENT WAVE
C                      ON A STACK OF UNIFORM LAYERS
C                      BOUNDED ABOVE BY A FREE SURFACE OR
C                      AN UPPER HALF SPACE
C
C-----------------------------------------------------------------------------
C**
C        B.L.N. KENNETT      OCTOBER 1978 :: JULY 1980
C**
      COMMON F(4096),GU(4096),GV(4096),GW(4096),WR(2500)
      COMMON /PRESS/ RU(5,2500,600),RV(5,2500,600),RW(5,2500,600)
      COMMON /STRUCT/
     &     ALSQ(500),BESQ(500),
     &     RHO(500),HL(500),NR(500)
      COMMON /PARE/
     &     DELT,DELX,DELF,FL1,FL2,FU1,FU2,
     &     ISW(15),NT,NTI
      COMMON /INTEG/
     &     PG(2500),DP(2500),PMX,PMN,NRP
      COMMON /SPOT/
     &     XS(100),AZ(100),PR,STMIN,NPX
      COMPLEX*16    ALSQ,BESQ,CQA,CQB
      COMPLEX       F,RU,RV,RW,GU,GV,GW
      REAL          WR
C**
      COMMON /SOURCE/
     #     SOURC(15),ALFSSQ,BETSSQ,QAS,QBS,YAS,
     #     CMA,CMB,CMC,DMC,CMD,CME,CMF,DMF,CMG,DMG,
     #     RHOS,TMXX,TMXY,TMXZ,TMYX,TMYY,TMYZ,TMZX,TMZY,TMZZ,
     #     HAS,HBS,HAR,HBR,ERX,ESX,JS,NRX,NSX
      COMPLEX*16
     #     SOURC,ALFSSQ,BETSSQ,QAS,QBS,YAS,
     #     PU,SU,HU,PD,SD,HD
      COMPLEX
     #     CMA,CMB,CMC,DMC,CMD,CME,CMF,DMF,CMG,DMG
      DIMENSION PU(5),SU(5),HU(5)
      EQUIVALENCE (SOURC(1),PU(1)),(SOURC(6),SU(1)),(SOURC(11),HU(1))
C**
      COMMON /COEFF/
     %     HP(500),HS(500),
     %     RDPPR(500),RDPSR(500),RDSPR(500),RDSSR(500),
     %     RDPPI(500),RDPSI(500),RDSPI(500),RDSSI(500),
     %     RUPPR(500),RUPSR(500),RUSPR(500),RUSSR(500),
     %     RUPPI(500),RUPSI(500),RUSPI(500),RUSSI(500),
     %     TDPPR(500),TDPSR(500),TDSPR(500),TDSSR(500),
     %     TDPPI(500),TDPSI(500),TDSPI(500),TDSSI(500),
     %     TUPPR(500),TUPSR(500),TUSPR(500),TUSSR(500),
     %     TUPPI(500),TUPSI(500),TUSPI(500),TUSSI(500),
     %     RDHHR(500),RDHHI(500),RUHHR(500),RUHHI(500),
     %     TDHHR(500),TDHHI(500),TUHHR(500),TUHHI(500)
      COMPLEX*16
     %     HP,HS
      REAL*8
     %     RDPPR,RDPSR,RDSPR,RDSSR, RDPPI,RDPSI,RDSPI,RDSSI,
     %     RUPPR,RUPSR,RUSPR,RUSSR, RUPPI,RUPSI,RUSPI,RUSSI,
     %     TDPPR,TDPSR,TDSPR,TDSSR, TDPPI,TDPSI,TDSPI,TDSSI,
     %     TUPPR,TUPSR,TUSPR,TUSSR, TUPPI,TUPSI,TUSPI,TUSSI,
     %     RDHHR,RDHHI,RUHHR,RUHHI, TDHHR,TDHHI,TUHHR,TUHHI
      COMMON /STACK/
     %     RPP,RPS,RSP,RSS,RHH,YUP,YUS,YVP,YVS,YWH,
     %     P,PQ,QAJF,QBJF,QMJF,AMUF,W
      COMPLEX*16
     %     RPP,RPS,RSP,RSS,RHH,YUP,YUS,YVP,YVS,YWH,
     %     P,PQ,QAJF,QBJF,QMJF,AMUF,W
*           COMMON /STACK/
*     %     RPP,RPS,RSP,RSS,RHH,TPP,TPS,TSP,TSS,THH,
*     %     P,PQ,QAJF,QBJF,QMJF,AMUF,W
*      COMPLEX*16
*     %     RPP,RPS,RSP,RSS,RHH,TPP,TPS,TSP,TSS,THH,
*     %     P,PQ,QAJF,QBJF,QMJF,AMUF,W
C**
      COMPLEX*16
     #  YDIW,CA,CB,CC,CD,PHPP,PHPS,PHSS,PQA,PQB,YA
      REAL*8
     #  RPPR,RPPI,RPSR,RPSI,RSPR,RSPI,RSSR,RSSI,
     #  YUPR,YUPI,YUSR,YUSI,YVPR,YVPI,YVSR,YVSI,
     #  UPPR,UPPI,UPSR,UPSI,USPR,USPI,USSR,USSI,
     #  VPPR,VPPI,VPSR,VPSI,VSPR,VSPI,VSSR,VSSI,
     #  VUPR,VUPI,VUSR,VUSI,VVPR,VVPI,VVSR,VVSI,
     #  WPPR,WPPI,WPSR,WPSI,WSPR,WSPI,WSSR,WSSI,
     #  UHHR,UHHI,VHHR,VHHI,RHHR,RHHI,YWHR,YWHI,
     #  WHHR,WHHI,VWHR,VWHI,DETT,DETR
      REAL*8 QQA(2),QQR,QQI,RT,DONE,GH
      COMPLEX*16 QQC
      EQUIVALENCE (QQC,QQA(1),QQR),(QQA(2),QQI)
      COMPLEX*16 YDI,CDZERO,CDTWO
      DATA YDI,CDZERO,CDTWO,DONE
     &      /(0.0D0,1.0D0),(0.0D0,0.0D0),(2.0D0,0.0D0),1.0D0/
C**
      NL  = ISW(1)
      NFS = ISW(6)+1
      JSA = JS-1
      NLA = NL-1
      LK  = NL-2
C
      PQA = CDTWO*P*QAJF
      PQB = CDTWO*P*QBJF
      YA  = -CDTWO*P*P+DONE/BESQ(1)
      CA  = PQA*PQB-YA*YA
      CD  = PQA*PQB+YA*YA
      CB = YDI*CDTWO*YA/(BESQ(1)*CD)
      CC = YDI*CDTWO*CDTWO*QAJF*QBJF/(BESQ(1)*CD)
      GH = 2.0D0
C
C                                 INITIAL CONDITIONS FOR ITERATION
C
      GO TO (2000,2000,2100,2100), NFS
C                                 reflection/free surface
 2000 QQC = CA/CD
      RPPR = QQR
      RPPI = QQI
      RSSR = -QQR
      RSSI = -QQI
      QQC = CDTWO*PQB*YA/CD
      RPSR = QQR
      RPSI = QQI
      QQC = CDTWO*PQA*YA/CD
      RSPR = QQR
      RSPI = QQI
      RHHR = DONE
      RHHI = 0.0D0
      GO TO 2200
C                                       reflection
 2100 RPPR = RUPPR(NLA)
      RPPI = RUPPI(NLA)
      RPSR = RUPSR(NLA)
      RPSI = RUPSI(NLA)
      RSPR = RUSPR(NLA)
      RSPI = RUSPI(NLA)
      RSSR = RUSSR(NLA)
      RSSI = RUSSI(NLA)
      RHHR = RUHHR(NLA)
      RHHI = RUHHI(NLA)
      GO TO 2200
C
 2200  IF(NFS.LT.4) GO TO 2300
       CB = YDI
       CC = YDI
       GH = DONE
C                                       displacement transfer
 2300  QQC = -QAJF*CB
       YUPR = QQR
       YUPI = QQI
       QQC = QBJF*CB
       YVSR = QQR
       YVSI = QQI
       QQC = P*CC
       YUSR = QQR
       YVPR = QQR
       YUSI = QQI
       YVPI = QQI
       YWHR = GH
       YWHI = 0.0D0
C
      IF(JSA.LT.2) GO TO 1200
      DO 1100 J = 2,JSA
C                                USE RECURSIVE SCHEME TO ADD
C                                A LAYER TO STACK FOR REFLECTION COEFFS
C
C                                          add in phase factors for layer
      YDIW = YDI*W
      QQC = YDIW*HP(LK+1)
      IF(QQR.GT.-32.0D0) GO TO 220
      PHPP = CDZERO
      GO TO 230
 220  PHPP = CDEXP(QQC)
 230  QQC = YDIW*HS(LK+1)
      IF(QQR.GT.-32.0D0) GO TO 240
      PHSS = CDZERO
      GO TO 250
 240  PHSS = CDEXP(QQC)
 250  CONTINUE
C                                            transmission
      QQR = YUPR
      QQI = YUPI
      QQC = PHPP*QQC
      YUPR = QQR
      YUPI = QQI
      QQR = YUSR
      QQI = YUSI
      QQC = PHSS*QQC
      YUSR = QQR
      YUSI = QQI
      QQR = YVPR
      QQI = YVPI
      QQC = PHPP*QQC
      YVPR = QQR
      YVPI = QQI
      QQR = YVSR
      QQI = YVSI
      QQC = PHSS*QQC
      YVSR = QQR
      YVSI = QQI
CH
      QQR = YWHR
      QQI = YWHI
      QQC = PHSS*QQC
      YWHR = QQR
      YWHI = QQI
CH
C                                                reflection
      PHPS = PHPP*PHSS
      PHPP = PHPP*PHPP
      PHSS = PHSS*PHSS
C
      QQR = RPPR
      QQI = RPPI
      QQC = PHPP*QQC
      RPPR = QQR
      RPPI = QQI
      QQR = RPSR
      QQI = RPSI
      QQC = PHPS*QQC
      RPSR = QQR
      RPSI = QQI
      QQR = RSPR
      QQI = RSPI
      QQC = PHPS*QQC
      RSPR = QQR
      RSPI = QQI
      QQR = RSSR
      QQI = RSSI
      QQC = PHSS*QQC
      RSSR = QQR
      RSSI = QQI
CH
      QQR = RHHR
      QQI = RHHI
      QQC = PHSS*QQC
      RHHR = QQR
      RHHI = QQI
CH
C                                COMPOSITION OF REFLECTIONS
C                                   transmission into layer
      IF (NR(J)-2) 300,400,400
 300  IF(NR(J)) 310,310,320
 310  RT = 0.0D0
      GO TO 330
 320  RT = DONE
 330  CONTINUE
      WPPR = TUPPR(LK)
      WPPI = TUPPI(LK)
      WPSR = TUPSR(LK)
      WPSI = TUPSI(LK)
      WSPR = TUSPR(LK)
      WSPI = TUSPI(LK)
      WSSR = TUSSR(LK)
      WSSI = TUSSI(LK)
      WHHR = TUHHR(LK)
      WHHI = TUHHI(LK)
      GO TO 750
C                                     layer reverberations
 400  RT = DONE
      UPPR = RDPPR(LK)*RPPR-RDPPI(LK)*RPPI+RDPSR(LK)*RSPR-RDPSI(LK)*RSPI
      UPPI = RDPPR(LK)*RPPI+RDPPI(LK)*RPPR+RDPSR(LK)*RSPI+RDPSI(LK)*RSPR
      UPSR = RDPPR(LK)*RPSR-RDPPI(LK)*RPSI+RDPSR(LK)*RSSR-RDPSI(LK)*RSSI
      UPSI = RDPPR(LK)*RPSI+RDPPI(LK)*RPSR+RDPSR(LK)*RSSI+RDPSI(LK)*RSSR
      USPR = RDSPR(LK)*RPPR-RDSPI(LK)*RPPI+RDSSR(LK)*RSPR-RDSSI(LK)*RSPI
      USPI = RDSPR(LK)*RPPI+RDSPI(LK)*RPPR+RDSSR(LK)*RSPI+RDSSI(LK)*RSPR
      USSR = RDSPR(LK)*RPSR-RDSPI(LK)*RPSI+RDSSR(LK)*RSSR-RDSSI(LK)*RSSI
      USSI = RDSPR(LK)*RPSI+RDSPI(LK)*RPSR+RDSSR(LK)*RSSI+RDSSI(LK)*RSSR
CH
      UHHR = RDHHR(LK)*RHHR-RDHHI(LK)*RHHI
      UHHI = RDHHR(LK)*RHHI+RDHHI(LK)*RHHR
CH
C*
      IF(NR(J)-2) 750,500,600
 500  UPPR = DONE+UPPR
      USSR = DONE+USSR
      UHHR = DONE+UHHR
      GO TO 700
C*
  600 VPPR = DONE-USSR
      VPPI = -USSI
      VSSR = DONE-UPPR
      VSSI = -UPPI
      VSPR = VPPR*VSSR-VPPI*VSSI-UPSR*USPR+UPSI*USPI
      VSPI = VPPI*VSSR+VPPR*VSSI-UPSI*USPR-UPSR*USPI
      DETT = VSPR*VSPR+VSPI*VSPI
      UPPR = (VPPR*VSPR+VPPI*VSPI)/DETT
      UPPI = (VPPI*VSPR-VPPR*VSPI)/DETT
      DETR = UPSR
      UPSR = (DETR*VSPR+UPSI*VSPI)/DETT
      UPSI = (UPSI*VSPR-DETR*VSPI)/DETT
      DETR = USPR
      USPR = (DETR*VSPR+USPI*VSPI)/DETT
      USPI = (USPI*VSPR-DETR*VSPI)/DETT
      USSR = (VSSR*VSPR+VSSI*VSPI)/DETT
      USSI = (VSSI*VSPR-VSSR*VSPI)/DETT
CH
      UHHR = DONE-UHHR
C     UHHI = UHHI
      DETT = UHHR*UHHR+UHHI*UHHI
      UHHR = UHHR/DETT
      UHHI = UHHI/DETT
CH
C*
  700 WPPR = UPPR*TUPPR(LK)-UPPI*TUPPI(LK)+UPSR*TUSPR(LK)-UPSI*TUSPI(LK)
      WPPI = UPPR*TUPPI(LK)+UPPI*TUPPR(LK)+UPSR*TUSPI(LK)+UPSI*TUSPR(LK)
      WPSR = UPPR*TUPSR(LK)-UPPI*TUPSI(LK)+UPSR*TUSSR(LK)-UPSI*TUSSI(LK)
      WPSI = UPPR*TUPSI(LK)+UPPI*TUPSR(LK)+UPSR*TUSSI(LK)+UPSI*TUSSR(LK)
      WSPR = USPR*TUPPR(LK)-USPI*TUPPI(LK)+USSR*TUSPR(LK)-USSI*TUSPI(LK)
      WSPI = USPR*TUPPI(LK)+USPI*TUPPR(LK)+USSR*TUSPI(LK)+USSI*TUSPR(LK)
      WSSR = USPR*TUPSR(LK)-USPI*TUPSI(LK)+USSR*TUSSR(LK)-USSI*TUSSI(LK)
      WSSI = USPR*TUPSI(LK)+USPI*TUPSR(LK)+USSR*TUSSI(LK)+USSI*TUSSR(LK)
      WHHR = UHHR*TUHHR(LK)-UHHI*TUHHI(LK)
      WHHI = UHHR*TUHHI(LK)+UHHI*TUHHR(LK)
C*
  750 VPPR = RPPR*WPPR-RPPI*WPPI+RPSR*WSPR-RPSI*WSPI
      VPPI = RPPR*WPPI+RPPI*WPPR+RPSR*WSPI+RPSI*WSPR
      VPSR = RPPR*WPSR-RPPI*WPSI+RPSR*WSSR-RPSI*WSSI
      VPSI = RPPR*WPSI+RPPI*WPSR+RPSR*WSSI+RPSI*WSSR
      VSPR = RSPR*WPPR-RSPI*WPPI+RSSR*WSPR-RSSI*WSPI
      VSPI = RSPR*WPPI+RSPI*WPPR+RSSR*WSPI+RSSI*WSPR
      VSSR = RSPR*WPSR-RSPI*WPSI+RSSR*WSSR-RSSI*WSSI
      VSSI = RSPR*WPSI+RSPI*WPSR+RSSR*WSSI+RSSI*WSSR
CH
      VHHR = RHHR*WHHR-RHHI*WHHI
      VHHI = RHHR*WHHI+RHHI*WHHR
CH
C                                    include transmission out of layer
C
C                                    Reflection
      RPPR = RUPPR(LK)*RT
     1   +TDPPR(LK)*VPPR-TDPPI(LK)*VPPI+TDPSR(LK)*VSPR-TDPSI(LK)*VSPI
      RPPI = RUPPI(LK)*RT
     1   +TDPPR(LK)*VPPI+TDPPI(LK)*VPPR+TDPSR(LK)*VSPI+TDPSI(LK)*VSPR
      RPSR = RUPSR(LK)*RT
     1   +TDPPR(LK)*VPSR-TDPPI(LK)*VPSI+TDPSR(LK)*VSSR-TDPSI(LK)*VSSI
      RPSI = RUPSI(LK)*RT
     1   +TDPPR(LK)*VPSI+TDPPI(LK)*VPSR+TDPSR(LK)*VSSI+TDPSI(LK)*VSSR
      RSPR = RUSPR(LK)*RT
     1   +TDSSR(LK)*VSPR-TDSSI(LK)*VSPI+TDSPR(LK)*VPPR-TDSPI(LK)*VPPI
      RSPI = RUSPI(LK)*RT
     1   +TDSSR(LK)*VSPI+TDSSI(LK)*VSPR+TDSPR(LK)*VPPI+TDSPI(LK)*VPPR
      RSSR = RUSSR(LK)*RT
     1   +TDSPR(LK)*VPSR-TDSPI(LK)*VPSI+TDSSR(LK)*VSSR-TDSSI(LK)*VSSI
      RSSI = RUSSI(LK)*RT
     1   +TDSPR(LK)*VPSI+TDSPI(LK)*VPSR+TDSSR(LK)*VSSI+TDSSI(LK)*VSSR
CH
      RHHR = RUHHR(LK)*RT  +  TDHHR(LK)*VHHR-TDHHI(LK)*VHHI
      RHHI = RUHHI(LK)*RT  +  TDHHR(LK)*VHHI+TDHHI(LK)*VHHR
CH
C                                    Transmission
      VUPR = YUPR*WPPR-YUPI*WPPI+YUSR*WSPR-YUSI*WSPI
      VUPI = YUPR*WPPI+YUPI*WPPR+YUSR*WSPI+YUSI*WSPR
      VUSR = YUPR*WPSR-YUPI*WPSI+YUSR*WSSR-YUSI*WSSI
      VUSI = YUPR*WPSI+YUPI*WPSR+YUSR*WSSI+YUSI*WSSR
      VVPR = YVPR*WPPR-YVPI*WPPI+YVSR*WSPR-YVSI*WSPI
      VVPI = YVPR*WPPI+YVPI*WPPR+YVSR*WSPI+YVSI*WSPR
      VVSR = YVPR*WPSR-YVPI*WPSI+YVSR*WSSR-YVSI*WSSI
      VVSI = YVPR*WPSI+YVPI*WPSR+YVSR*WSSI+YVSI*WSSR
      VWHR = YWHR*WHHR-YWHI*WHHI
      VWHI = YWHR*WHHI+YWHI*WHHR
C
      YUPR = VUPR
      YUPI = VUPI
      YUSR = VUSR
      YUSI = VUSI
      YVPR = VVPR
      YVPI = VVPI
      YVSR = VVSR
      YVSI = VVSI
      YWHR = VWHR
      YWHI = VWHI
C*
      LK = LK-1
 1100 CONTINUE
C                   > > > > > > > > > > > > END OF LAYER LOOP TWO
C                              return complex reflection and
C                              displacement coefficients
 1200  RPP = DCMPLX(RPPR,RPPI)
       RPS = DCMPLX(RPSR,RPSI)
       RSP = DCMPLX(RSPR,RSPI)
       RSS = DCMPLX(RSSR,RSSI)
       RHH = DCMPLX(RHHR,RHHI)
       YUP = DCMPLX(YUPR,YUPI)
       YUS = DCMPLX(YUSR,YUSI)
       YVP = DCMPLX(YVPR,YVPI)
       YVS = DCMPLX(YVSR,YVSI)
       YWH = DCMPLX(YWHR,YWHI)
      RETURN
      END

      SUBROUTINE SEISB
C----------------------------------------------------------------------------
C      SUBROUTINE SEISB
C           PERFORMS INTEGRATION OVER SLOWNESS AND FFT OVER TIME
C          TO GENERATE A SEISMOGRAM TIME SERIES FOR EACH DISTANCE
C
C          THE THREE COMPONENTS OF DISPLACEMENT ARE WRITTEN TO
C          UNIT 11 IN THE ORDER
C                  -  VERTICAL  COMPONENT
C                  -  RADIAL    COMPONENT
C                  -  AZIMUTHAL COMPONENT
C------------------------------------------------------------------------------
c
c update required for integration
c will depend on formulation of response
c
C**
      COMMON F(4096),GU(4096),GV(4096),GW(4096),WR(2500)
      COMMON /PRESS/ RU(5,2500,600),RV(5,2500,600),RW(5,2500,600)
      COMMON /STRUCT/
     &     ALSQ(500),BESQ(500),
     &     RHO(500),HL(500),NR(500)
      COMMON /PARE/
     &     DELT,DELX,DELF,FL1,FL2,FU1,FU2,
     &     ISW(15),NT,NTI
      COMMON /INTEG/
     &     PG(2500),DP(2500),PMX,PMN,NRP
      COMMON /SPOT/
     &     XS(100),AZ(100),PR,STMIN,NPX
      COMPLEX*16    ALSQ,BESQ,CQA,CQB
      COMPLEX       F,RU,RV,RW,GU,GV,GW
      REAL          WR
C**
      REAL FR(4096)
      COMPLEX CZERO
      COMPLEX*16
     #  CA,CB,CC,E1,F1,E2,F2,EP,CR,BA,CDZERO,YDI,
     #  YXWP,YXWP2,BAE1,BAF1,BAE2,BAF2,BBE1,BBF1,BBE2,BBF2,
     #  YBAE1,YBAF1,YBAE2,YBAF2,BJ0,BJ1,BJ2,BJ0D,BJ1D,BJ2D
      REAL*8
     #  P,CP,BESFJ0,BESFJ1,XWP
      CHARACTER*4 CZC,CRC,CTC
      DATA CZC,CRC,CTC/' Z  ',' R  ',' T  '/
      real radeg
C**
C
      PIB2 = 2.0*ATAN(1.0)
      PI2 = 8.0*ATAN(1.0)
      CZERO = (0.0,0.0)
      CDZERO = (0.0D0,0.0D0)
      YDI = (0.0D0,1.0D0)
      DW = PI2*DELF
      radeg = 45.0/atan(1.0)
C
      NNF = ISW(4)
      NLO = IFIX(FL1/DELF+0.1)+1
      NLP = NLO-1
      NUP = IFIX(FU2/DELF+0.1)+1
      NUQ = NUP+1
      NTM = NT/2+1
      NW = NUP-NLO+1
      EPS = 4.0*DELF
      IF(ISW(5).GT.0) EPS = 0.0
      NP1 = 1+ISW(8)
      NP2 = NRP-ISW(9)
      ZP1 = FLOAT(NP1)
      ZP2 = FLOAT(NRP-NP2)
C
C
      REWIND 11
c                                      zst format header
      ncomp = 3
      write (11) npx,ncomp
C                                      INTEGRATION TO YIELD SEISMOGRAMS
C
C++++++                              ++++++ LOOP OVER DISTANCES
      DO 2000 JX = 1,NPX
C
C                                    azimuth terms
        BA = YDI*AZ(JX)
        E1 = CDEXP(BA)
        F1 = (1.0D0,0.0D0)/E1
        E2 = E1*E1
        F2 = (1.0D0,0.0D0)/E2
        LW = 1
        W = PI2*FL1
C******                              ****** LOOP OVER FREQUENCY
        DO 1500 L=NLO,NUP
          CA = CDZERO
          CB = CDZERO
          CC = CDZERO
          XW = W*XS(JX)
          CR = -YDI*(PR*XW+W*STMIN)
          EP = CDEXP(CR)
C<<<<<<                              <<<<<< TRAPEZIUM RULE INTEGRATION
C                                              OVER SLOWNESS
C
          DO 1400 LP=1,NRP
            WR(LP) = 1.0
            IF(LP.LT.NP1) then
              WR(LP) = WR(LP)*FLOAT(LP)/ZP1
            ELSEIF(LP.GT.NP2) then
              WR(LP) = WR(LP)*FLOAT(NRP+1-LP)/ZP2
            ENDIF
            WR(1)   = 0.5*WR(1)
            WR(NRP) = 0.5*WR(NRP) 
            PGR = PG(LP)
            P = PGR
            CP = P*DP(LP)*DEXP(-EPS*P*XS(JX))
            XWP = XW*P
C
            IF(NNF.eq.1) then
               Z = DSQRT(1.0D0/(PIB2*XWP))
               BJ0 = Z*CDEXP(YDI*(XWP-0.5*PIB2))*CP
               BJ1 = -YDI*BJ0
            ELSE
               BJ0 = BESFJ0(XWP)*CP
               BJ1 = BESFJ1(XWP)*CP
            ENDIF
            BJ2 = 2.0*BJ1/XWP-BJ0
            BJ0D = -BJ1
            BJ1D = BJ0-BJ1/XWP
            BJ2D = BJ1-2.0*BJ2/XWP
C
            YXWP = YDI/XWP
            YXWP2 = 2.0*YXWP
            BAE1 = BJ1*E1
            BAF1 = BJ1*F1
            BAE2 = BJ2*E2
            BAF2 = BJ2*F2
            BBE1 = BJ1D*E1
            BBF1 = BJ1D*F1
            BBE2 = BJ2D*E2
            BBF2 = BJ2D*F2
            YBAE1 = YXWP*BAE1
            YBAF1 = YXWP*BAF1
            YBAE2 = YXWP2*BAE2
            YBAF2 = YXWP2*BAF2
C                                    @ @ @ @ AZIMUTH LOOP
C                                       dominant terms
            CA = CA+WR(LP)*(
     ^              RU(1,LP,LW)*BJ0 
     ^             +RU(2,LP,LW)*BAE1
     ^             -RU(3,LP,LW)*BAF1
     ^             +RU(4,LP,LW)*BAE2
     ^             +RU(5,LP,LW)*BAF2)
            CB = CB+WR(LP)*(
     ^              RV(1,LP,LW)*BJ0D
     ^             +RV(2,LP,LW)*BBE1
     ^             -RV(3,LP,LW)*BBF1
     ^             +RV(4,LP,LW)*BBE2
     ^             +RV(5,LP,LW)*BBF2)
            CC = CC+WR(LP)*(
     ^              RW(1,LP,LW)*BJ0D
     ^             +RW(2,LP,LW)*BBE1
     ^             -RW(3,LP,LW)*BBF1
     ^             +RW(4,LP,LW)*BBE2
     ^             +RW(5,LP,LW)*BBF2)
C                                      near field terms
C          IF(NNF.eq.0) then
            CB = CB+WR(LP)*(
     ^              RV(2,LP,LW)*YBAE1
     ^             +RV(3,LP,LW)*YBAF1
     ^             +RV(4,LP,LW)*YBAE2
     ^             -RV(5,LP,LW)*YBAF2)
            CC = CC+WR(LP)*(
     ^             -RW(2,LP,LW)*YBAE1
     ^             -RW(3,LP,LW)*YBAF1
     ^             -RW(4,LP,LW)*YBAE2
     ^             +RW(5,LP,LW)*YBAF2)
C          ENDIF
C                                    @ @ @ @ END AZIMUTH LOOP
 1400     CONTINUE
C
C<<<<<<                               <<<<<< END SLOWNESS LOOP
C
          GU(L) =  YDI*CA*F(L)*EP
          GV(L) =     CB*F(L)*EP
          GW(L) =    -CC*F(L)*EP
c*          write(6,*) "l,ca,bc,cc: ",l,ca,cb,cc
c*          write(6,*) gu(l),gv(l),gw(l)
          W = W+DW
          LW = LW+1
 1500   CONTINUE
C
C******                                ****** END FREQUENCY LOOP
C
C                                          REFLECT SPECTRUM
       DO 1840 L=1,NLP
         GU(L) = CZERO
         GV(L) = CZERO
         GW(L) = CZERO
 1840  CONTINUE
       DO 1880 L=NUQ,NTM
         GU(L) = CZERO
         GV(L) = CZERO
         GW(L) = CZERO
 1880  CONTINUE
       NTA = NT+2
       DO 1900 L=NTM,NT
         LR = NTA-L
         GU(L) = CONJG(GU(LR))
         GV(L) = CONJG(GV(LR))
         GW(L) = CONJG(GW(LR))
 1900  CONTINUE
C                                          INVERT FOURIER TRANSFORM
C                                          WRITE OUT SEISMOGRAMS AT
C                                          ONE DISTANCE  
C
C    in zst format  blnk Sept 1988:
        TCAL = STMIN+PR*XS(JX)
        SMP = 0.
C
        NT3 = 3*NT/4
        CALL FCOOLR(NTI,GU,-1.0)
        DO 1910 L=1,NT
          GAIN = 1.0/FLOAT(NT)
          IF (L .LT. NT3)
     &      GAIN = EXP ( FLOAT(L-1)*DELT*EPS ) / FLOAT(NT)
          FR(L) = REAL(GU(L))*GAIN
          IF(abs(FR(L)).gt.SMP) SMP = abs(FR(L))
 1910   CONTINUE
        WRITE(6,*) XS(JX),AZ(JX)*radeg,CZC,DELT,NT,PR,TCAL,SMP
        WRITE(11) XS(JX),AZ(JX)*radeg,CZC,DELT,NT,PR,TCAL,SMP,
     &            (FR(LK),LK=1,NT)
        WRITE(312,*) XS(JX),AZ(JX)*radeg,CZC,DELT,NT,PR,TCAL,SMP
        WRITE(12,*) (FR(LK),LK=1,NT)
        IF(ISW(15).GT.0) THEN
          WRITE(6,90) (FR(LK),LK=1,NT)
        ENDIF
C
        SMP = 0.
        CALL FCOOLR(NTI,GV,-1.0)
        DO 1920 L=1,NT
          GAIN = 1.0/FLOAT(NT)
          IF (L .LT. NT3)
     &      GAIN = EXP ( FLOAT(L-1)*DELT*EPS ) / FLOAT(NT)
          FR(L) = REAL(GV(L))*GAIN
          IF(abs(FR(L)).gt.SMP) SMP = abs(FR(L))
 1920   CONTINUE
        WRITE(6,*) XS(JX),AZ(JX)*radeg,CRC,DELT,NT,PR,TCAL,SMP
        WRITE(11) XS(JX),AZ(JX)*radeg,CRC,DELT,NT,PR,TCAL,SMP,
     &            (FR(LK),LK=1,NT)
        WRITE(313,*) XS(JX),AZ(JX)*radeg,CRC,DELT,NT,PR,TCAL,SMP
        WRITE(13,*) (FR(LK),LK=1,NT)
        IF(ISW(15).GT.0) THEN
          WRITE(6,90) (FR(LK),LK=1,NT)
        ENDIF
C
        SMP = 0.
        CALL FCOOLR(NTI,GW,-1.0)
        DO 1930 L=1,NT
          GAIN = 1.0/FLOAT(NT)
          IF (L .LT. NT3)
     &      GAIN = EXP ( FLOAT(L-1)*DELT*EPS ) / FLOAT(NT)
          FR(L) = REAL(GW(L))*GAIN
          IF(abs(FR(L)).gt.SMP) SMP = abs(FR(L))
 1930   CONTINUE
        WRITE(6,*) XS(JX),AZ(JX)*radeg,CTC,DELT,NT,PR,TCAL,SMP
        WRITE(11) XS(JX),AZ(JX)*radeg,CTC,DELT,NT,PR,TCAL,SMP,
     &            (FR(LK),LK=1,NT)
        WRITE(314,*) XS(JX),AZ(JX)*radeg,CTC,DELT,NT,PR,TCAL,SMP
        WRITE(14,*) (FR(LK),LK=1,NT)
        IF(ISW(15).GT.0) THEN
          WRITE(6,90) (FR(LK),LK=1,NT)
        ENDIF
C
 2000 CONTINUE
C++++++                                  ++++++ END DISTANCE LOOP
C
      RETURN
  90   FORMAT(8E15.5)
  91   FORMAT (//1X,' COMP AT:',F10.2,' KM    PR=',F12.5,/)
      END

