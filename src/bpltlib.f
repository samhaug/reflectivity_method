      subroutine toupper(cword,n)
      character*80 cword
      character*1 cb
      do 10 k=1,n
        cb = cword(k:k)
        ib = ichar(cb)
        if(ib.gt.97) ib=ib-32
        cb = char(ib)
        cword(k:k) = cb
 10   continue
      return
      end
        SUBROUTINE FCOOLR(K,D,SN)
C----------------------------------------------------------------------
C          SUBROUTINE FCOOLR(K,D,SN)
C          FAST FOURIER TRANSFORM OF N = 2**K COMPLEX DATA POINTS
C          REPARTS HELD IN D(1,3,...2N-1), IMPARTS IN D(2,4,...2N).
C
C----------------------------------------------------------------------
C
        DIMENSION INU(15),D(1)
        LX=2**K
        Q1=LX
        IL=LX
        SH=SN*6.28318530718/Q1
        DO 10 I=1,K
        IL=IL/2
10      INU(I)=IL
        NKK=1
        DO 40 LA=1,K
        NCK=NKK
        NKK=NKK+NKK
        LCK=LX/NCK
        L2K=LCK+LCK
        NW=0
        DO 40 ICK=1,NCK
        FNW=NW
        AA=SH*FNW
        W1=COS(AA)
        W2=SIN(AA)
        LS=L2K*(ICK-1)
        DO 20 I=2,LCK,2
        J1=I+LS
        J=J1-1
        JH=J+LCK
        JH1=JH+1
        Q1=D(JH)*W1-D(JH1)*W2
        Q2=D(JH)*W2+D(JH1)*W1
        D(JH)=D(J)-Q1
        D(JH1)=D(J1)-Q2
        D(J)=D(J)+Q1
20      D(J1)=D(J1)+Q2
        DO 29 I=2,K
        ID=INU(I)
        IL=ID+ID
        IF(NW-ID-IL*(NW/IL)) 40,30,30
30      NW=NW-ID
29      CONTINUE
40      NW=NW+ID
        NW=0
        DO 6 J=1,LX
        IF(NW-J) 8,7,7
7       JJ=NW+NW+1
        J1=JJ+1
        JH1=J+J
        JH=JH1-1
        Q1=D(JJ)
        D(JJ)=D(JH)
        D(JH)=Q1
        Q1=D(J1)
        D(J1)=D(JH1)
        D(JH1)=Q1
8       DO 9 I=1,K
        ID=INU(I)
        IL=ID+ID
        IF(NW-ID-IL*(NW/IL)) 6,5,5
5       NW=NW-ID
9       CONTINUE
6       NW=NW+ID
        RETURN
        END
C-------------------------------------------------------------------------------
C     PLOTTING ROUTINES
C     FOR SEISMOGRAMS AND POLARISATION
C-------------------------------------------------------------------------------
      SUBROUTINE BORDER(GLX,IX,GLY,IY,GLH)                              BORDER
C                          draws frame for seismograms
      common /zorg/ x0,y0
      call plot(x0,y0,3)
      A = GLX/FLOAT(IX)
      B = GLY/FLOAT(IY)
      DO 100 J = 1,IX
      C = A*FLOAT(J) + x0
      call plot(C,y0,2)
      call plot(C,y0+GLH,2)
  100 call plot(C,y0,2)
      DO 200 J = 1,IY
      D = B*FLOAT(J) + y0
      call plot(C,D,2)
      call plot(C-GLH,D,2)
  200 call plot(C,D,2)
      DO 300 J = 1,IX
      E = A*FLOAT(IX-J) + x0
      call plot(E,D,2)
      call plot(E,D-GLH,2)
  300 call plot(E,D,2)
      DO 400 J = 1,IY
      F = B*FLOAT(IY-J)  + y0
      call plot(x0,F,2)
      call plot(x0+GLH,F,2)
  400 call plot(x0,F,2)
      call plot(x0,y0,3)
      RETURN
      END                                                               BORDER
      SUBROUTINE BORSCL(SMAX,SMIN,GLS,IS,RMAX,RMIN,GLR,IR)              BORSCL
C                          writes scales on frame
      common /zorg/ x0,y0
      ZM = (SMAX-SMIN)/FLOAT(IS)
      ZN = SMIN
      ZG = y0 - 0.1
      CALL NUMBER(x0-1.2,ZG,0.3,ZN,0.0,41)
      DO 60 IZ = 1,IS
      ZN = ZN+ZM
      ZG = ZG+GLS/FLOAT(IS)
      CALL NUMBER(x0-1.2,ZG,0.3,ZN,0.0,41)
 60   CONTINUE
      ZM = (RMAX-RMIN)/FLOAT(IR)
      ZN = RMIN
      ZG = x0 - 0.6
      CALL NUMBER(ZG,y0-0.5,0.3,ZN,0.0,60)
      DO 61 IZ = 1,IR
      ZN = ZN+ZM
      ZG = ZG+GLR/FLOAT(IR)
      CALL NUMBER(ZG,y0-0.5,0.3,ZN,0.0,60)
 61   CONTINUE
      RETURN
      END                                                               BORSCL
      SUBROUTINE BPOLBX(XWIDS,TLENS,MP)                                 BPOLBX
C                          polarisation indicator box
      character*1 ca,cb,cc,cd
      common /zorg/ x0,y0
      if(mp.eq.3 .or. mp.eq.4) then
       ca = 'U'
       cb = 'D'
       cc = 'T'
       cd = 'A'
      else if(mp.eq.5) then
        ca = 'L'
        cb = 'R'
        cc = 'T'
        cd = 'A'
      else if(mp.eq.6) then
        ca = 'U'
        cb = 'D'
        cc = 'L'
        cd = 'R'
      end if
      v = x0
      t = y0 - 2.0
      va = 0.5*xwids+0.9
      ta = 0.45*tlens
      tb = tlens+0.1
      call pen(2,0)
      call bpvbox(0.5*xwids+0.3,-2.0,xwids,tlens)
      call pen(2,0) 
       call symbol(v,t+tb,0.3,ca,0.0,1)
       call symbol(v,t-0.5,0.3,cb,0.0,1)
       call symbol(v-va,t+ta,0.3,cc,0.0,1)
       call symbol(v+va,t+ta,0.3,cd,0.0,1)
       if(mp.gt.3) return
       call pen(2,0)
       ca = 'L'
       cb = 'R'
       CALL SYMBOL(V+0.4,T+TB,0.3,ca,0.0,1)
       CALL SYMBOL(V+0.4,T-0.1,0.3,cb,0.0,1)
      RETURN
      END                                                               BPOLBX
      SUBROUTINE BPVBOX(A,B,C,D)                                        BPVBOX
C                          polarisation box
      common /zorg/ x0,y0
      E = A+0.5*C + x0
      F = A-0.5*C + y0
      G = B+D + y0
      call plot(E,B + y0,3)
      call plot(E,G,2)
      call plot(F,G,2)
      call plot(F,B + y0,2)
      call plot(A + x0,B + y0,3)
      RETURN
      END                                                               BPVBOX
      SUBROUTINE BSSET(SS,SMIN,SMAX,GMS,SO,DS,NLIM,                     BSSET
     %                 RR,RMIN,RMAX,GMR,RO,I1,I2,ISKIP)
C                          set up parameters for seismogram plotting
C                          time on y-axis
      common /zorg/ x0,y0
      ISKIP = 0
      if(RR.GT.RMAX) go to 90
      RO = (RR-RMIN)*GMR
      IF(RO.LT.0.0)GO TO 90
      IF(SS.GT.SMAX)GO TO 90
      call plot(x0 + RO,y0,3)
      I1 = 1
      SO = (SS-SMIN)*GMS
      IF(SO.GE.0.0)GO TO 70
      I1 = IFIX((SMIN-SS)/DS)+2
      SO = (SS+FLOAT(I1-1)*DS-SMIN)*GMS
 70   call plot(x0 + RO,y0 + SO,2)
      I2 = NLIM
      S1 = SMIN+FLOAT(NLIM-1)*DS
      IF(S1.GT.SMAX) I2 = IFIX((SMAX-SS)/DS+0.1)
      RETURN
 90   ISKIP = 1
      I1 = 1
      I2 = NLIM
      RETURN
      END                                                               BSSET
      SUBROUTINE BSDRAW(ZA,I1,I2,RO,SO,DGMS,GLS,FAC)                     BSDRAW
C                          draw seismograms
C                          time on y-axis
      common /zorg/ x0,y0
      DIMENSION ZA(1)
      T = SO + y0
      call plot(x0 + RO,T,3)
      kk = 1 
      DO 180 J=I1,I2
        T = T+DGMS
        V = x0 + RO+ZA(J)*FAC
        call plot(V,T,2)
        if(kk.eq.250) then
          kk = 0
          call plot(V,T,3)
        endif
        kk = kk+1
 180  continue
      call plot(x0 + RO,T,2)
      call plot(x0 + RO,y0 + GLS,3)
      RETURN
      END                                                               BSDRAW
      SUBROUTINE BSDRAZ(ZA,i1,i2,RO,SO,DGMS,GLS,FAC,TSC,TWR,LS)         BSDRAZ
C                          draw seismograms with variable area fill
C                          and clipping
C                          time on y-axis
      common /zorg/ x0,y0
      logical ls
      dimension ZA(1)
      ylim = twr
      T = SO + y0
      call plot(x0 + RO,T,3)
      kk = 1 
      ts = T
      tr = T
      vb = x0+RO
      vs = 0.0
      vr = 0.0
      DO 180 J=i1,i2
        tr = tr+DGMS
        sfq = 1.0
        if(tsc.gt.0.0) then
         sfr = float(j+1-i1)/float(i2-i1)
         sfq = sfr**tsc
        endif
        vr = ZA(J)*FAC*sfq
        if(vr.gt. ylim) vr= ylim
        if(vr.lt.-ylim) vr=-ylim
        if(ls) then
          if(vr.gt.0.0) then
            call plot(vb+vr,tr,2)
            call plot(vb,tr,2)
            call plot(vb+vs,ts,2)
            call plot(vb+vr,tr,2)
          elseif(vr.le.0.0) then
            call plot(vb+vr,tr,2)
          endif
        else
          call plot(vb+vr,tr,2)
        endif
        if(kk.eq.100) then
          kk = 0
          call plot(vb+vr,tr,3)
        endif
        kk = kk+1
        ts = tr
        vs = vr
 180  continue
      call plot(vb,tr,2)
      call plot(x0 + RO,y0 + GLS,2)
      call plot(x0 + RO,y0 + GLS,3)
      RETURN
      END                                                               BSDRAZ
      SUBROUTINE bssev(SS,SMIN,SMAX,GMS,SO,DS,NLIM,                     bssev
     %                 RO,i1,i2,ISKIP)
C                          set up parameters for seismogram plotting
C                          time on x-axis, no range position in x-axis
      common /zorg/ x0,y0
      ISKIP = 0
      call plot(x0,y0+RO,3)
      IF(SS.GT.SMAX)go to 90
      I1 = 1
      SO = (SS-SMIN)*GMS
      IF(SO.GE.0.0)go to 70
      I1 = IFIX((SMIN-SS)/DS)+2
      SO = (SS+FLOAT(I1-1)*DS-SMIN)*GMS
 70   call plot(x0+SO,y0+RO,3)
      I2 = NLIM
      S1 = SMIN+FLOAT(NLIM-1)*DS
      IF(S1.GT.SMAX) I2 = IFIX((SMAX-SS)/DS+0.1)
      RETURN
 90   ISKIP = 1
      I1 = 1
      I2 = NLIM
      RETURN
      END                                                               bssev
      SUBROUTINE bsdrav(ZA,i1,i2,RO,SO,DGMS,GLS,FAC)                    bsdrav
C                          draw seismograms
C                          time on x-axis
      common /zorg/ x0,y0
      DIMENSION ZA(1)
      T = SO + x0
      call plot(T,y0 + RO,3)
      kk = 1 
      DO 180 J=i1,i2
        T = T+DGMS
        V = y0+RO - ZA(J)*FAC
        call plot(T,V,2)
        if(kk.eq.250) then
          kk = 0
          call plot(T,V,3)
        endif
        kk = kk+1
 180  continue
      call plot(T,y0+RO,2)
      call plot(x0+GLS,y0+RO,2)
      call plot(x0+GLS,y0+RO,3)
      RETURN
      END                                                               bsdrav
      SUBROUTINE bsdray(ZA,i1,i2,RO,SO,DGMS,GLS,FAC,TSC,TWR,LS)         bsdray
C                          draw seismograms with variable area fill
C                          and clipping
C                          time on x-axis
      common /zorg/ x0,y0
      logical ls
      dimension ZA(1)
      ylim = 0.8*twr
      T = SO + x0
      call plot(T,y0 + RO,3)
      kk = 1 
      ts = T
      tr = T
      vb = y0+RO
      vs = 0.0
      vr = 0.0
      DO 180 J=i1,i2
        tr = tr+DGMS
        sfq = 1.0
        if(tsc.gt.0.0) then
         sfr = float(j+1-i1)/float(i2-i1)
         sfq = sfr**tsc
        endif
        vr = ZA(J)*FAC*sfq
        if(vr.gt. ylim) vr= ylim
        if(vr.lt.-ylim) vr=-ylim
        if(ls) then
          if(vr.gt.0.0) then
            call plot(tr,vb+vr,2)
            call plot(tr,vb,2)
            call plot(ts,vb+vs,2)
            call plot(tr,vb+vr,2)
          elseif(vr.lt.0.0) then
            call plot(tr,vb+vr,2)
          endif
        else
          call plot(tr,vb+vr,2)
        endif
        if(kk.eq.100) then
          kk = 0
          call plot(tr,vb+vr,3)
        endif
        kk = kk+1
        ts = tr
        vs = vr
 180  continue
      call plot(tr,vb,2)
      call plot(x0+GLS,y0+RO,2)
      call plot(x0+GLS,y0+RO,3)
      RETURN
      END                                                               bsdray
C===================================================================
C
      SUBROUTINE bframe(X01,Y01,XASL1,YASL1,FX01,XTICL1,DFX1,NXSUB1,
     & NXDEC1,ITX,ITT,sizl1,sizt1,Ipen11,Ipen21,Ipen31)
C
c
C     This subroutine draws a frame
C     Parameters:
C     X0,Y0 = coordinates of lower left corner (cm)
C     XASL,YASL length of x- and y-axis (cm)
C     FX0 = functional value at lower left corner
C     XTICL= distance (cm) between large tics
C     DFX= step in function values between two large tics
C     NXSUB= number of subdivisions with small tics between any two
C                   large tics
C     NXDEC= number of decimals in large tic labels (set -1 for integer)
C     ITX(18)= text for x axis (centered at char 18
C     ITT(18)= title text (max 72 char, start at ITT(1), unlike ITX)
C     sizl = labelsize in cm
C     sizt = textletter size in cm
C     ipen1 frame-box pen
C     ipen2 tics and symbols pen
C     ipen3 text pen
C
      integer ITX(18),ITT(18)
C
c********this section added to obviate problems concerning 
c        passage of constants in subroutine calls
      x0=x01
      y0=y01
      xasl=xasl1
      yasl=yasl1
      fx0=fx01
      xticl=xticl1
      dfx=dfx1
      nxsub=nxsub1
      nxdec=nxdec1
      fy0=fy01
      yticl=yticl1
      sizl=sizl1
      sizt=sizt1
      ipen1=ipen11
      ipen2=ipen21
      ipen3=ipen31
c**********************************************************
      st=sizt
      sl=sizl
      X0I=X0
      Y0I=Y0
      xlow=x0i
      ylow=y0i
      XASLI=XASL
      YASLI=YASL
      xup=x0i+xasli
      yup=y0i+yasli
      call Pen(ipen1,0)
C                            Move to lower left corner and draw frame
      X=X0I
      Y=Y0I
      call Plot(X,Y,3)
      call Plot(x,yup,2)
      call Plot(xup,yup,2)
      call Plot(xup,y,2)
      call Plot(x,y,2)
C                            Compute subdivision parameters
      NX=XASL/XTICL+1.01
      NXSUB=MAX0(1,NXSUB)
      DX=XTICL/NXSUB
C                            Reorigin in function values 
C                            relative to (fx,fy)=(0,0)
      if(fx0.lt.0.and.dfx.gt.0) rox=(ifix(fx0/dfx)-1)*dfx
      if(fx0.ge.0.and.dfx.gt.0) rox=(ifix(fx0/dfx+0.01))*dfx
      if(fx0.lt.0.and.dfx.lt.0) rox=(ifix(fx0/dfx)  )*dfx
      if(fx0.ge.0.and.dfx.lt.0) rox=(ifix(fx0/dfx)-1)*dfx
      x0i=x0i-xticl/dfx *(fx0-rox)
      x=x0i
      y=ylow
      F=rox
      call Plot(xlow,ylow,3)
      call Pen(ipen2,0)
C                            Plot tics and numbers lower x-axis
      DO 20 I=1,NX+1
      DO 10 J=1,NXSUB
      X=X+DX
      if(x.lt.xlow) goto 10
      if(x.gt.xup)  goto 21
      call Plot(X,Y,3)
      call Plot(X,Y+.25,2)
      call Plot(X,Y,3)
   10 CONTINUE
      F=F+DFX
      call Plot(x,y,3)
      call Plot(X,Y+.50,2)
      nxd=nxdec
      call nfig(f,nxd,nf)
      call typnum(X-sl*nf/3,Y-.25-sl,0.9*sl,F,0.,NXD)
      call Plot(X,Y,3)
   20 CONTINUE
   21 continue
      call Pen(ipen3,0)
      call typstr(Xlow+.5*XASLI-2.40,Ylow-2.5*sl-.6,sl,ITX,0.,60)
      call Pen(ipen2,0)
C                            Move to upper left corner
      X=X0I
      Y=Yup
      call Plot(Xlow,ylow,3)
C                            Plot top x-axis
      DO 60 I=1,NX+1
      DO 50 J=1,NXSUB
      X=X+DX
      if(x.lt.xlow) goto 50
      if(x.gt.xup)  goto 61
      call Plot(X,Y,3)
      call Plot(X,Y-.25,2)
      call Plot(X,Y,3)
   50 CONTINUE
      call Plot(X,Y-.40,2)
      call Plot(X,Y,3)
   60 CONTINUE
   61 continue
C                            Title text
      call Pen(ipen3,0)
      call Plot(xup,yup,3)
      X=Xlow
      call Plot(X,Yup+1.50,3)
      call typstr(X,Yup+1.50,st,itt,0.,60)
      call Pen(1,0)
      RETURN
      END
C===================================================================
C
      SUBROUTINE aframe(X01,Y01,XASL1,YASL1,FX01,XTICL1,DFX1,NXSUB1,
     & NXDEC1,FY01,YTICL1,DFY1,NYSUB1,NYDEC1,ITX,ITY,ITT,sizl1,sizt1,
     & Ipen11,Ipen21,Ipen31)
C
c
C     This subroutine draws a frame
C     Parameters:
C     X0,Y0 = coordinates of lower left corner (cm)
C     XASL,YASL length of x- and y-axis (cm)
C     FX0,FY0 = functional values at lower left corner
C     XTICL,YTICL = distance (cm) between large tics
C     DFX,DFY = step in function values between two large tics
C     NXSUB,NYSUB = number of subdivisions with small tics between any two
C                   large tics
C     NXDEC,NYDEC = number of decimals in large tic labels (set -1 for integer)
C     ITX(18),ITY(18)= text for x and y axis respectively (centered at char 18
C     ITT(18)= title text (max 72 char, start at ITT(1), unlike ITX,ITY)
C     sizl = labelsize in cm
C     sizt = textletter size in cm
C     ipen1 frame-box pen
C     ipen2 tics and symbols pen
C     ipen3 text pen
c
c
C
      integer ITX(18),ITY(18),ITT(18)
c     DATA CM/2.54/
C
C
c********this section added to obviate problems concerning passage of constants
c in subroutine calls
      x0=x01
      y0=y01
      xasl=xasl1
      yasl=yasl1
      fx0=fx01
      xticl=xticl1
      dfx=dfx1
      nxsub=nxsub1
      nxdec=nxdec1
      fy0=fy01
      yticl=yticl1
      dfy=dfy1
      nysub=nysub1
      nydec=nydec1
      sizl=sizl1
      sizt=sizt1
      ipen1=ipen11
      ipen2=ipen21
      ipen3=ipen31
c********************************************************************************
C  Formerly convert to inches
      st=sizt
      sl=sizl
      X0I=X0
      Y0I=Y0
      xlow=x0i
      ylow=y0i
      XASLI=XASL
      YASLI=YASL
      xup=x0i+xasli
      yup=y0i+yasli
C
      call Pen(ipen1,0)
C     Move to lower left corner and draw frame
      X=X0I
      Y=Y0I
      call Plot(X,Y,3)
      call Plot(x,yup,2)
      call Plot(xup,yup,2)
      call Plot(xup,y,2)
      call Plot(x,y,2)
c
C
C     Compute subdivision parameters
      NX=XASL/XTICL+1.01
      NY=YASL/YTICL+1.01
      NXSUB=MAX0(1,NXSUB)
      NYSUB=MAX0(1,NYSUB)
      DX=XTICL/NXSUB
      DY=YTICL/NYSUB
c
c     Reorigin in function values relative to (fx,fy)=(0,0)
      if(fx0.lt.0.and.dfx.gt.0) rox=(ifix(fx0/dfx)-1)*dfx
      if(fx0.ge.0.and.dfx.gt.0) rox=(ifix(fx0/dfx)  )*dfx
      if(fx0.lt.0.and.dfx.lt.0) rox=(ifix(fx0/dfx)  )*dfx
      if(fx0.ge.0.and.dfx.lt.0) rox=(ifix(fx0/dfx)-1)*dfx
*
      if(fy0.lt.0.and.dfy.gt.0) roy=(ifix(fy0/dfy)-1)*dfy
      if(fy0.ge.0.and.dfy.gt.0) roy=(ifix(fy0/dfy)  )*dfy
      if(fy0.lt.0.and.dfy.lt.0) roy=(ifix(fy0/dfy)  )*dfy
      if(fy0.ge.0.and.dfy.lt.0) roy=(ifix(fy0/dfy)-1)*dfy
*
      x0i=x0i-xticl/dfx *(fx0-rox)
      y0i=y0i-yticl/dfy *(fy0-roy)
c
      x=x0i
      y=ylow
      F=rox
      call Plot(xlow,ylow,3)
C
*
      call Pen(ipen2,0)
*
C     Plot tics and numbers lower x-axis
      DO 20 I=1,NX+1
      DO 10 J=1,NXSUB
      X=X+DX
      if(x.lt.xlow) goto 10
      if(x.gt.xup)  goto 21
      call Plot(X,Y,3)
      call Plot(X,Y+.25,2)
      call Plot(X,Y,3)
   10 CONTINUE
      F=F+DFX
      call Plot(x,y,3)
      call Plot(X,Y+.50,2)
      nxd=nxdec
      call nfig(f,nxd,nf)
      call typnum(X-sl*nf/3,Y-.25-sl,0.8*sl,F,0.,NXD)
      call Plot(X,Y,3)
   20 CONTINUE
c  21 call numc(itx,nct)
   21 continue
      call Pen(ipen3,0)
      call typstr(Xlow+.5*XASLI-2.40,Ylow-2.5*sl-.6,sl,ITX,0.,60)
      call Pen(ipen2,0)
C
      X=Xlow
      Y=Y0I
      call Plot(Xlow,ylow,3)
      F=roy
C
C     Plot left y-axis
      DO 40 I=1,NY+1
      DO 30 J=1,NYSUB
      Y=Y+DY
      if(y.lt.ylow) goto 30
      if(y.gt.yup)  goto 41
      call Plot(X,Y,3)
      call Plot(X+.25,Y,2)
      call Plot(X,Y,3)
   30 CONTINUE
      F=F+DFY
      call Plot(x,y,3)
      call Plot(X+.40,Y,2)
      nyd=nydec
      call nfig(f,nyd,nf)
      call typnum(X-sl*nf/1.5-.35,Y-sl*0.25,0.8*sl,F,0.,NYD)
c     call typnum(X-.35,Y-sl*nf/3,0.8*sl,F,90.,NYD)
      call Plot(X,Y,3)
   40 CONTINUE
c  41 call numc(ity,nct)
   41 continue
      call Pen(ipen3,0)
      call typstr(Xlow-sl-1.65,Ylow+.5*YASLI-2.40,sl,ITY,90.,60)
c      call typstr(Xlow-sl-1.95,Ylow+.5*YASLI-2.40,sl,ITY,90.,60)
      call Pen(ipen2,0)
C
C     Move to upper left corner
      X=X0I
      Y=Yup
      call Plot(Xlow,ylow,3)
C
C     Plot top x-axis
      DO 60 I=1,NX+1
      DO 50 J=1,NXSUB
      X=X+DX
      if(x.lt.xlow) goto 50
      if(x.gt.xup)  goto 61
      call Plot(X,Y,3)
      call Plot(X,Y-.25,2)
      call Plot(X,Y,3)
   50 CONTINUE
      call Plot(X,Y-.40,2)
      call Plot(X,Y,3)
   60 CONTINUE
   61 continue
C
C
C     Plot right y-axis
      X=Xup
      Y=Y0I
      call Plot(xlow,ylow,3)
      DO 80 I=1,NY+1
      DO 70 J=1,NYSUB
      Y=Y+DY
      if(y.lt.ylow) goto 70
      if(y.gt.yup)  goto 81
      call Plot(X,Y,3)
      call Plot(X-.25,Y,2)
      call Plot(X,Y,3)
   70 CONTINUE
      call Plot(X-.40,Y,2)
      call Plot(X,Y,3)
   80 CONTINUE
   81 continue
C
C     title text
      call Pen(ipen3,0)
      call Plot(xup,yup,3)
      X=Xlow
c     call numc(itt,nct)
      call Plot(X,Yup+1.50,3)
      call typstr(X,Yup+1.50,st,itt,0.,60)
      call Pen(1,0)
      RETURN
      END
c-----
      SUBROUTINE cframe(X01,Y01,XASL1,YASL1,FX01,XTICL1,DFX1,NXSUB1,
     & NXDEC1,FY01,YTICL1,DFY1,NYSUB1,NYDEC1,ITX,ITY,ITT,sizl1,sizt1,
     & Ipen11,Ipen21,Ipen31)
C
c
C     This subroutine draws a frame
C     Parameters:
C     X0,Y0 = coordinates of lower left corner (cm)
C     XASL,YASL length of x- and y-axis (cm)
C     FX0,FY0 = functional values at lower left corner
C     XTICL,YTICL = distance (cm) between large tics
C     DFX,DFY = step in function values between two large tics
C     NXSUB,NYSUB = number of subdivisions with small tics between any two
C                   large tics
C     NXDEC,NYDEC = number of decimals in large tic labels (set -1 for integer)
C     ITX(18),ITY(18)= text for x and y axis respectively (centered at char 18
C     ITT(18)= title text (max 72 char, start at ITT(1), unlike ITX,ITY)
C     sizl = labelsize in cm
C     sizt = textletter size in cm
C     ipen1 frame-box pen
C     ipen2 tics and symbols pen
C     ipen3 text pen
c
c
C
      integer ITX(18),ITY(18),ITT(18)
c     DATA CM/2.54/
C
C
c********this section added to obviate problems concerning passage of constants
c in subroutine calls
      x0=x01
      y0=y01
      xasl=xasl1
      yasl=yasl1
      fx0=fx01
      xticl=xticl1
      dfx=dfx1
      nxsub=nxsub1
      nxdec=nxdec1
      fy0=fy01
      yticl=yticl1
      dfy=dfy1
      nysub=nysub1
      nydec=nydec1
      sizl=sizl1
      sizt=sizt1
      ipen1=ipen11
      ipen2=ipen21
      ipen3=ipen31
c********************************************************************************
C  Formerly convert to inches
      st=sizt
      sl=sizl
      X0I=X0
      Y0I=Y0
      xlow=x0i
      ylow=y0i
      XASLI=XASL
      YASLI=YASL
      xup=x0i+xasli
      yup=y0i+yasli
C
      call Pen(ipen1,0)
C     Move to lower left corner and draw frame
      X=X0I
      Y=Y0I
      call Plot(X,Y,3)
      call Plot(x,yup,2)
      call Plot(xup,yup,2)
      call Plot(xup,y,2)
      call Plot(x,y,2)
c
C
C     Compute subdivision parameters
      NX=XASL/XTICL+1.01
      NY=YASL/YTICL+1.01
      NXSUB=MAX0(1,NXSUB)
      NYSUB=MAX0(1,NYSUB)
      DX=XTICL/NXSUB
      DY=YTICL/NYSUB
c
c     Reorigin in function values relative to (fx,fy)=(0,0)
      if(fx0.lt.0.and.dfx.gt.0) rox=(ifix(fx0/dfx)-1)*dfx
      if(fx0.ge.0.and.dfx.gt.0) rox=(ifix(fx0/dfx)  )*dfx
      if(fx0.lt.0.and.dfx.lt.0) rox=(ifix(fx0/dfx)  )*dfx
      if(fx0.ge.0.and.dfx.lt.0) rox=(ifix(fx0/dfx)-1)*dfx
*
      if(fy0.lt.0.and.dfy.gt.0) roy=(ifix(fy0/dfy)-1)*dfy
      if(fy0.ge.0.and.dfy.gt.0) roy=(ifix(fy0/dfy)  )*dfy
      if(fy0.lt.0.and.dfy.lt.0) roy=(ifix(fy0/dfy)  )*dfy
      if(fy0.ge.0.and.dfy.lt.0) roy=(ifix(fy0/dfy)-1)*dfy
*
      x0i=x0i-xticl/dfx *(fx0-rox)
      y0i=y0i-yticl/dfy *(fy0-roy)
c
      x=x0i
      y=ylow
      F=rox
      call Plot(xlow,ylow,3)
C
*
      call Pen(ipen2,0)
*
C     Plot tics and numbers lower x-axis
      DO 20 I=1,NX+1
      DO 10 J=1,NXSUB
      X=X+DX
      if(x.lt.xlow) goto 10
      if(x.gt.xup)  goto 21
      call Plot(X,Y,3)
      call Plot(X,Y+.25,2)
      call Plot(X,Y,3)
   10 CONTINUE
      F=F+DFX
      call Plot(x,y,3)
      call Plot(X,Y+.50,2)
      nxd=nxdec
      call nfig(f,nxd,nf)
      call typnum(X+sl*nf/3,Y-.25,0.8*sl,F,180.,NXD)
      call Plot(X,Y,3)
   20 CONTINUE
c  21 call numc(itx,nct)
   21 continue
      call Pen(ipen3,0)
      call typstr(Xlow+.5*XASLI+2.40,Ylow-2.5*sl-.2,sl,ITX,180.,60)
      call Pen(ipen2,0)
C
      X=Xlow
      Y=Y0I
      call Plot(Xlow,ylow,3)
      F=roy
C
C     Plot left y-axis
      DO 40 I=1,NY+1
      DO 30 J=1,NYSUB
      Y=Y+DY
      if(y.lt.ylow) goto 30
      if(y.gt.yup)  goto 41
      call Plot(X,Y,3)
      call Plot(X+.25,Y,2)
      call Plot(X,Y,3)
   30 CONTINUE
      F=F+DFY
      call Plot(x,y,3)
      call Plot(X+.40,Y,2)
      nyd=nydec
      call nfig(f,nyd,nf)
      call typnum(X-sl*nf/1.5+.35,Y+sl*0.25,0.8*sl,F,180.,NYD)
c     call typnum(X-.35,Y-sl*nf/3,0.8*sl,F,90.,NYD)
      call Plot(X,Y,3)
   40 CONTINUE
c  41 call numc(ity,nct)
   41 continue
      call Pen(ipen3,0)
      call typstr(Xlow-sl-1.65,Ylow+.5*YASLI-2.40,sl,ITY,90.,60)
c      call +ypstr(Xlow-sl-1.95,Ylow+.5*YASLI-2.40,sl,ITY,90.,60)
      call Pen(ipen2,0)
C
C     Move to upper left corner
      X=X0I
      Y=Yup
      call Plot(Xlow,ylow,3)
C
C     Plot top x-axis
      DO 60 I=1,NX+1
      DO 50 J=1,NXSUB
      X=X+DX
      if(x.lt.xlow) goto 50
      if(x.gt.xup)  goto 61
      call Plot(X,Y,3)
      call Plot(X,Y-.25,2)
      call Plot(X,Y,3)
   50 CONTINUE
      call Plot(X,Y-.40,2)
      call Plot(X,Y,3)
   60 CONTINUE
   61 continue
C
C
C     Plot right y-axis
      X=Xup
      Y=Y0I
      call Plot(xlow,ylow,3)
      DO 80 I=1,NY+1
      DO 70 J=1,NYSUB
      Y=Y+DY
      if(y.lt.ylow) goto 70
      if(y.gt.yup)  goto 81
      call Plot(X,Y,3)
      call Plot(X-.25,Y,2)
      call Plot(X,Y,3)
   70 CONTINUE
      call Plot(X-.40,Y,2)
      call Plot(X,Y,3)
   80 CONTINUE
   81 continue
C
C     title text
      call Pen(ipen3,0)
      call Plot(xup,yup,3)
      X=Xlow+12.
c     call numc(itt,nct)
      call Plot(X,Yup+1.50,3)
      call typstr(X,Yup+1.50,st,itt,180.,60)
      call Pen(1,0)
      RETURN
      END
c-----
      subroutine nfig(f,ndec,nf)
      if(abs(f).lt.0.00001) then
            x=0
      else
            x=alog10(abs(f))
            if(abs(f-1).lt.0.0001) x=0
      endif
      if(x.ge.0) then
            nf=x+2+ndec
      else
            ng=abs(x)+2
            nf=max0(ng,ndec+2)
      endif
      if(f.lt.0) nf=nf+1
c  plotpak use
c     ndec= nf*10+ndec
      return
      end
      subroutine btxypl(X,Y,IT,KSL,IPEN,
     &                  rmin,rmax,smin,smax,glr,gmr,gls,gms)
c--------------------------------------------------------------------
c     btxypl
c                       plotting parametric arrays in a window
c                       rmin,rmax in x length glr scale gmr
c                       smin,smax in y length gls scale gms
c 
c                       pen colour ipen
c                       parametric data: X(1000), Y(1000)
c--------------------------------------------------------------------
      dimension X(1000), Y(1000)
      character*2 ksl,kdl
      common /zorg/x0,y0
c
      call pen(ipen,0)
      call plot(x0,y0,3)
      kp = 3
      xl = 0.0
      yl = 0.0
      nl = 0
      do 500 j=1,it
        xn = (X(j)-rmin)*gmr
        yn = (Y(j)-smin)*gms
        nn = 0
c                              check window (l.r.u.d)
        if(xn.lt.0.)  nn=1
        if(xn.gt.glr) nn=2
        if(yn.lt.0.)  nn=3
        if(yn.gt.gls) nn=4
c
        if(nn.gt.0) go to 200
        if(nl.gt.0) go to 300
        if(j.eq.2.and.ksl.eq.'LI') go to 391
        if(xn.eq.xl.and.yn.eq.yl) go to 400
        if(ksl.eq.'LI') call plot(x0+xn,y0+yn,kp)
        if(ksl.eq.'SY') call csymbl(x0+xn,y0+yn,3,0.20,12)
        kp = 2
        go to 400
c				current point out of window
 200    if(nl.gt.0) goto 400
        goto (210,220,230,240), nn
 210     xe = 0.0
         goto 225
 220     xe = glr
 225     ye = yl+(yn-yl)*(xl-xe)/(xl-xn)
         goto 250
 230     ye = 0.0
         goto 245
 240     ye = gls
 245     xe = xl+(xn-xl)*(yl-ye)/(yl-yn)
 250    if(j.eq.2) goto 392
        if(ksl.eq.'LI') call plot(x0+xe,y0+ye,kp)
        if(ksl.eq.'SY') call csymbl(x0+xe,y0+ye,3,0.20,12)
        kp = 2
        goto 400
c                            previous point out of window
 300    goto (310,320,330,340), nl
 310     xe = 0.0
         goto 325
 320     xe = glr
 325     ye = yl+(yn-yl)*(xl-xe)/(xl-xn)
         goto 390
 330     ye = 0.0
         goto 345
 340     ye = gls
 345     xe = xl+(xn-xl)*(yl-ye)/(yl-yn)
 390    call plot(x0+xe,y0+ye,3)
        if(ksl.eq.'LI') call plot(x0+xn,y0+yn,2)
        goto 400
 391     call plot(x0+xn,y0+yn,3)
         goto400
 392     call plot(x0+xe,y0+ye,3)
 400    nl = nn
        xl = xn
        yl = yn
        kp = 2
 500  continue
      return
      end
