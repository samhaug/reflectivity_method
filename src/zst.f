C---------------------------------------------------------------------------
C+
C+      **  Seisutil   **  :     Zstplot
C+
C+      Plotting of Synthetic Seismograms and/or Travel Times
C+      - Windowed output
C
c         Format Specification for:
c            Theoretical seismograms and
c            Ray Travel Time information 
c         - designed for use with unformatted input/output
c
c For seismograms:
c ---------------
c       nrange  -    number of distances
c       ncomp   -    number of components for each distance
c
c...for each component:   (nrange*ncomp records)
c       range   -    distance in km
c       azim    -    azimuth (degrees)
c       ichar   -    component identifier (char*4)
c       delt    -    time sampling interval
c       ntim    -    number of time points
c       pcal    -    reduction slowness for calculation
c       tcal    -    start time of calculation
c       smax    -    maximum value of seismogram
c       (seis(k),k=1,ntim)  -  time series
c...end component loop
c
c
c For travel times:
c ----------------
c      iray     -    number of ray segments
c
c...for each ray segment 
c      it       -    number of points
c      (d(k),t(k),pq(k),k=1,it)
c               -    distance, time, slowness
c...end ray segment loop
c
C
C****       P.E. HP   LIBRARY REQUIRED
C------------------------------------------------------------------------
C+      B.L.N. KENNETT
C+      R.S.E.S. A.N.U. September 1988   
C+       based on earlier zsplot program
C------------------------------------------------------------------------
C**
*     real*4 za(12000,3),fa(10000,3),sm(3)
*     real*4 zu(12000),zv(12000),zw(12000) 
*     real*4 zr(12000)
      real*4 za(120000,3),fa(100000,3),sm(3)
      real*4 zu(120000),zv(120000),zw(120000) 
      real*4 zr(120000)
      real*4 at(1000),ad(1000),atau(1000),apq(1000),atr(1000)
c
      character*2 kans,kplc,ktt,kframe,ktsl(3)
      character*4 iuchar
      character*20 kfile
      character*40 cword
      character*72 rtext,ttext,title
      logical lse,lnrm,ltt
      integer ipsc(3),iptt(3),npx,mus,mut
      integer  iword(10),irtext(18),ittext(18),ititle(18)
c
      common /zorg/ x0,y0
c
      equivalence (za(1,1),zu(1)),(za(1,2),zv(1)),(za(1,3),zw(1))
      equivalence (irtext(1),rtext),(ittext(1),ttext),(ititle(1),title)
      equivalence (iword(1),cword)
C**
      lse = .false.
      ltt = .false.
      lnrm = .false.
c                                       read in from command file
c                                       read seismograms?
      read(5,502) kans
      call toupper(kans,2)
      if(kans.eq.'YE') lse = .true.
c<se>            
        read(5,502) kplc
        call toupper(kplc,2)
        read(5,*) mus
        im = 11
        do 100 i=1,mus
          read(5,502) kfile  
          if(lse) open(im,file=kfile,form='unformatted')
          im = im+1
 100    continue
        read(5,*) npx
        read(5,*) vrp
        read(5,*) stmin
        read(5,*) xwid,tlen
        read(5,*) ipsc(1),ipsc(2),ipsc(3)
        read(5,502) kans
        call toupper(kans,2)
        if(kans.eq.'YE') lnrm = .true.
        read(5,*) amp,b1,b2,epic,eps
c<se>
c                                       read travel times ?
      read(5,502) kans
      call toupper(kans,2)
      if(kans.eq.'YE') ltt = .true.
c<tt>
        read(5,*) mut
        im = 14
        do 110 i=1,mut
          read(5,502) kfile
          if(ltt) open(im,file=kfile,form='unformatted')
          read(5,502) ktsl(i)
          call toupper(ktsl(i),2)
          read(5,*) iptt(i)
          im = im+1
110    continue
c<tt>
c<fr>                                       frame information
c                               Input of parameters
      x0 = 5.0
      y0 = 4.0
      read(5,*) kframe
      read(5,*) ifont
      if(ifont.eq.0) ifont=3
      read(5,*) rmin,rmax
      read(5,*) glr
      read(5,*) trmin,trmax
      read(5,*) gltr
      read(5,*) dfr,dft
      read(5,*) ddfr,ddft
      read(5,*) nrdec,ntdec
      read(5,*) sizl,sizt
      read(5,502) rtext
      read(5,502) ttext
      read(5,502) title
      ipen1 = 1
      ipen2 = 2
      ipen3 = 2
C
      ir = int(abs(rmax-rmin)/abs(dfr)+0.1)
      itr= int(abs(trmax-trmin)/abs(dft)+0.1)
      rticl=abs(glr/(rmax-rmin)*abs(dfr))
      tticl=abs(gltr/(trmax-trmin)*abs(dft))
      nrsub=abs(dfr/ddfr)
      ntsub=abs(dft/ddft)
C
c<fr>
C					parameter output
      write(6,30) title
      write(6,31) lse,kplc,mus,npx,vrp,stmin,xwid,tlen,
     #            lnrm,ipsc(1),ipsc(2),ipsc(3)
 30   format(1x,'**  ZST:',A,/)
 31   format(
     #" Seismograms     :",1x,l2,/,  
     #" type, # files   :",1x,a,6x,i8,/,
     #" npx             :",i4,/,
     #" vrp,stmin       :",f8.3,1x,f8.3,/,
     #" xwid,tlen       :",f8.3,1x,f8.3,/,
     #" normalise       :",1x,l2,/,
     #" comp. pens      :",i3,1x,i3,1x,i3,/)
      write(6,32) ltt,mut,ktsl,iptt
 32   format(
     #" Travel times    :",1x,l2,/,  
     #" # files         :",i4,/,
     #" style           :",1x,a,2x,a,2x,a,/,
     #" pens            :",1x,i2,2x,i2,2x,i2,/)
      write(6,33)
     %  x0,y0,glr,gltr,rmin,rmax,trmin,trmax,dfr,dft,ddfr,ddft,
     %  nrdec,ntdec,sizl,sizt,rtext,ttext,title,
     %  ifont,ipen1,ipen2,ipen3
 33   format(/
     #" Frame parameters:",/ 
     #" Xor,Yor         :",f8.3,1x,f8.3,"  cm",/,
     #" Raxl,Taxl       :",f8.3,1x,f8.3,"  cm",/,
     #" rmin,rmax       :",f8.3,1x,f8.3,/,
     #" trmin,trmax     :",f8.3,1x,f8.3,/,
     #" Large tic space :",f8.3,1x,f8.3," R,T ",/,
     #" Small tic space :",f8.3,1x,f8.3," R,T ",/,
     #" # dec. in label :",3X,I5,3X,I5,"  R,T",/,
     #" char size text  :",f8.3,"  cm",/,
     #" char size title :",f8.3,"  cm",/,
     #" R-text          :",a,/,
     #" T-text          :",a,/,
     #" Title           :",a,/,
     #" Font            :",i5,/,
     #" Frame pen       :",i5,/,
     #" Ticks pen       :",i5,/,
     #" Text  pen       :",i5)
C                                       set parameters for plotting
      pr = 1.0/vrp
      glh = 0.4
      gmr = glr/(rmax-rmin)
      gmtr = gltr/(trmax-trmin)
      xwids = xwid*gmr
      tlens = tlen*gmtr
      soff = xwids*0.25
C                                       initialize plotting
      open(8,file='zst.ps',status='unknown')
      call Hplots(1,0,8,1)
      call typset(0.0,0.0)
      call zpick(ifont,0,is)
      if(ifont.eq.12) call zpick(5,3,is)
c                                       draw frame
      if(kframe.eq.'NL') then
        call border(glr,ir,gltr,itr,glh)
      elseif(kframe.eq.'FR')then
        CALL aframe
     &  (x0,y0,glr,gltr,rmin,rticl,dfr,nrsub,nrdec,
     &   trmin,tticl,dft,ntsub,ntdec,rtext,ttext,title,
     &   sizl,sizt,ipen1,ipen2,ipen3)
      end if
c<se>					seismograms
      if (lse) then
c                                       polarisation
        if      (kplc.eq.'PA') then
          cword = 'Vert. & Horiz Plane '
          call symbol( 4.0, 0.5,0.30,cword,0.0,20)
          mp = 3
        else if (kplc.eq.'PV') then
          cword = 'Vertical Plane      '
          call symbol( 4.0, 0.5,0.30,cword,0.0,20)
          mp = 4
        else if (kplc.eq.'PH') then
          cword = 'Horizontal Plane    '
          call symbol( 4.0, 0.5,0.30,cword,0.0,20)
          mp = 5
        else if (kplc.eq.'PT') then
          cword = 'Vertical vs Tang.   '
          call symbol( 4.0, 0.5,0.30,cword,0.0,20)
          mp = 6
        endif
        if(kplc(1:1).eq.'P') then
          call bpolbx(xwids,tlens,mp)
          call pen(2,0)
        else  
          call symbol(4.0,0.5,0.3,kplc,0.0,2)
        end if
c
        im = 11
        do 200 i=1,mus
          read(im) nrange,ncomp
          nxc = nrange*ncomp
          im = im+1
 200    continue
c
        do 299 kp=1,npx
          do 205 l=1,120000
            za(l,1) = 0.0
            za(l,2) = 0.0
            za(l,3) = 0.0
 205      continue
          im =11
          npts = 1
          do 250 i=1,mus
            do 220 jc=1,ncomp
              read(im) xx,azim,iuchar,dt,ntim,pcal,tcal,sm(jc)
     %                 ,(fa(ll,jc),ll=1,ntim)
c              write(6,*) xx,azim,iuchar,dt,ntim,pcal,tcal,sm(jc)
c.            toff = tcal+xx*(pcal-pr)-stmin
              toff =  tcal-xx*pr-stmin
              loff = nint(toff/dt)
              do 210 ll=1,ntim
                lc = ll+loff
                za(lc,jc) = za(lc,jc)+fa(ll,jc)
 210          continue
 220        continue
            npta = ntim+loff
            if(npta.gt.npts) npts = npta
            im = im+1
 250      continue
c 
c          write(6,*) npts,loff,toff
          dgmtr = dt*gmtr
          ca = 0.0
          cb = 0.0
          cc = 0.0
          do 251 l=1,npts
            if(abs(za(l,1)).gt.ca) ca = abs(za(l,1))
            if(abs(za(l,2)).gt.cb) cb = abs(za(l,2))
            if(abs(za(l,3)).gt.cc) cc = abs(za(l,3))
 251      continue
          sm(1) = ca
          sm(2) = cb
          sm(3) = cc
          if(kplc.eq.'VA') then
            cr = 0.0
            do 254 ll=1,npts
              zr(ll) = sqrt(zu(ll)**2+zv(ll)**2+zw(ll)**2)
              if(zr(ll).gt.cr) cr = zr(ll)
 254        continue
          endif
c
c                                     normalised plot
          if(lnrm) then
            do 260 jc=1,ncomp
              if(sm(jc).eq.0.0) go to 260
              do 255 ll=1,npts
                za(ll,jc) = za(ll,jc)/sm(jc)
 255          continue
 260        continue
            if(kplc.eq.'VA') then
              do 256 ll=1,npts
                zr(ll) = zr(ll)/cr
 256          continue
            endif
          endif
c
          call pen(2,0)
          call bsset(stmin,trmin,trmax,gmtr,to,dt,npts,
     &               xx,rmin,rmax,gmr,vo,i1,i2,iskip)
          write(6,*) kplc,xx,to,i1,i2,iskip,ca,cb,cc
          if(iskip.eq.1) go to 299
          t = t0
C                                     DRAW SEISMOGRAMS
C                                      vertical - black
C                                      radial   - red
C                                       tangential - green
C
          if(lnrm) then
             afactr = amp
          else
             afactr = 1000.0*amp*(b1+b2*(xx/epic))**eps
          endif
c          write(6,*) xx,afactr
          if(kplc.eq.'3C') then
            call pen(ipsc(1),0)
            VB = VO-SOFF
            CALL BSDRAW(zu,I1,I2,VB,TO,DGMTR,GLTR,AFACTR)
            call pen(ipsc(2),0)
            VB = VO
            CALL BSDRAW(zv,I1,I2,VB,TO,DGMTR,GLTR,AFACTR)
            call pen(ipsc(3),0)
            VB = VO+SOFF
            CALL BSDRAW(zw,I1,I2,VB,TO,DGMTR,GLTR,AFACTR)
          else if (kplc.eq.'ZC') then
            call pen(ipsc(1),0)
            VB = VO-SOFF
            CALL BSDRAW(zu,I1,I2,VB,TO,DGMTR,GLTR,AFACTR)
          else if (kplc.eq.'RC') then
            call pen(ipsc(2),0)
            VB = VO
            CALL BSDRAW(zv,I1,I2,VB,TO,DGMTR,GLTR,AFACTR)
          else if (kplc.eq.'TC') then
            call pen(ipsc(3),0)
            VB = VO+SOFF
            CALL BSDRAW(zw,I1,I2,VB,TO,DGMTR,GLTR,AFACTR)
          else if (kplc.eq.'VA') then
            call pen(ipsc(3),0)
            VB = VO+SOFF
            CALL BSDRAW(zr,I1,I2,VB,TO,DGMTR,GLTR,AFACTR)
          endif
c<pol>
          if(kplc(1:1).eq.'P') then
            cd = 1.0
            nlen = nint(tlen/dt)
            i3 = i2-nlen 
            if(mp.lt.4) then
              cd = (ca+cb+cc)/3.0
            else if(mp.eq.4) then
              cd = (ca+cb)/2.0
            else if(mp.eq.5) then
              cd = (cb+cc)/2.0
            else if(mp.eq.6) then
              cd = (ca+cc)/2.0
            end if
c            write(6,*) cd
            sc = xwids*0.6/cd
c<pol>                               DRAW POLARISATION DIAGRAMS
            NA = I1
            DO 600 J = I1,I3,NLEN
              TA = T+0.5*TLEN*GMTR
              V = VO
              call pen(2,0)
c              CALL BPVBOX(V,T,XWIDS,TLENS)
              NB = NA+NLEN
              MQ = MP-2
c             write(6,*) na,nb,mq
              go to (571,571,572,573), MQ
C                                     radial vs vertical (red)
 571          call pen(3,0)
              DO 580 JN=NA,NB
                XS = za(JN,2)*SC+V  +x0
                YS = za(JN,1)*SC+TA  +y0
                IF(JN.EQ.NA) call plot(XS,YS,3)
                call plot(XS,YS,2)
 580          CONTINUE
              goto (572,574,572,573), MQ
C                                     radial vs tangential (green)
 572          call pen(5,0)
              DO 590 JN=NA,NB
                XS = za(JN,2)*SC+V +x0
                YS = za(JN,3)*SC+TA  +y0
                IF(JN.EQ.NA) call plot(XS,YS,3)
                call plot(XS,YS,2)
 590          CONTINUE
              goto (574,574,574,573), MQ
C                                     vertical vs tangential (blue)
 573          call pen(4,0)
              DO 596 JN=NA,NB
                XS = za(JN,1)*SC+V +x0
                YS = za(JN,2)*SC+TA  +y0
                IF(JN.EQ.NA) call plot(XS,YS,3)
                call plot(XS,YS,2)
 596          CONTINUE
c
 574          NA = NB
              T = T+TLEN*GMTR
 600        CONTINUE
          endif
c<pol>
 299    continue
c                             end of seismogram distance loop
      endif
c<se>
c<tt>                                travel times
      if(ltt) then
        im = 14
c                                    loop over input units
        do 900 i=1,mut
          read(im) iray
          do 799 kt=1,iray
            read(im) it,(ad(k),at(k),apq(k),k=1,it)
            write(6,*) '<tt>',im,iray,it
            do 720 jt=1,it
              atr(jt)  = at(jt) - pr    *ad(jt)
              atau(jt) = at(jt) - apq(jt)*ad(jt)
 720        continue
            call btxypl(AD,ATR,IT,ktsl(i),iptt(i),
     &                  rmin,rmax,trmin,trmax,glr,gmr,gltr,gmtr)
 799      continue
          im = im+1
 900    continue
      endif
c<tt>
c
      call Hplots(0,0,8,1)
C
      STOP
 502    format(a)
      END
