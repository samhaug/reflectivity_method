C-----------------------------------------------------------
C  RPOSTPAK:  SEISMOLOGY GROUP 
C         RESEARCH SCHOOL OF EARTH SCIENCES
C         AUSTRALIAN NATIONAL UNIVERSITY
C-----------------------------------------------------------
c
c     RPOSTPAK    - low level plotting routines
c             to support Postscript output
c             using plotpak calls
c             together with some typset calls
c             implemented to exploit the font 
c             capabilities in Postscript
c
c     Revision History
c      version 1.0 - Sept. 1988                  blnk
c      version 1.1 - April 1989                  mb    
c      version 1.2 - pimask added July 1989          blnk
c     rewritten to include structured Postscript
c      version 2.0 - March 1997                  blnk
c      version 2.1 - April 1997 (additional routines)  blnk
c
c     supported Laserwriter fonts (these may not be available
c      on all printers)
c
c     1      Helvetica
c     2      Helvetica-Bold
c     3      Palatino-Roman  
c     4      Times-Roman
c     5      Helvetica-Oblique
c     6      Palatino-Italic
c     7      Times-Italic
c     8      Times-Bold
c     9      Times-BoldItalic
c     10     Symbol
c     11     Palatino-Bold
c     12     Palatino-BoldItalic
c     13     LucidaSans
c     14     LucidaSans-Italic
c     15     LucidaSans-Bold
c     16     LucidaSans-BoldItalic
c     17     Helvetica-BoldOblique
C
C    Default Pen conventions
c     1      thick line (black)
c     2      thin line (black)	
c     3      red
c     4      blue
c     5      green
c     6      yellow
c     7      orange
c     8      tan
c     9      beige
c     10     purple
c     11     pink
c     12     lavender
c     13     light green
c     14     light blue
c     15     light grey
c
c     16-31  shades of gray (16-black, 31-white)
C---------------------------------------------------------------bk
      subroutine HPLOTS(ION,IRO,LPL,ILS)
C
C    Initialises plotter : if ILS .eq. 0  -  mapped from A4 paper
C                  if ILS .eq. 1  -  mapped from A3 paper
C      and establishes handshaking characteristics (ION=1).
C      Terminates plot file if  ION .ne. 1
C
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
*      character*40 iword
      IF(ION.eq.1) THEN
C
      A=1.0
      B=0.0
      C=1.0
      D=0.0
      asp = 0.6666
      IL34 = ils
      LPLOT=LPL
      IROT=IRO
      do 31 j=1,256
        icolr(j) = 0
        icolg(j) = 0
        icolb(j) = 0
 31   continue
      xorig = 0.
      yorig = 0.
      ipage = 1
      iobj = 1
C
c       Postscript initialisation
      write(LPLOT,fmt='(a)') '%!PS-Adobe-2.0'
      write(LPLOT,fmt='(a)') '%%Creator: ps_rpost 2.1'
      write(LPLOT,fmt='(a)') '%%Pages: (atend)'
      write(LPLOT,fmt='(a)') '%%BoundingBox: (atend)'
      write(LPLOT,fmt='(a)') '%%End Comments'
      write(LPLOT,fmt='(a)') '%%BeginProlog'
      write(LPLOT,fmt='(a)') ' '
      write(LPLOT,fmt='(a)') '% operator abbreviations'
      write(LPLOT,fmt='(a)') '/pM {stroke newpath moveto} def'
      write(LPLOT,fmt='(a)') '/pL {lineto} def'
      write(LPLOT,fmt='(a)') '/rM {rmoveto} def'
      write(LPLOT,fmt='(a)') '/rL {rlineto} def'
      write(LPLOT,fmt='(a)') '/sh  {show} def'
      write(LPLOT,fmt='(a)') '/ss {show stroke} def'
      write(LPLOT,fmt='(a)') '/sC {setrgbcolor} def'
      write(LPLOT,fmt='(a)') '/sH {sethsbcolor} def'
      write(LPLOT,fmt='(a)') '/sL {setlinewidth} def'
      write(LPLOT,fmt='(a)') '/sG {setgray} def'
      write(LPLOT,fmt='(a)') '/cl {closepath} def'
      write(LPLOT,fmt='(a)') '/cf {closepath fill} def'
      write(LPLOT,fmt='(a)') '/rot {rotate} def'
      write(LPLOT,fmt='(a)') '/cap {/setlinecap} def'
      write(LPLOT,fmt='(a)') '2 setlinejoin'
      write(LPLOT,fmt='(a)') '% page state control'
      write(LPLOT,fmt='(a)') '/bplot {gsave} def'
      write(LPLOT,fmt='(a)') '/eplot {stroke grestore} def'
      write(LPLOT,fmt='(a)') '%'
      write(LPLOT,fmt='(a)') '/th9  {3.0 sL} def'
      write(LPLOT,fmt='(a)') '/th8  {2.5 sL} def'
      write(LPLOT,fmt='(a)') '/th7  {2.0 sL} def'
      write(LPLOT,fmt='(a)') '/th6  {1.5 sL} def'
      write(LPLOT,fmt='(a)') '/th5  {1.25 sL} def'
      write(LPLOT,fmt='(a)') '/th4  {1.00 sL} def'
      write(LPLOT,fmt='(a)') '/th3  {0.75 sL} def'
      write(LPLOT,fmt='(a)') '/th2  {0.50 sL} def'
      write(LPLOT,fmt='(a)') '/th1  {0.25 sL} def'
      write(LPLOT,fmt='(a)') '/th0  {0.10 sL} def'
      write(LPLOT,fmt='(a)') '%'
      write(LPLOT,*) 
     ^'/f1 {/Helvetica findfont exch scalefont setfont} def'
      write(LPLOT,*)  
     ^'/f2 {/Helvetica-Bold findfont exch scalefont setfont} def'
      write(LPLOT,*)  
     ^'/f3 {/Palatino-Roman findfont exch scalefont setfont} def'
      write(LPLOT,*)  
     ^'/f4 {/Times-Roman findfont exch scalefont setfont} def'
      write(LPLOT,*)  
     ^'/f5 {/Helvetica-Oblique findfont exch scalefont setfont} def'
      write(LPLOT,*)  
     ^'/f6 {/Palatino-Italic findfont exch scalefont setfont} def'
      write(LPLOT,*)  
     ^'/f7 {/Times-Italic findfont exch scalefont setfont} def'
      write(LPLOT,*)  
     ^'/f8 {/Times-Bold findfont exch scalefont setfont} def'
      write(LPLOT,*)  
     ^'/f9 {/Times-BoldItalic findfont exch scalefont setfont} def'
      write(LPLOT,*)  
     ^'/f10 {/Symbol findfont exch scalefont setfont} def'
      write(LPLOT,*)  
     ^'/f11 {/Palatino-Bold findfont exch scalefont setfont} def'
      write(LPLOT,*)  
     ^'/f12 {/Palatino-BoldItalic findfont exch scalefont',
     ^' setfont} def'
      write(LPLOT,*)  
     ^'/f13 {/LucidaSans findfont  exch scalefont setfont} def'
      write(LPLOT,*)  
     ^'/f14 {/LucidaSans-Italic findfont exch scalefont',
     ^' setfont} def'
      write(LPLOT,*)  
     ^'/f15 {/LucidaSans-Bold findfont exch scalefont setfont} def'
      write(LPLOT,*)  
     ^'/f16 {/LucidaSans-BoldItalic findfont exch scalefont',
     ^' setfont} def'
      write(LPLOT,*)  
     ^'/f17 {/Helvetica-BoldOblique findfont exch scalefont',
     ^' setfont} def'
      write(LPLOT,fmt='(a)') '%'
      call colint
      write(LPLOT,fmt='(a)') '%'
      write(LPLOT,fmt='(a)') '%%EndProlog'
      write(LPLOT,fmt='(a)') '%%Page: 1 1'
      write(LPLOT,fmt='(a)') '%%BeginObject: sheet 1'
      write(LPLOT,fmt='(a)') ' '
      write(LPLOT,fmt='(a)') 'bplot'
      if(IROT.eq.0) then
        write(LPLOT,fmt='(a)') '590 5 translate '
        write(LPLOT,fmt='(a)') '90 rotate '
        write(LPLOT,fmt='(a)') 'newpath '
      endif

c
      if ( IL34 .eq. 0 )  then
      PSCA = 72.0/2.54
C
      else if (IL34 .eq. 1) then
      PSCA = 72.0/(SQRT(2.0)*2.54)
C
      else if (IL34 .eq. 2) then
C    initialise plotter to allow convenient screen mapping
C    (1cm = 10 points)
C
      PSCA = 10.0
C
      end if
C
C    Select pen #1
C
      thw = 1.0
      call pen(1,0)
C
      RETURN
C
C     Postscript close
C
      ELSE IF (ION.ne.1) THEN
      write(LPLOT,fmt='(a)') 'eplot'
      write(LPLOT,fmt='(a,i3)') '%%EndObject sheet',iobj
      write(LPLOT,fmt='(a)') 'showpage'
      write(LPLOT,fmt='(a)') '%%Trailer'
      write(LPLOT,fmt='(a)') '%%BoundingBox:    0    0   590   840'
      write(LPLOT,fmt='(a,i3)') '%%Pages: ',ipage
      write(LPLOT,fmt='(a)') '%%EOF'
      write(LPLOT,*) 
      close(LPLOT)
       RETURN
      ENDIF
C
      END
C---------------------------------------------------------------bk
      subroutine newpage
C
C     starts a new page in postscript file
C
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
c
      write(LPLOT,fmt='(a)') 'eplot'
      write(LPLOT,fmt='(a,i3)') '%%EndObject sheet ',iobj
      write(LPLOT,fmt='(a)') 'showpage'
      iobj = iobj+1
      ipage = ipage+1
      write(LPLOT,fmt='(a,i3,i3)') '%%Page: ',ipage,ipage
      write(LPLOT,fmt='(a,i3)') '%%BeginObject: sheet ',iobj
      write(LPLOT,fmt='(a)') ' '
      write(LPLOT,fmt='(a)') 'bplot'
      if(IROT.eq.0) then
        write(LPLOT,fmt='(a)') '590 5 translate '
        write(LPLOT,fmt='(a)') '90 rotate '
        write(LPLOT,fmt='(a)') 'newpath '
      endif
      return 
      end
C---------------------------------------------------------------bk
      subroutine newplot
C
C     starts a new plot object in postscript file
C
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
c
      write(LPLOT,fmt='(a)') 'eplot'
      write(LPLOT,fmt='(a,i3)') '%%EndObject sheet ',iobj
      iobj = iobj+1
      write(LPLOT,fmt='(a,i3)') '%%BeginObject: sheet ',iobj
      write(LPLOT,fmt='(a)') ' '
      write(LPLOT,fmt='(a)') 'bplot'
      return 
      end
C-----------------------------------------------------------------
C  ROUTINES to use Postscript basic commands
C-----------------------------------------------------------------
      subroutine qrotate(ang)
C
C     to rotate graphics - angle in degrees
c
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
c
      write(lplot,'(f8.2,a)') ang,' rot'
c
      return
      end
C-----------------------------------------------------------------
      subroutine qtransl(ptx,pty)
C
C     to translate graphics - ptx,pty in points
c
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
c
      write(lplot,fmt='(f8.2,a)') ptx,pty,' translate'
c
      return
      end
C-----------------------------------------------------------------
      subroutine qscale(sc)
c
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
c
      write(lplot,fmt='(2f7.3,a)') sc,sc,' scale'
c
      return
      end
C-----------------------------------------------------------------
      subroutine qsetlw(thw)
C
C     specify linewidth (in points)
c
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
c
      write(lplot,fmt='(f5.2,a)') thw,' sL'
c
      return
      end
C-----------------------------------------------------------------
C  ROUTINES affecting COLOUR and DRAWING STYLE
C-----------------------------------------------------------------
      subroutine DASHLN(LDASH,LPAT)
C
C     defines  the style for line drawing
C      ldash < 0   -  reset to solid line
C       or = 12
C
C      ldash = 0   -  dots at calling points
C          = 1   -  dots
C          = 2   -  half dash
C          = 3   -  long dash
C          = 4   -  chain dotted
C          = 5   -  long and short
C          = LPL,*   -  long and two short
C
C       lpat - percentage of diagonal of paper used for
C            a pattern
C            if (lpat.eq.0 ) lpat = 2
C
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
C      CHARACTER*40 IWORD
C
      integer*4 ldash,lpat
      write(LPLOT,fmt='(a)') ' stroke' 
      if(ldash.lt.0)  write(LPLOT,fmt='(a)') '[] 0 setdash'
      if(ldash.eq.12) write(LPLOT,fmt='(a)') '[] 0 setdash'
c
      if(ldash.eq.0) write(LPLOT,fmt='(a)') '[1 8] 0 setdash'
      if(ldash.eq.1) write(LPLOT,fmt='(a)') '[2 2] 0 setdash'
      if(ldash.eq.2) write(LPLOT,fmt='(a)') '[4 4] 0 setdash'
      if(ldash.eq.3) write(LPLOT,fmt='(a)') '[8 8] 0 setdash'
      if(ldash.eq.4) write(LPLOT,fmt='(a)') '[6 2 2 2] 0 setdash'
      if(ldash.eq.5) write(LPLOT,fmt='(a)') '[8 4 4 4] 0 setdash'
      if(ldash.eq.6) write(LPLOT,fmt='(a)') '[6 4 4 4 4 4] 0 setdash'
      xv=psca*xo
      yv=psca*yo
      write(LPLOT,fmt='(2f8.2,a)') xv,yv,' pM'
C
      return
      end
C-----------------------------------------------------------------
      subroutine THICKLIN(ithw)
C
C     specify linewidth (- preset values)
c
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
c
      write(lplot,fmt='(a,i1)') 'th',ithw
c
      return
      end
C-------------------------------------------------------------bk-
      SUBROUTINE PEN(IPEN,ITHK)
C
C     allows choice of pen colour and thickness
C        ITHW > 1 progessively thicker lines
C        ITHW < 1 progressively thinner lines  
C    
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      COMMON/PC0000/colr(256),colg(256),colb(256)
      COMMON/L00000/PSCA,xo,yo
c
      thw = 0.8
      if(ipen.eq.1) thw = 1.0
      write(lplot,fmt='(a)') ' stroke'
      if(ipen.lt.10) then
        write(lplot,75) ipen
      elseif(ipen.ge.10.and.ipen.lt.100) then
        write(lplot,76) ipen
      elseif(ipen.ge.100) then
        write(lplot,77) ipen
      endif
 75   format(1x,'pen',i1)
 76   format(1x,'pen',i2)
 77   format(1x,'pen',i3)
      if(ithk.ge.0) then
        thw = thw*(1.0+0.5*float(ITHK))
        write(lplot,78) thw
      elseif(ithk.le.-1) then
        thw = thw/float(-ithk+1)
        write(lplot,78) thw
      else
      endif
 78   format(1x,f5.2,' sL ')
      xv=psca*xo
      yv=psca*yo
      write(LPLOT,fmt='(2f8.2,a)') xv,yv,' pM'
      RETURN
      END
C--------------------------------------------------------------bk--
      SUBROUTINE COLINT
C
C     set up colour values for colours up to 31 
C     (white background)
C
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
C 
      dimension ICOL(96)
      data icol/ 255,255,255,  0,  0,  0,  0,  0, 63,255, 31, 31,
     &          63, 63,255, 31,255, 31,225,225, 31,225,139, 31,
     &         191,127, 63,225,191, 97,159, 63,159,225,139,139,
     &         225,139,225,139,139,225,139,225,139,159,159,159,
     &          15, 15, 15, 31, 31, 31, 47, 47, 47, 63, 63, 63, 
     &          79, 79, 79, 95, 95, 95,111,111,111,127,127,127,
     &         143,143,143,159,159,159,175,175,175,191,191,191,
     &         207,207,207,223,223,223,239,239,239,255,255,255/ 
c    &          19, 65, 61, 37, 86, 79, 56, 98, 87, 74,110, 95,
c    &          92,121,103,109,132,111,127,143,118,144,154,126,
c    &         160,164,133,176,175,140,191,184,147,206,194,153, 
c    &         220,203,159,233,211,165,245,219,170,255,255,245/
       do 10 ic=1,32         
         ik = (ic-1)*3
         icolr(ic) = icol(ik+1)
         icolg(ic) = icol(ik+2)
         icolb(ic) = icol(ik+3)
         cr = float(icol(ik+1))/255.
         cg = float(icol(ik+2))/255.  
         cb = float(icol(ik+3))/255.
         if(ic.le.10) then
           write(lplot,fmt="('/pen',i1,' {',3f7.4,' sC} def')")
     ^       ic-1,cr,cg,cb
         elseif(ic.gt.10) then
           write(lplot,fmt="('/pen',i2,' {',3f7.4,' sC} def')")
     ^       ic-1,cr,cg,cb
         endif
 10    continue
       do 20 ic=33,256
         icolr(ic) = 255
         icolg(ic) = 255
         icolb(ic) = 255
 20    continue
       RETURN
       END
c------------------------------------------------------------------
      subroutine LDCOLR(lunit)
c
c     load a colour map for pens > 32 from lunit
c
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
      read(lunit,*) ncol
      do 10 j=1,ncol
        read(lunit,*) k,ir,ig,ib
        ic = k
        icolr(k+1) = ir
        icolg(k+1) = ig
        icolb(k+1) = ib
        cr = float(ir)/255.
        cg = float(ig)/255.
        cb = float(ib)/255.
        if(ic.le.10) then
          write(lplot,fmt="('/pen',i1,' {',3f7.4,' sC} def')")
     ^       ic,cr,cg,cb
        elseif(ic.gt.10.and.ic.lt.100) then
          write(lplot,fmt="('/pen',i2,' {',3f7.4,' sC} def')")
     ^       ic,cr,cg,cb
        elseif(ic.gt.100) then
          write(lplot,fmt="('/pen',i3,' {',3f7.4,' sC} def')")
     ^       ic,cr,cg,cb
       endif
 10   continue
      RETURN
      END
C-----------------------------------------------------------------
      subroutine RGBCOL(ipen,ir,ig,ib)
c
c     define a pen in terms of rgb input (integers less than 255)
c
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
      ic = ipen+1
      icolr(ic) = ir
      icolg(ic) = ig
      icolb(ic) = ib
      cr = float(ir)/255.
      cg = float(ig)/255.
      cb = float(ib)/255.     
      if(ipen.lt.10) then
          write(lplot,fmt="('/pen',i1,' {',3f7.4,' sC} def')")
     ^       ipen,cr,cg,cb
      elseif(ipen.ge.10.and.ic.le.99) then
          write(lplot,fmt="('/pen',i2,' {',3f7.4,' sC} def')")
     ^       ipen,cr,cg,cb
      elseif(ipen.gt.99) then
          write(lplot,fmt="('/pen',i3,' {',3f7.4,' sC} def')")
     ^       ipen,cr,cg,cb
      endif
      RETURN
      END
C-----------------------------------------------------------------
      subroutine HSBCOL(ipen,HC,SC,BC)
c
c     define a pen in terms of hue, saturation and brightness
c     hc,sc,bc  real variables < 1.0
c
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
  
      if(ipen.lt.10) then
          write(lplot,fmt="('/pen',i1,' {',3f7.4,' sH} def')")
     ^       ipen,hc,sc,bc
      elseif(ipen.ge.10.and.ipen.lt.100) then
          write(lplot,fmt="('/pen',i2,' {',3f7.4,' sH} def')")
     ^       ipen,hc,sc,bc
      elseif(ipen.ge.100) then
          write(lplot,fmt="('/pen',i3,' {',3f7.4,' sH} def')")
     ^       ipen,hc,sc,bc
      endif
      RETURN
      END
C-----------------------------------------------------------------
      subroutine GREYTON(ipen,GC)
c
c     define a pen in terms of greytone
c     gc  real variable < 1.0
c
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
  
      if(ipen.lt.10) then
          write(lplot,fmt="('/pen',i1,' {',f7.4,' sG} def')")
     ^       ipen,gc
      elseif(ipen.ge.10.and.ipen.lt.100) then
          write(lplot,fmt="('/pen',i2,' {',f7.4,' sG} def')")
     ^       ipen,gc
      elseif(ipen.ge.100) then
          write(lplot,fmt="('/pen',i3,' {',f7.4,' sG} def')")
     ^       ipen,gc
      endif
      RETURN
      END
C
C-----------------------------------------------------------------
C  BASIC PLOT ROUTINES
C-----------------------------------------------------------------
      subroutine PLOT(X,Y,I)
C
C     Raises (I=3) or lowers (I=2) pen and moves to coordinates
C      (X,Y) if I>0 or to current position plus (X,Y) if I<0
C
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
C      CHARACTER*40 IWORD
      DATA IUP/1/
      II=IABS(I)
C
C     Rotate plot by 90 degrees if necessary
C
      XP=X
      YP=Y
      IF(IROT.EQ.0)GO TO 30
      YP=X
      XP=-Y
      if(il34.eq.0) then
       if(I.GT.0) XP = 27.2-Y
      else if (il34.eq.1) then
       if(I.GT.0) XP = 40.1-Y
      end if
C
C    convert to points
C
   30 XV= PSCA*XP
      YV= PSCA*YP
C
C     plot
C
      if(I.eq.2)  write(LPLOT,fmt='(2f8.2,a)') xv,yv,' pL'
      if(I.eq.3)  write(LPLOT,fmt='(2f8.2,a)') xv,yv,' pM'
      if(I.eq.-2) write(LPLOT,fmt='(2f8.2,a)') xv,yv,' rL'
      if(I.eq.-3) write(LPLOT,fmt='(2f8.2,a)') xv,yv,' rM'
c 
c      if(I.gt.0) then
       xo = x   
       yo = y
c      endif
      RETURN
      END
C------------------------------------------------------------------
      subroutine PLOTU(X,Y,II)
C
C     scales user coordinates to plotter coordinates
C
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
      XP=A*X
      YP=C*Y
      IF(II.LT.0)GO TO 10
      XP=XP+B
      YP=YP+D
   10 CALL PLOT(XP,YP,II)
      RETURN
      END
C---------------------------------------------------------gh--bk-
      subroutine SCALE(XMIN,XMAX,PX1,PX2,YMIN,YMAX,PY1,PY2)
C
C      sets up scale factors used in PLOTU and other routines
C
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
      common/p00002/a1,a2,b1,b2,c1,c2,d1,d2
c
      A=(PX2-PX1)/(XMAX-XMIN)
      B=PX1-A*XMIN
      C=(PY2-PY1)/(YMAX-YMIN)
      D=PY1-C*YMIN
c
      a1 = xmin
      a2 = xmax
      b1 = px1
      b2 = px2
      c1 = ymin
      c2 = ymax
      d1 = py1
      d2 = py2
c
      RETURN
      END
C
C-----------------------------------------------------------------
C  CIRCLES AND SYMBOLS
C-----------------------------------------------------------------
      subroutine CIRCLE(RADIUS,NSIDES)
C
C    draws circle centred at current pen location, with
C    circumference divided into NSIDES straight segments
C
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
C      CHARACTER*40 IWORD
C
      cx = xo
      cy = yo
      nsid = NSIDES
      if(nsid.EQ.0) nsid = 72
      rpa = radius
      ANG = 6.283185308/float(nsid)
      xv = rpa+cx
      yv = cy
      call plot(xv,yv,3)
      sta=0.0
      do 30 i=1,nsid
      sta = sta+ang
      xv = rpa*cos(sta)+cx
      yv = rpa*sin(sta)+cy
      call plot(xv,yv,2)
 30   continue
      xo = cx
      yo = cy
      RETURN
      END
C----------------------------------------------------------------------
      subroutine CSYMBL(X,Y,IP,SIZE,INT)
C
C      writes a centered symbol at location (X,Y). The symbol is
C      is selected from the list below by INT for 1<INT<10
C      and is circle,triangle,square,pentagon,hexagon,heptagon,
C      octagon for 11<INT<17
C
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
      CHARACTER*1 ISYM
C      CHARACTER*40 IWORD
      DIMENSION ISYM(10)
      DIMENSION ICIR(10)
      DATA ISYM/'O','X','*','+','#','$','@','8','H','Z'/
      DATA ICIR/20,3,4,5,6,7,8,9,10,11/
      iny = INT
      ifill = 0
      if(iny.GT.20) then
        iny = iny-10
        ifill=1
      endif
C     write (6,*) iny,ifill
      IF(iny.GE.11)GO TO 20
C
C    select character size
C
      call plotu(x,y,ip)
      call symbol(x-0.5*size,y-0.5*size,size,isym(int),0.0,1)
      return
c
C     move pen to symbol location, symbol is written after move
C
   20 CALL PLOTU(X,Y,IP)
      CALL CIRCLE(SIZE*0.75,ICIR(iny-10))
      if(ifill.eq.1) then
        write(LPLOT,*) 'cf'
      endif
C
      RETURN
      END
C
C-----------------------------------------------------------------
C  SIMPLE ANNOTATION - basic font
C-----------------------------------------------------------------
      subroutine NUMBER(X,Y,SIZE,RN,ANGL,NSF)
C
C     writes a number on the plot: if NSF=klm, format is Fkl.m
C      if NSF=-lm, RN is fixed to an integer and format is Ilm.
C
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
      CHARACTER*8 IFORM
      CHARACTER*40 IWORD
C
C    create the format expression in IFORM
C
      IF(NSF.LT.0)GO TO 20
      ITOT=NSF/10
      IDPL=MOD(NSF,10)
      Write(IFORM,55)ITOT,IDPL
   55 FORMAT('(F',I2,'.',I1,')')
      Write(IWORD,IFORM)RN
      GO TO 30
C
C    for integer format
C
   20 ITOT=-NSF
      Write(IFORM,65)ITOT
   65 FORMAT('(I',I2,')   ')
      IR=IFIX(RN)
      Write(IWORD,IFORM)IR
C
C     encode number and send to plotter
C
   30 CALL SYMBOL(X,Y,SIZE,IWORD,ANGL,ITOT)
      RETURN
      END
C-----------------------------------------------------------------
      subroutine SYMBOL(X,Y,SIZE,IWORD,ANGL,NCHAR)
C
C     writes a Hollerith string on the plot--plotter units
C
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
      CHARACTER*80 IWORD
      CHARACTER*80 CWORD
      nch = nchar
      IF(NCHAR.GT.80)NCH=80
C
C     select character orientation
C
      ab=0.0
      if(irot.ne.0) ab=90.0
C
C     select character size
C
   40 SZ=ABS(SIZE)*PSCA*1.5
      do 50 k=1,80
      cword(k:k) = iword(k:k)
   50 continue
      do 51 k=nch+1,80
      cword(k:k) = ' '
   51 continue
C
C      move pen to symbol location
C
      IP=3
      IF(SIZE.LT.0.0)IP=-3
      CALL PLOT(X,Y,IP)
      ang=angl+ab
      bng = -ang 
      if(ang.NE.0.0) write(LPLOT,fmt='(f8.2,a)') ang,' rot'
C
c     write character string
C
      write(LPLOT,fmt='(f8.2,x,a,x,a,a,a,a)') 
     ^    sz,' f1','(',cword(1:nch),')',' sh'
C
C     reset character orientation if necessary
C
      if(ang.NE.0.0) write(LPLOT,fmt='(f8.2,a)') bng,' rot'
10    RETURN
      END
c-----------------------------------------------------------------bk
      subroutine SYMBU(X,Y,SIZE,IWORD,ANGL,NCHAR)
C
C     writes a Hollerith string on the plot--user units
C
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
      CHARACTER*80 IWORD
      CHARACTER*80 CWORD
      nch = nchar
      IF(NCHAR.GT.80)NCH=80
C
C     select character orientation
C
C      IF(ANGL.EQ.0.0)GO TO 40
C
C     select character size
C
   40 SZ=ABS(SIZE)*PSCA*1.5
      do 50 k=1,80
      cword(k:k) = iword(k:k)
   50 continue
      do 51 k=nch+1,80
      cword(k:k) = ' '
   51 continue
C
C      move pen to symbol location
C
      IP=3
      IF(SIZE.LT.0.0)IP=-3
      CALL PLOTU(X,Y,IP)
      if(ANGL.NE.0.0) write(LPLOT,fmt='(f8.2,a)') angl,' rot'
C
c     write character string
C
      write(LPLOT,fmt='(f8.2,x,a,x,a,a,a,a)') 
     ^    sz,' f1','(',cword(1:nch),')',' sh'
C
C     reset character orientation if necessary
C
      IF(ANGL.EQ.0.0)RETURN
      bngl = -angl 
      write(LPLOT,fmt='(f8.2,a)') bngl,' rot'
      RETURN
      END
C
C-----------------------------------------------------------------
C  ROUTINES FOR SHAPES
C-----------------------------------------------------------------
c-----------------------------------------------------------------bk
      subroutine FILLTYP(it,spac,ian)
C
C     specifies fill type for shading
C
C     it    - 
C     spac  - line spacing in cm converted to gray tone
C     ian   - 
C
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
C
      spec = spac
c     if(spec.gt.0.9) spec=0.9
      write(LPLOT,fmt='(f7.4,a)') spec,' sG'
C
      return
      end
C---------------------------------------------------------------bk-
      subroutine SHADRT(XI,YI)
C
C     Shades rectangle defined by coordinate increments xinc,yinc
C
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
C      
      xin = a*xi
      yin = c*yi
      call edgert(xi,yi)
      write(LPLOT,fmt='(a)') 'fill'
C
      return
      end
C---------------------------------------------------------------bk-
      subroutine EDGERT(XI,YI)
C
C     Edges rectangle defined by coordinate increments xinc,yinc
C
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
C
      xin = a*xi
      yin = c*yi
      call plot(xin,0.0,-2)
      call plot(0.0,yin,-2)
      call plot(-xin,0.0,-2)
      call plot(0.0,-yin,-2)
      write(LPLOT,fmt='(a)') 'cl'
C
      return
      end
C---------------------------------------------------------------bk-
      subroutine FILLPOLY(PX,PY,N)
C
C     Fills polygon with edge specified by N points (<2000)
C
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
      dimension px(2000),py(2000)
C
      call plot(px(1),py(1),3)
      do m=1,n
        call plot(px(m),py(m),2)
      end do
      call plot(px(1),py(1),2)
      write(lplot,*) "cf"
C
      return
      end
C---------------------------------------------------------------bk-
      subroutine EDGPOLY(PX,PY,N)
C
C     Draw polygon with edge specified by N points (<2000)
C
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
      dimension px(2000),py(2000)
C
      call plot(px(1),py(1),3)
      do m=1,n
        call plot(px(m),py(m),2)
      end do
      call plot(px(1),py(1),2)
      write(lplot,*) "cl"
C
      return
      end

C-----------------------------------------------------------------
C  ROUTINES FOR PIXEL IMAGES
C-----------------------------------------------------------------
C-------------------------------------------------------------------
      subroutine PIMAG4 (xori,yori,xxl,yyl,nsxx,nsyy,arr,
     &               nth11,nth22,sth11,sth22)
c------
c       pimag4       bitmap image plotting in postscript 
c                  (4 bit representation)
c                  xori - x origin for bit image
c                  yori - y origin for bit image
c                  xxl  - x dimension of block
c                  yyl  - y dimension of block
c                  arr(nsxx,nsyy) - data array
c                  nth11,nth22  - range of levels (1,16)
c                  sth11,sth22  - corresponding array values
c
c     n.b.   grey scale inverted i.e. 0 - white 16 - black
c------
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
      real*4 xl,yl,sth1,sth2
      integer*4 nsx,nsy,nth1,nth2
      character*1 car,chex(16)
      dimension car(256,256)
      dimension arr(nsxx,nsyy)
      data chex/'0','1','2','3','4','5','6','7','8','9',
     &        'A','B','C','D','E','F'/
c    
      do 100 j=1,256
      do 110 k=1,256
        car(k,j) = ' '
 110    continue
 100  continue
c                             create Hex map
      xl=xxl
      yl=yyl
      nsx=nsxx
      nsy=nsyy
      nth1=nth11
      nth2=nth22
      sth1=sth11
      sth2=sth22
      nsx1 = nsx
      nr = MOD(nsx,2)
      if(nr.eq.1) nsx1=nsx1+1
      nsx2 = nsx1/2
      do 10 j=1,nsy
      do 20 i=1,nsx1
        npix=int(float(nth1)+float(nth2-nth1)/(sth2-sth1)*
     &       (arr(i,j)-sth1))
        if(npix.lt.nth1) npix=nth1
        if(npix.gt.nth2) npix=nth2
        car(i,j) = chex(17-npix)
c        write(6,*) i,j, arr(i,j),npix,car(i,j)
20      continue
10    continue     
c100   format(Z1)
c                             write Postscript commands 
      xso = xori*psca
      yso = yori*psca
      xscal = xl*psca
      yscal = yl*psca
      write(LPLOT,fmt='(a)') 'gsave'
      write(LPLOT,81) xso,yso
 81    format(1x,2f8.3,' translate')
      write(LPLOT,82) xscal,yscal
 82    format(1x,2f10.3,' scale')
      write(LPLOT,83) nsx,nsy,nsx,nsy
 83    format(' /imPr {',2i4,' 4 [',i4,' 0 0',i4,' 0 0 ]',/
     ^       ' { currentfile iLn readhexstring pop} image } def')
      write(LPLOT,84) nsx2
 84    format(' /iLn ',i4,' string def',/,' imPr')
      write(LPLOT,85) ((car(ip,jp),ip=1,nsx1),jp=1,nsy)
 85    format(1x,64a1)
      write(LPLOT,fmt='(a)') 'grestore'
c
      return 
      end
C-------------------------------------------------------------------
      subroutine PIMAG8(xori,yori,xxl,yyl,nsxx,nsyy,arr,
     &               nth11,nth22,sth11,sth22)
c------
c       pimag8       bitmap image plotting in postscript 
c                  (8 bit representation)
c                  xori - x origin for bit image
c                  yori - y origin for bit image
c                  xxl  - x dimension of block
c                  yyl  - y dimension of block
c                  arr(nsxx,nsyy) - data array
c                  nth11,nth22  - range of levels (1,256)
c                  sth11,sth22  - corresponding array values
c
c     n.b.   grey scale inverted i.e. 1 - white 256 - black
c------
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
      real*4 xl,yl,sth1,sth2
      integer*4 nsx,nsy,nth1,nth2
      character*1 chex(16)
      character*2 car,ccar
      dimension car(256,256)
      dimension arr(nsxx,nsyy)
      data chex/'0','1','2','3','4','5','6','7','8','9',
     &        'A','B','C','D','E','F'/
c    
      do 100 j=1,256
      do 110 k=1,256
        car(k,j) = '  '
 110    continue
 100  continue
c                             create Hex map
      xl=xxl
      yl=yyl
      nsx=nsxx
      nsy=nsyy
      nth1=nth11
      nth2=nth22
      sth1=sth11
      sth2=sth22
      do 10 j=1,nsy
      do 20 i=1,nsx
        npix=int(float(nth1)+float(nth2-nth1)/(sth2-sth1)*
     &       (arr(i,j)-sth1))
        if(npix.lt.nth1) npix=nth1
        if(npix.gt.nth2) npix=nth2
        npix=256-npix
        np1=npix/16+1
        np2=MOD(npix,16)+1
        ccar(1:1)=chex(np1)
        ccar(2:2)=chex(np2)
        car(i,j) = ccar 
c        write(6,*) i,j, arr(i,j),npix,car(i,j)
20      continue
10    continue     
c                             write Postscript commands 
      xso = xori*psca
      yso = yori*psca
      xscal = xl*psca
      yscal = yl*psca
      write(LPLOT,fmt='(a)') 'gsave'
      write(LPLOT,81) xso,yso
 81    format(1x,2f8.3,' translate')
      write(LPLOT,82) xscal,yscal
 82    format(1x,2f10.3,' scale')
      write(LPLOT,83) nsx,nsy,nsx,nsy
 83    format(' /imPr {',2i4,' 8 [',i4,' 0 0',i4,' 0 0 ]',/
     ^       ' { currentfile iLn readhexstring pop} image } def')
      write(LPLOT,84) nsx
 84    format(' /iLn ',i4,' string def',/,' imPr')
      write(LPLOT,85) ((car(ip,jp),ip=1,nsx),jp=1,nsy)
 85    format(1x,32a2)
      write(LPLOT,fmt='(a)') 'grestore'
c
      return 
      end
C
      subroutine pimask (xori,yori,xxl,yyl,nsxx,nsyy,arr,
     &               nk11,nk22,nk33,sk11,sk22,kpen1)
c------
c       pimask       bitmap imagemask plotting in postscript 
c                  (1 bit representation)
c                  xori - x origin for bit image
c                  yori - y origin for bit image
c                  xxl  - x dimension of block
c                  yyl  - y dimension of block
c                  arr(nsxx,nsyy) - data array
c                  nk11  - mask on
c                  nk22  - mask on ) alternate
c                  nk33  - mask off) rows
c                  sth11,sth22  - limits for band 
c     nk1,nk2,nk3 must all lie in 1,16 and 16-(nk.-1) will
c     be interpreted as 4 1bit operations in terms of the mask
c
c     n.b.   grey scale inverted i.e. 0 - white 16 - black
c------
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
      COMMON/T00000/zxb0,zyb0,fchar,ffac
c
      real*4 xl,yl,sk1,sk2
      integer*4 nsx,nsy,nk1,nk2,nk3
      character*1 car,chex(16)
      dimension car(256,256)
      dimension arr(nsxx,nsyy)
      character*4 fchar
      data chex/'0','1','2','3','4','5','6','7','8','9',
     &        'A','B','C','D','E','F'/
c    
      do 100 j=1,256
      do 110 k=1,256
        car(k,j) = ' '
 110    continue
 100  continue
c                             create Hex map
      xl=xxl
      yl=yyl
      nsx=nsxx
      nsy=nsyy
      nk1=nk11
      nk2=nk22
      nk3=nk33
      sk1=sk11
      sk2=sk22
      kpen = kpen1
      write(6,fmt='(a)') 'pimask:',xl,yl,nsx,nsy,nk1,nk2,nk3,sk1,sk2,kpen 
c
      nsx1 = nsx
      nr = MOD(nsx,2)
      if(nr.eq.1) nsx1=nsx1+1
      nsx2 = nsx1/2
      do 10 j=1,nsy
      do 20 i=1,nsx1
        if(mod(j,2).eq.0) then
          npix = nk2
        elseif(mod(j,2).eq.1) then
          npix = nk3
        endif
        if(arr(i,j).lt.sk1) npix = nk1              
        if(arr(i,j).gt.sk2) npix = nk1
        car(i,j) = chex(17-npix)
c      write(6,*) i,j, arr(i,j),npix,car(i,j)
20      continue
10    continue     
c100   format(Z1)
c                             write Postscript commands 
      gray = float(kpen)/16.
      nsx4 = nsx*4
      xso = xori*psca
      yso = yori*psca
      xscl = xl*psca
      yscl = yl*psca
      write(8,80) gray    
 80    format(1x,f8.4,' setgray')
      write(8,fmt='(a)') 'gsave'
      write(8,81) xso,yso
 81    format(1x,2f8.3,' translate')
      write(8,82) xscl,yscl
 82    format(1x,2f10.3,' scale')
      write(8,83) nsx4,nsy,nsx4,nsy
 83    format(2i4,' true  [',i4,' 0 0',i4,' 0 0 ]')
      write(8,fmt='(a)') '{<'
      write(8,85) ((car(ip,jp),ip=1,nsx1),jp=1,nsy)
 85    format(1x,64a1)
      write(8,fmt='(a)') '>}  imagemask'
      write(8,fmt='(a)') 'grestore'
c
      return 
      end
C-------------------------------------------------------------------
      subroutine PIMCOL8(xori,yori,xxl,yyl,nsxx,nsyy,arr,
     &                   nth11,nth22,sth11,sth22)
c------
c         pimcol8         bitmap image plotting in postscript 
c                        (colour 8 bit representation)
c                        xori - x origin for bit image
c                        yori - y origin for bit image
c                        xxl  - x dimension of block
c                        yyl  - y dimension of block
c                        arr(nsxx,nsyy) - data array
c                        nth11,nth22  - range of levels (1,256)
c                        sth11,sth22  - corresponding array values
c
c     n.b.   uses predefined pen colours 1-256
c            so ldcolor or equivalent pen definitions need to
c            be used to set up colours before pimcol8 is used
c------
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
      real*4 xl,yl,sth1,sth2
      integer*4 nsx,nsy,nth1,nth2
      character*1 chex(16)
      character*2 chr,chg,chb,cc
      dimension chr(400,400),chg(400,400),chb(400,400)
      dimension arr(nsxx,nsyy)
      data chex/'0','1','2','3','4','5','6','7','8','9',
     &          'A','B','C','D','E','F'/
c    
      do 100 j=1,400
        do 110 k=1,400
          chr(k,j) = '  '
          chg(k,j) = '  '
          chb(k,j) = '  '
110    continue
 100  continue
c                                     create Hex map
      xl=xxl
      yl=yyl
      nsx=nsxx
      nsy=nsyy
      nth1=nth11
      nth2=nth22
      sth1=sth11
      sth2=sth22
      do 10 j=1,nsy
        do 20 i=1,nsx
          npix=int(float(nth1)+float(nth2-nth1)/(sth2-sth1)*
     &         (arr(i,j)-sth1))+1
          if(npix.lt.nth1) npix=nth1
          if(npix.gt.nth2) npix=nth2
c red
          npir=icolr(npix)
          np1=npir/16+1
          np2=MOD(npir,16)+1
          cc(1:1)=chex(np1)
          cc(2:2)=chex(np2)
          chr(i,j) = cc 
*          write(6,*) "red",npir,np1,np2,cc
c green
          npig=icolg(npix)
          np1=npig/16+1
          np2=MOD(npig,16)+1
          cc(1:1)=chex(np1)
          cc(2:2)=chex(np2)
          chg(i,j) = cc 
*          write(6,*) "green",npig,np1,np2,cc
c blue
          npib=icolb(npix)
          np1=npib/16+1
          np2=MOD(npib,16)+1
          cc(1:1)=chex(np1)
          cc(2:2)=chex(np2)
          chb(i,j) = cc 
*          write(6,*) "blue",npib,np1,np2,cc
20      continue
10    continue     
c                                     write Postscript commands
      nsx3 = 3*nsx 
      xso = xori*psca
      yso = yori*psca
      xscal = xl*psca
      yscal = yl*psca
      write(LPLOT,fmt='(a)') 'gsave'
      write(LPLOT,81) xso,yso
 81    format(1x,2f8.3,' translate')
      write(LPLOT,82) xscal,yscal
 82    format(1x,2f10.3,' scale')
      write(LPLOT,83) nsx,nsy,nsx,nsy
 83    format(' /imCr {',2i4,' 8 [',i4,' 0 0',i4,' 0 0 ]',/
     ^       ' { currentfile iLc readhexstring pop}', 
     ^       ' false 3 colorimage } def')
      write(LPLOT,84) nsx3
 84    format(' /iLc ',i4,' string def',/,' imCr')
      write(LPLOT,85) 
     ^ ((chr(ip,jp),chg(ip,jp),chb(ip,jp),ip=1,nsx),jp=nsy,1,-1)
 85    format(1x,36a2)
      write(LPLOT,fmt='(a)') 'grestore'
c
      return 
      end
C------------------------------------------------------------bk--
      SUBROUTINE PICOL(xori,yori,xxl,yyl,nsxx,nsyy,arr,
     &                 nth11,nth22,sth11,sth22)
C----------
C        picol         colour image plotting
C                      xori - x origin for image
C                      yori - y origin for image
C                      xxl  - x dimension of block
C                      yyl  - y dimension of block
C                      arr(nsxx,nsyy) - data array
C                      nth11 - lowest colour
C                      nth22 - highest colour
C                      sth11 - array value for bottom of lowest colour
C                      sth22 - array value for top of highest colour
C
C       The array is supposed to be organised so the 
C       columns proceed down the page                
C
C       For uneven spacings PIMASK can be used with 
C       the pen set to the current colour   
C--------- 
C    
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
      real*4 xl,yl,sth1,sth2
      integer*4 nsx,nsy,nth1,nth2
      dimension arr(nsxx,nsyy)
      integer nc
C      
      xl = xxl
      yl = yyl
      nsx = nsxx
      nsy = nsyy
      nth1 = nth11
      nth2 = nth22
      sth1 = sth11
      sth2 = sth22
*     write(6,*) 'picol:',xl,yl,nsx,nsy,nth1,nth2,sth1,sth2
c 
      xso = xori*psca
      yso = (yori+yyl)*psca
      dsx = xl*psca/float(nsx)
      dsy = yl*psca/float(nsy)
c                                write Postscript commands                      
      write(LPLOT,fmt='(a)') 'gsave'
      write(LPLOT,81) xso,yso
 81   format(1x,2f8.3,' pM')
      ysa = yso-dsy
      do 10 j=1,nsy
        xsa = xso
        do 20 i=1,nsx
          nc = int(float(nth1)+
     &         float(nth2-nth1)*(arr(i,j)-sth1)/(sth2-sth1))
          call pen(nc,0)
          write(LPLOT,81) xsa,ysa
          write(LPLOT,82) xsa,ysa+dsy,xsa+dsx,ysa+dsy,xsa+dsx,ysa
 82       format(1x,2f8.3,' pL',2f8.3,' pL',2f8.3,' pL')
          write(LPLOT,83) xsa,ysa
 83       format(1x,2f8.3,' pL cf ')
          xsa = xsa+dsx	
 20     continue
        ysa = ysa-dsy
 10   continue
c
      WRITE(LPLOT,fmt='(a)') 'grestore'
      RETURN      
      END
C
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C*    TYPSET SIMULATION
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~bk~
      SUBROUTINE TYPSET( XB0 , YB0 )
C
c++   This routine resets the origin in plotter units to
c++   (xbo ,yb0) for an interface to the R.S.E.S. plotter
c++   libraries based on the PLOTPAK calls
c
C
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
      COMMON/T00000/zxb0,zyb0,fchar,ffac
      character*4 fchar
C
      ZXB0 = XB0
      ZYB0 = YB0
      RETURN
      END
c------------------------------------------------------------bk
      subroutine zpick(ifont,n,is)
c     
c     choose a Laserwriter font
c
c     1      Helvetica
c     2      Helvetica-Bold
c     3      Palatino-Roman  
c     4      Times-Roman
c     5      Helvetica-Oblique
c     6      Palatino-Italic
c     7      Times-Italic
c     8      Times-Bold
c     9      Times-BoldItalic
c     10     Symbol
c     11     Palatino-Bold
c     12     Palatino-BoldItalic
c     13     LucidaSans
c     14     LucidaSans-Italic
c     15     LucidaSans-Bold
c     16     LucidaSans-BoldItalic
c     17     Helvetica-BoldOblique
c
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
      COMMON/T00000/zxb0,zyb0,fchar,ffac
      
      character*4 fchar
      if (ifont.lt.10) then
      write(fchar,51) ifont
  51    format('f',i1,'  ')
      elseif (ifont.ge.10) then
      write(fchar,52) ifont 
  52    format('f',i2,' ')
      endif
      return
      end
c-------------------------------------------------------------bk
      subroutine zfont(FF,FS)
c
c     choose a Postscript font by font Family FF:
c      HE   Helvetica
c      LS   LucidaSans
c      TI   Times
c      PA   Palatino
c      SY   Symbol
c     and Font Style FS:
c      RO   Roman
c      IT   Italic
c      BO   Bold
c      BI   BoldItalic     
c
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
      COMMON/T00000/zxb0,zyb0,fchar,ffac
c
      character*2 FF,FS 
      character*4 fchar
c
      if(FF.eq."SY") then
        ifont = 10
      elseif(FF.eq."HE") then
        if(FS.eq."RO") ifont=1
        if(FS.eq."IT") ifont=5
        if(FS.eq."BO") ifont=2
        if(FS.eq."BI") ifont=17
      elseif(FF.eq."TI") then
        if(FS.eq."RO") ifont=4
        if(FS.eq."IT") ifont=7
        if(FS.eq."BO") ifont=8
        if(FS.eq."BI") ifont=9
      elseif(FF.eq."PA") then
        if(FS.eq."RO") ifont=3
        if(FS.eq."IT") ifont=6
        if(FS.eq."BO") ifont=11
        if(FS.eq."BI") ifont=12
      elseif(FF.eq."LS") then
        if(FS.eq."RO") ifont=13
        if(FS.eq."IT") ifont=14
        if(FS.eq."BO") ifont=15
        if(FS.eq."BI") ifont=16
      endif
c
      write(6,*) "zfont: ",FF," ",FS,ifont
c
      if (ifont.lt.10) then
      write(fchar,51) ifont
  51    format('f',i1,'  ')
      elseif (ifont.gt.10) then
      write(fchar,52) ifont 
  52    format('f',i2,' ')
      endif
      return
      end
c-------------------------------------------------------------bk
      SUBROUTINE TYPNUM(X,Y,SIZE1,FNUM1,ANGLE,NDEC1)
C
C-- THIS ROUTINE CONVERTS FNUM1 TO THE APPROPRIATE FIXED DECIMAL
C-- EQUIVALENT AND PLOTS IT TO ANY DEGREE OF ACCURACY WITH ROUNDING.
C
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
      COMMON/T00000/zxb0,zyb0,fchar,ffac
      character*4 fchar
      CHARACTER*8 IFORM
      CHARACTER*40 CWORD
C
C    create the format expression in IFORM
C
      NDEC = NDEC1
      SIZE=ABS(SIZE1)
      RN=FNUM1
      iadd = 1
      if(ndec.ge.0) iadd=2
      if(rn.lt.0) iadd=iadd+1
      if(abs(rn).lt.1) iadd=iadd+1
      if(rn.eq.0) then
      if(ndec.lt.0) np = 1
      if(ndec.ge.0) np = 2
      else
      pow = alog10(abs(rn))
      np = int(pow+0.01)+iadd
      if(np.lt.0.) np = 1
      endif
      if(ndec.lt.0) nsf = -np
      if(ndec.ge.0) nsf = 10*(np+ndec)+ndec
c       write(6,*) nsf,rn
c 
      IF(NSF.LT.0)GO TO 20
      ITOT=NSF/10
      IDPL=MOD(NSF,10)
      Write(IFORM,55)ITOT,IDPL
   55 FORMAT('(F',I2,'.',I1,')')
      Write(CWORD,IFORM)RN
      GO TO 30
C
C    for integer format
C
   20 ITOT=-NSF
      Write(IFORM,65)ITOT
   65 FORMAT('(I',I2,')   ')
      IR=INT(RN)
      Write(CWORD,IFORM)IR
   30 CALL TYPSTR(X,Y,SIZE,CWORD,ANGLE,ITOT)
      RETURN
      END
c---------------------------------------------------------bk
      SUBROUTINE TYPSTR(X,Y,SIZE,KWORD,ANGL,NCHAR)
C
C     writes a character string on the plot--plotter units
C
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
      COMMON/T00000/zxb0,zyb0,fchar,ffac
      character*4 fchar
      CHARACTER*80 KWORD
      CHARACTER*60 CWORD
c
      nch = nchar
      IF(NCHAR.GT.77)NCH=77
C
C     select character orientation
C
c      IF(ANGL.EQ.0.0)GO TO 40
C
C     select character size
C
   40 SZ=ABS(SIZE)*PSCA*1.5
      do 50 k=1,60
      cword(k:k) = kword(k:k)
   50 continue
      do 51 k=nch+1,60
      cword(k:k) = ' '
   51 continue
C
C      move pen to symbol location
C
      IP=3
      IF(SIZE.LT.0.0)IP=-3
      CALL PLOT(X+zxb0,Y+zyb0,IP)
      IF(ANGL.NE.0.0) write(LPLOT,fmt='(f8.2,a)') angl,' rot'
C
c     write character string
C
      write(LPLOT,fmt='(f8.2,x,a,x,a,a,a,a)') 
     ^  sz,fchar,'(',cword(1:nch),')',' sh'
C
C     reset character orientation if necessary
C
      IF(ANGL.EQ.0.0)RETURN
      bngl = -angl 
      write(LPLOT,fmt='(f8.2,a)') bngl,' rot'
      RETURN
      END
c
c---------------------------------------------------------bk
      SUBROUTINE TYPchr (X,Y,SIZE,KWORD,J1,J2)
C
C     writes a portion of a character string on the plot
c     --plotter units
C
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/PC0000/icolr(256),icolg(256),icolb(256)
      COMMON/L00000/PSCA,xo,yo
      COMMON/T00000/zxb0,zyb0,fchar,ffac
      character*4 fchar
      character*4 cww,kword
      do 10 j=1,4
        cww(j:j) = ' '
10    continue
      k=1
      do 11 j=j1,j2
        cww(k:k)=kword(j:j) 
        k=k+1
 11   continue
C
C     select character size
C
   40 SZ=ABS(SIZE)*PSCA*1.5
C
C      move pen to symbol location
C
      IP=3
      IF(SIZE.LT.0.0)IP=-3
      CALL PLOT(X+zxb0,Y+zyb0,IP)
C
c     write character string
C
      write(LPLOT,fmt='(f8.2,x,a,x,a,a,a,a)') 
     ^  sz,fchar,'(',cww,')',' sh'
C
      RETURN
      END
C---------------------------------------------------------------bk-
      subroutine woper(x,y,siz,cw,if1,if2)
      character*4 cw
      ssiz = 0.7*siz
      csiz = 0.05*siz
      call zpick(if1,0,is)
      call typchr(x     ,y ,siz ,cw,1,1)
      call typchr(x+csiz,y ,siz ,cw,1,1)
      call zpick(if2,0,is)  
      call typchr(x+1.05*siz,y-0.4*siz,ssiz,cw,2,2)
      call typchr(x+1.10*siz,y+0.8*siz,ssiz,cw,3,4)
      return
      end
C-----------------------------------------------------------------
C  ROUTINES RETAINED FOR COMPATABILITY
C-----------------------------------------------------------------
      subroutine ASPECT(RASP)
C
C     sets width to height ratio for characters to RASP
C
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      COMMON/PC0000/colr(256),colg(256),colb(256)
      COMMON/L00000/PSCA,xo,yo
      ASP = RASP
      return
      end
C-----------------------------------------------------------------
      subroutine rotpt(x,y,xx,yy,angl)
c      rotate pts
      pi=3.14159
      a=angl/180.0*pi
      xx=x*cos(a)-y*sin(a)
      yy=x*sin(a)+y*cos(a)
      return
      end
C-----------------------------------------------------------------
C  ROUTINES based on Plotpak - DO NOT USE ROTATION
C    derived from Japanese version of plotpak
C    courtesy of T. Shibutani
C-----------------------------------------------------------------
C---------------------------------------------------------------ts-
      subroutine PEXTEND
c
c     adds useful symbols etc
c
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/L00000/PSCA,xo,yo
C
c     Postscript operator definition
      write(lplot,*) '%'
      write(lplot,*) '/AR {stroke newpath 0 360 arc cl stroke} def'
      write(lplot,*) '/cAR {stroke newpath 0 360 arc cf} def'
      write(lplot,*) '/gBOX {stroke newpath moveto'
      write(lplot,*) '    dx 0 rL 0  dy rL dx neg 0 rL 0 dy neg rL'
      write(lplot,*) '    clospath hue setgray fill} def' 
      write(lplot,*) '/cBOX {stroke newpath moveto'
      write(lplot,*) '    dx 0 rL 0  dy rL dx neg 0 rL 0 dy neg rL'
      write(lplot,*) '    closepath hue 1. 1. sH fill} def' 
      write(lplot,*) '/PLUS {stroke newpath moveto'
      write(lplot,*) '   -0.5 0 rL 1 0 rL -0.5 -0.5 rM 0 1 rL} def'
      write(lplot,*) '/SQR {stroke newpath moveto -4 -4 rM '
      write(lplot,*) '   0 8 rL 8 0 r 0 -8 rL -8 0 rL cf} def'
      write(lplot,*) '/TRI {stroke newpath moveto -4 -4 rM'
      write(lplot,*) '    4 8 r 4 -8 rL -8 0 rL cf} def'
      write(lplot,*) '/STAR {stroke newpath moveto'
      write(lplot,*) '   0 8.0 rM 4.8 -14.4 rL'
      write(lplot,*) '   -12.4 8.8 rL 15.2 0 rL'
      write(lplot,*) '   -12.4 -8.8 rL 4.8 14.4 r cf} def'
c
      return
      end
C-----------------------------------------------------------gh--bk
       subroutine ORIGIN(x,y,iorig)
c
      common/p00001/ xorig,yorig,ipage,iobj
      common/l00000/psca,xo,yo
c
      if(iorig.eq.0) then
         xorig=x*psca
         yorig=y*psca
      else if(iorig.gt.0) then
         xorig=xorig+x*psca
         yorig=yorig+y*psca
      else if(iorig.lt.0) then
         if(psca.eq.0) stop 'ORIGIN error: zero scale'
         x=xorig/psca
         y=yorig/psca
      endif
      return
      end
C---------------------------------------------------------------ts-
      subroutine cellsize(dx,dy)
c	To paint cells.
c
        COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
        common/p00001/ xorig,yorig,ipage,iobj
        COMMON/L00000/PSCA,xo,yo
C
	ddx=psca*dx
	ddy=psca*dy
c
	write(lplot,'("/dx ",f9.3," def")') ddx
	write(lplot,'("/dy ",f9.3," def")') ddy
c
	return
	end
C---------------------------------------------------------------ts-
        subroutine fcell(x,y,dx,dy,col)
c
c	To paint cells in colour
c
        COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
        common/p00001/ xorig,yorig,ipage,iobj
        COMMON/L00000/PSCA,xo,yo
C
	ddx=psca*dx
	ddy=psca*dy
	x1=psca*x-ddx/2.+xorig
	y1=psca*y-ddy/2.+yorig
c
	write(lplot,'("/hue ",f9.3," def")') col
	write(lplot,'(2f9.3," cBOX")') x1,y1
c
	return
	end
C---------------------------------------------------------------ts-
	subroutine fcellg(x,y,dx,dy,col)
c
c	To paint cells - greytone
c
        COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
        common/p00001/ xorig,yorig,ipage,iobj
        COMMON/L00000/PSCA,xo,yo
C
	ddx=psca*dx
	ddy=psca*dy
	x1=psca*x-ddx/2.+xorig
	y1=psca*y-ddy/2.+yorig
c
	write(lplot,'("/hue ",f9.3," def")') col
	write(lplot,'(2f9.3," gBOX")') x1,y1
c
	return
	end
C---------------------------------------------------------------ts-
      subroutine PLUS(x,y)
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/L00000/PSCA,xo,yo
      xp=PSCA*x+xorig
      yp=PSCA*y+yorig
      write(lplot,'(2f9.3,a)') xp,yp,' PLUS'
      end
C---------------------------------------------------------------ts-
      subroutine SQR(x,y)
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/L00000/PSCA,xo,yo
      xp=PSCA*x+xorig
      yp=PSCA*y+yorig
      write(lplot,'(2f9.3,a)') xp,yp,' SQR'
      end
C---------------------------------------------------------------ts-
      subroutine star(x,y)
      common/P00000/LPLOT,IROT,IL34,a,b,c,d,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      common/L00000/psca,xo,yo
      xp=psca*x+xorig
      yp=psca*y+yorig
      write(lplot,'(2f9.3,a)') xp,yp,' STAR'
      return
      end
C---------------------------------------------------------------ts-
      subroutine TRI(x,y)
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/L00000/PSCA,xo,yo
      xp=PSCA*x+xorig
      yp=PSCA*y+yorig
      write(lplot,'(2f9.3,a)') xp,yp,' TRI'
      end
C---------------------------------------------------------------ts-
      subroutine ARC(x,y,RADIUS,ang1,ang2)
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/L00000/PSCA,xo,yo
      xp=PSCA*x+xorig
      yp=PSCA*y+yorig
      rp=PSCA*radius
      write(lplot,'(5f9.3,a)') xp,yp,rp,ang1,ang2,' arc'
      end
C---------------------------------------------------------------ts-
      subroutine ARCN(x,y,RADIUS,ang1,ang2)
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/L00000/PSCA,xo,yo
      xp=PSCA*x+xorig
      yp=PSCA*y+yorig
      rp=PSCA*radius
      write(lplot,'(5f9.3,a)') xp,yp,rp,ang1,ang2,' arcn'
      end
C---------------------------------------------------------------ts-
      subroutine cCIRC1(x,y,RADIUS)
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/L00000/PSCA,xo,yo
      xp=PSCA*x+xorig
      yp=PSCA*y+yorig
      rp=PSCA*radius
      write(lplot,'(3f9.3,a)') xp,yp,rp,' cAR'
      end
C---------------------------------------------------------------ts-
      subroutine CIRC1(x,y,RADIUS)
      COMMON/P00000/LPLOT,IROT,IL34,A,B,C,D,ASP,THET
      common/p00001/ xorig,yorig,ipage,iobj
      COMMON/L00000/PSCA,xo,yo
      xp=PSCA*x+xorig
      yp=PSCA*y+yorig
      rp=PSCA*radius
      write(lplot,'(3f9.3,a)') xp,yp,rp,' AR'
      end
