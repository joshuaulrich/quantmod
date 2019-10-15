c
c     to.period imlpementation to speed up data
c     aggregation in OHLC format
c
c     currently translates to 100X speed improvement
c     
c     Copyright Jeffrey A. Ryan 2007
c     jeff _dot_ ryan _at_ quantmod _dot_ com
c
c     Distributed under GPL-3
c
      subroutine pmaxz(bp,lbp,ia,lia,ret)
c     Usage:
c      
c     bp   index of breakpoints
c     lbp  length of breakpoints index
c     ia   input array of double precision
c     lia  length of ia vector
c     ret  return array of values
c
      implicit none

      integer lbp,lia
      integer i,j
      integer bp(lbp),pos
      double precision ps(lbp)
      double precision ia(lia), ret(*)

c
c     pos keeps track of location in col-major array
c
      pos = 1
       
      do 10 i=1,(lbp-1)
c      
c     step through each period of bp
c
        do 20 j=(bp(i)+1),bp(i+1)
          if(j .eq. (bp(i)+1)) ps(i) = ia(j)
c
c         step through each obs. and note values
          ps(i) = max(ps(i),ia(j))

   20   continue

c
c       after each period - create entry in ret array
c
        ret(pos) = ps(i)
c
c       increment position by 1
c
        pos = i+1

   10 continue
      end



      subroutine pminz(bp,lbp,ia,lia,ret)
c     Usage:
c      
c     bp   index of breakpoints
c     lbp  length of breakpoints index
c     ia   input array of double precision
c     lia  length of ia vector
c     ret  return array of values
c
      implicit none

      integer lbp,lia
      integer i,j
      integer bp(lbp),pos
      double precision ps(lbp)
      double precision ia(lia), ret(*)

c
c     pos keeps track of location in col-major array
c
      pos = 1
c
c     data must be OHLC or value,volume
c     offsets if incoming OHLC data
       
      do 10 i=1,(lbp-1)
c      
c     step through each period of bp
c
        do 20 j=(bp(i)+1),bp(i+1)
          if(j .eq. (bp(i)+1)) ps(i) = ia(j)
c
c       step through each obs. and note values
          ps(i) = min(ps(i),ia(j))

   20   continue

c
c       after each period - create entry in ret array
c
        ret(pos) = ps(i)
c
c       increment position by 1
c
        pos = i+1

   10 continue
      end
