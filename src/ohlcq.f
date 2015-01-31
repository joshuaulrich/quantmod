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
      subroutine ohlcq(bp,lbp,ia,lia,nri,hasvol,hasadj,ret)
c     Usage:
c      
c     bp   index of breakpoints
c     lbp  length of breakpoints index
c     ia   input array of double precision
c     lia  length of ia vector
c     nri  number of rows in input array
c     ret  return array of values
c
      implicit none

      integer lbp,lia,nri
      integer i,j
      integer hi,lo,cl,vo,ad
      integer bp(lbp),pos,hasvol,hasadj
      double precision o(lbp),h(lbp),l(lbp),c(lbp),a(lbp),v(lbp)
      double precision ia(lia), ret(*)
c
c     pos keeps track of location in col-major array
c
      pos = 1
c
c     data must be OHLC or value,volume
c     offsets if incoming OHLC data
        hi = 1
        lo = 2
        cl = 3
        vo = 4
        ad = 5
       

      do 10 i=1,(lbp-1)
c      
c     step through each period of bp
c
        do 20 j=(bp(i)+1),bp(i+1)
c
c       step through each obs. and note values
c
          if(j .eq. bp(i)+1) then
c         if first obs. in bp period take
c         as opening value, and initialize
c         high and lows
            o(i) = ia(j)
            h(i) = ia(j+hi*nri)
            l(i) = ia(j+lo*nri)
c           test if volume is in series before assigning
            if(hasvol .eq. 1) v(i) = 0.0D0
          endif
c
c         check if each value is a new high or low and inc.vol
          h(i) = max(h(i),ia(j+hi*nri))
          l(i) = min(l(i),ia(j+lo*nri))
          if(hasvol .eq. 1) v(i) = v(i) + ia(j+vo*nri)
           
          if(j .eq. bp(i+1)) then
c           if last obs. in bp period take as close and adj
            c(i) = ia(j+cl*nri)  
            if(hasadj .eq. 1) a(i) = ia(j+ad*nri)  
          endif

   20   continue

c
c       after each period - create entry in ret array
c
        ret(pos) = o(i)
        ret(pos+1) = h(i)
        ret(pos+2) = l(i)
        ret(pos+3) = c(i)
        if(hasvol .eq. 1) ret(pos+4) = v(i)
        if(hasadj .eq. 1) ret(pos+5) = a(i)

c
c       increment position 
c
        pos = (i * (4+hasvol+hasadj)) +1

   10 continue
      end
