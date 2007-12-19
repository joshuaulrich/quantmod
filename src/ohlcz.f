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
      subroutine ohlcz(bp,lbp,ia,lia,ret)
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
      double precision o(lbp),h(lbp),l(lbp),c(lbp)
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
c
c       step through each obs. and note values
c
          if(j .eq. bp(i)+1) then
c         if first obs. in bp period take
c         as opening value, and initialize
c         high and lows
            o(i) = ia(j)
            h(i) = ia(j)
            l(i) = ia(j)
          endif
c
c         check if each value is a new high or low and inc.vol
          h(i) = max(h(i),ia(j))
          l(i) = min(l(i),ia(j))
           
          if(j .eq. bp(i+1)) then
c           if last obs. in bp period take as close and adj
            c(i) = ia(j)  
          endif

   20   continue

c
c       after each period - create entry in ret array
c
        ret(pos) = o(i)
        ret(pos+1) = h(i)
        ret(pos+2) = l(i)
        ret(pos+3) = c(i)

c
c       increment position 
c
        pos = (i * 4) +1

   10 continue
      end
