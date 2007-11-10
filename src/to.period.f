c
c     to.period imlpementation to speed up data
c     aggregation in OHLC format
c     
c     Copyright Jeffrey A. Ryan 2007
c     jeff _dot_ ryan _at_ quantmod _dot_ com
c
c     Distributed under GPL-3
c
      subroutine toperiod(bp,lbp,ia,lia,nri,nci,ret)
c     Usage:
c      
c     bp   index of breakpoints
c     lbp  length of breakpoints index
c     ia   input array of double precision
c     lia  length of ia vector
c     nri  number of rows in input array
c     nci  number of columns in input array
c     ret  return array of values
c
      integer lbp,lia,nci,nri
      integer bp(lbp),pos
      double precision o(lbp),h(lbp),l(lbp),c(lbp),v(lbp),a(lbp)
      double precision ia(lia), ret(*)

c
c     position keeps track of location in col-major array
c
      pos = 1
      do 10 i=1,(lbp-1)
c      
c     step through each period of bp
c
        do 20 j=(bp(i)+1),bp(i+1)
c
c       step through each obs. and note values
c
          if(j .eq. bp(i)+1) then
c
c         if first obs. in bp period take
c         as opening value, and initialize
c         high and lows
c
            o(i) = ia(j)
            h(i) = ia(j+1*nri)
            l(i) = ia(j+2*nri)
          endif

c
c         check if each value is a new high or low
c
          h(i) = max(h(i),ia(j+1*nri))
          l(i) = min(l(i),ia(j+2*nri))
           
          if(j .eq. bp(i+1)) then
c
c         if last obs. in bp period take as close
c
            c(i) = ia(j+3*nri)  
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
c       increment position by nci
c
        pos = (i * nci) +1

   10 continue
      end
