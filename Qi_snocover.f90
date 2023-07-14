      subroutine snocover
      use parm
      implicit none
      integer :: j
      real :: xx

       j = 0
       j = ihru
 
 ! adjust for areal extent of snow cover
          if (sno_hru(j) < snocovmx) then
            xx = 0.
            xx = sno_hru(j) / snocovmx
            snoco(j) = xx / (xx + Exp(snocov1 - snocov2 * xx))
          else
            snoco(j) = 1.
          endif
          
          return
          end