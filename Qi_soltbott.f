      subroutine soltbott
      use parm
      implicit none
      integer :: j
      real :: f, tamp, ansnodep
 
       j = 0
       j = ihru

       f=0
       tamp=0
       f=0.00032*60*60*24*365.25/3.14159265/0.2
       f=Sqrt(f)               !!=126.776 cm

	 ansnodep=1200 !!mm

	 tamp = (tmpmx(i_mo,hru_sub(j)) - tmpmn(i_mo,hru_sub(j))) / 2
       !!sur_tmp(j)=tmp_an(hru_sub(j))+(0.5*tamp)*(1-Exp(-ansnodep/10/f))
       bot_tmp(j)=tmp_an(hru_sub(j))+(0.5*tamp)*(1-Exp(-ansnodep/10/f))

	  !!bot_tmp(j)=-5
      return
      end
