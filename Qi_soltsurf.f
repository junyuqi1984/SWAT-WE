      subroutine soltsurf
      use parm
      implicit none
      integer :: j, k, nly, ib,ij,nlyr,idp
      real :: f, tamp, d,t_c,a
      real :: tk, ea, ed, ralb, rbo, rto, rout, rn_pet,snot
      real :: sum, smp, smfac, smleb
      real :: xx, snocov,ee
	
      real :: cej, eaj
       j = 0
       j = ihru



      if (igro(j) == 1) then     !!land cover status code:
	 idp=idplt(j)
	 select case (idc(idp))
	 case(1,2,3,4,5,6)
        ambtmp(j)=tmpav(j)
	 case(7)
	  t_c = -0.11+0.96*tmpav(j)-0.00008*tmpav(j)**3  !!air temp under closed canopy
        ambtmp(j)=tmpav(j)+(t_c-tmpav(j))              !!ambient air temp
     &	  *(log(1+min(blai(idp),laiday(j))) / log(1+ blai(idp)) )
	 end select
	else
       ambtmp(j)=tmpav(j)
	end if

       !!ambtmp(j)=ambtmp(j)-10

      select case (3)
	!
	case (1)!!-----------------------------modified method-----------------
     

	case(2)!!--------------------------default method---------------------

	
	case(3)!!----------------------SolSWAT------------------------------------
                 !! -----------------net radiation------------------

       ! adjust for areal extent of snow cover
          if (sno_hru(j) < snocovmx) then
            xx = 0.
            xx = sno_hru(j) / snocovmx
            snoco(j) = xx / (xx + Exp(snocov1 - snocov2 * xx))
          else
            snoco(j) = 1.
          endif

      cej = -5.e-5
      eaj = 0.
      eaj = Exp(cej * (sol_cov(j) + .1))   !! equation 2.2.16 in SWAT manual

      if (snoco(j) <= 0.5) then
        albday = sol_alb(j)
      !! if (laiday(j) > 0.) albday = .23 * (1. - eaj) + sol_alb(j) * eaj
      else
        albday = 0.8
      end if
          ralb = 0.
          ralb = hru_ra(j) *(1.0 - albday)
          

       if (igro(j) == 1) then     !!land cover status code:
	 idp=idplt(j)
       ralb =hru_ra(j)*(1.0 - albday)*Exp(-ext_coef(idp)*laiday(j))
	else
       ralb =hru_ra(j)*(1.0 - albday)
	 endif

        
         
                   !!------------calculate net long-wave radiation--------

          !! net emissivity  equation 2.2.20 in SWAT manual
          ea = 0.
          ed = 0.
          rbo = 0.
          ea = Ee(tmpav(j))
          ed = ea * rhd(j)
          rbo = -(0.34 - 0.139 * Sqrt(ed))

          !! cloud cover factor equation 2.2.19
          rto = 0.
            if (hru_rmx(j) < 1.e-4) then
		    rto = 0.
            else
              rto = 0.9 * (hru_ra(j) / hru_rmx(j)) + 0.1
            end if

          !! net long-wave radiation equation 2.2.21
          rout = 0.
          tk = 0.
          tk = tmpav(j) + 273.15
          rout = rbo * rto * 4.9e-9 * (tk**4)

		
          
		
		!! calculate net radiation
          rn_pet = 0.
          rn_pet = ralb + rout   !!MJ/m^2 d
          rn_pet = 100*rn_pet   !!J/(cm^2 d)
          ralb=100*ralb  !!J/ (cm^2 d)
	   
       
	end select  !!------------------------------------------------------------

       select case(1)

	case(1)
      ! if (igro(j) == 1) then
	   idp=idplt(j)
	   eff_conr=8.1*ralb*(1 - exp(min(blai(idp),laiday(j))-6.8))/86400
	! else
	   ! eff_conr=8.1*ralb*(1 - exp(-6.8))/86400
      !end if
	 
	 eff_conr=eff_conr*eff_coe(j)     !!--calibration---------------
	
	case(2) 

	case(3)
       
	 end select
	  
      
       t_bare(j)=ambtmp(j)/(1+eff_conr)+eff_conr/(1+eff_conr)
     &       *(sol_tmp1(1,j)+rn_pet/(k_sol(1,j)/((sol_thic(1,j)/10)/2)))


       snosurtmp(j)=ambtmp(j)/(1+eff_conr)+eff_conr/(1+eff_conr)              &
     & *(snotmp1(j)+(rn_pet)                                                  &
     &       /(k_sno(j)/((sno_dep(j)/10)/2)))
 
	if (sno_hru(j) <=0) then
       sur_tmp(j)=ambtmp(j)/(1+eff_conr)+eff_conr/(1+eff_conr)
     &       *(sol_tmp1(1,j)+rn_pet/(k_sol(1,j)/((sol_thic(1,j)/10)/2)))
	else
      sur_tmp(j)=ambtmp(j)/(1+eff_conr)+eff_conr/(1+eff_conr)                 &
     & *(snotmp1(j)+(rn_pet)                                                  &
     &       /(k_sno(j)/((sno_dep(j)/10)/2)))
	end if
           


      return
      end
