      subroutine solthermal
      use parm
      implicit none
      integer :: j, k, nly,lu,idp
      real :: aa, bb, cc, d, den, a, b, c, e, pp
      real ::denh2o,denice,om_csol,cpice,cpliq
      real :: tkwat,tkice, om_tkd,om_tkm,fl
	
       !real, dimension (mlyr+10,mhru) :: om_frac,csol,watsat,h2osoi_ice
       ! real, dimension (mlyr+10,mhru) :: h2osoi_liq,cv
       ! real, dimension (mlyr+10,mhru) :: tkm,tkmg,tkdry,satw,dke,dksat
       ! real, dimension (mlyr+10,mhru) :: thk,om_watsat
      

       j = 0
       j = ihru
       nly = 0
       nly = sol_nly(j)
       
       

	!!-------------------------snow thermal parameters--------------------------
	 
     if( k_sno(j) >= 864*2 ) then
	   k_sno(j) = 864*2
	 else
	 if ( sno_den(j) < 0.156 ) then
	   k_sno(j) = 0.023+0.234*sno_den(j)
	 else
	   k_sno(j) = 0.138-1.01*sno_den(j)+3.233*sno_den(j)**2
	 end if

       k_sno(j)=k_sno(j)*864*ks_coe(j)  !! J/(cm.d.C)
	 
	 endif 

	 !!k_sno(j)=86400 * (0.000358+0.0264*(sno_den(j)**2) )

	 ca_sno(j)=1.9 * sno_den(j)**2/0.917           !!J/(cm^3.C)  1.9*10**6 sno_den/ice_den (J/kg.K)

      !!-------------------------heat capacities of soil layers-------------------
       select case(1)

      case(1)
    do k = 1, nly
    ca_sol(k,j)=4.1868*( 0.48*sol_minv(k,j) +0.6*sol_orgv(k,j)+1*sol_wcv(k,j)+0.45*sol_icev(k,j)) !!j/(cm^3.C)
	 ca_sol(k,j)=ca_sol(k,j)*c_coe(j)   !! calibration
	 !!ca_sol(k,j)=ca_sol(k,j)*(sol_thic(k,j)/10) !! J/(cm^2 C)  
	end do
	
	case(2)
	! do k = 1, nly
	!denh2o = 1000.   !!kg/m3
   !   denice = 916.7   !!kg/m3
	!om_frac(k,j)=min(1.,(sol_cbn(k,j)/100)*sol_bd(k,j)*1000/130) 
	!watsat(k,j) = 0.489 - 0.00126 * sol_sand(k,j) !! Eq.7.91
    !  om_watsat(k,j) = max(0.93 - 0.1*(sol_nd(k,j)/1000/0.5), 0.83) 
    !  watsat(k,j) =(1.- om_frac(k,j))*watsat(k,j)
    ! &              +om_watsat(k,j)*om_frac(k,j)
      
       !watsat(k,j)=sol_por(k,j)   !SWAT POROSITY
      
	!om_csol=2.5 !j /cm3 K
	!csol(k,j) =((1.-om_frac(k,j))*(2.128*sol_sand(k,j)+2.385*sol_clay(k,j)) 
   !  &      / (sol_sand(k,j)+sol_clay(k,j)) + om_csol*om_frac(k,j))*1.e6  ! J/(m3 K)  
                          
	!cpice=2.11727*1000   !j /kg K
	!cpliq=4.188*1000  !j/ kg K
	!watsat(k,j)=sol_por(k,j)   
	!!h2osoi_ice(k,j)=sol_icev(k,j)*(sol_thic(k,j)/1000)*denice
	!h2osoi_liq(k,j)=sol_wcv(k,j)*(sol_thic(k,j)/1000)*denh2o
	!cv(k,j) = csol(k,j)*(1-watsat(k,j))*(sol_thic(k,j)/1000)
   !  &         + (h2osoi_ice(k,j)*cpice+ h2osoi_liq(k,j)*cpliq)  ! J/(m3 K)
              
	!ca_sol(k,j)=cv(k,j)/1.e6
    !  end do
	end select 

      !!---------------thermal conductivities of soil layers (j/(cm C d)----------
      k=0
	lu=2
      idp = idplt(j)
      pp=0.0  !!   pp~(-0.2, + 0.2)

	select case(1)

      case(1)
       !!--------------------------Johansen Method-------------------------------
       !!Johansen's method is suitable for calculating soil
       !! thermal conductivity of both coarse- and fine- grained soils in the frozen and unfrozen states. However, 
       !! it is limited to saturations greater than 20%.
	!!define saturation

	 do k = 1, nly
	  sol_satu(k,j)= max((sol_wc(k,j)+sol_ice(k,j))/sol_pormm(k,j),0.0001)                       
     sol_kd(k,j)=864*((0.135*sol_bd(k,j)*1000+64.7)/ (2700-0.947*sol_bd(k,j)*1000)+ pp)    !! J/(cm d C)           
     end do

     do k = 1, nly
      if (  (sol_ice(k,j)/sol_wc(k,j)) > 1) then
	   sol_ke(k,j)= sol_satu(k,j)    !! J/(cm d C)
       sol_ksat(k,j)=(864*2.9)**(1-sol_por(k,j))*(864*2.2)**(sol_por(k,j)-sol_wcv(k,j))*(0.57*864)**sol_wcv(k,j) 
	  else
       if (sol_satu(k,j)>0.1) then
	    sol_ke(k,j)=log10(sol_satu(k,j))+1
	    else 
       sol_ke(k,j)=0
	    end if
      sol_ksat(k,j)=(864*0.57)**sol_por(k,j)* (2.9*864)**(1-sol_por(k,j))          
     end if
	 end do

    !! sol_ksat is soil thermal conductivity in the saturated state
	 !! sol_kd is soil thermal conductivity in the dry state
	 !! sol_ke is a dimensionless function of soil saturation

     do k = 1, nly
	  k_sol(k,j)=(sol_ksat(k,j)-sol_kd(k,j))*sol_ke(k,j)+sol_kd(k,j)  
	  end do

	
	case(2)
	
	!tkwat=0.57  !! thermal conductivity of water W/ m K
	!tkice=2.29  !! thermal conductivity of ice W/ m K
	!om_tkd=0.05  !! thermal conducitivty of dry origanic soil
	!om_tkm=0.25 !!thermal conductivity of organic soil 
	!denh2o = 1000.   !!kg/m3
   !   denice = 916.7   !!kg/m3
	!do k =1, nly
	!om_frac(k,j)=min(1.,(sol_cbn(k,j)/100)*sol_bd(k,j)*1000 / 130)
	!watsat(k,j) = 0.489 - 0.00126 * sol_sand(k,j) !! Eq.7.91
   !   om_watsat(k,j) = max(0.93 - 0.1*(sol_nd(k,j)/1000/0.5), 0.83) 
   !   watsat(k,j) =(1.- om_frac(k,j))*watsat(k,j)
   !  &              +om_watsat(k,j)*om_frac(k,j)
	
	!watsat(k,j)=sol_por(k,j)   !SWAT POROSITY
	  
	!h2osoi_ice(k,j)=sol_icev(k,j)*(sol_thic(k,j)/1000)*denice
	!h2osoi_liq(k,j)=sol_wcv(k,j)*(sol_thic(k,j)/1000)*denh2o
	 
	
	!tkm(k,j) = (1.-om_frac(k,j))*(8.80*sol_sand(k,j)+2.92*sol_clay(k,j))
   !  &          /(sol_sand(k,j)+sol_clay(k,j))+om_tkm*om_frac(k,j) ! Thermal conductivity of soil solids W/(m K)
	!tkmg(k,j)   = tkm(k,j) ** (1.- watsat(k,j))           
     
      !! dry thermal conductivity W/ m K
   !   tkdry(k,j) = ((0.135*sol_bd(k,j)+64.7)/(2.7e3-0.947*sol_bd(k,j)))
   !  &              *(1.-om_frac(k,j)) +  om_tkd * om_frac(k,j) 
                            
	
	
	! satw(k,j) = (h2osoi_liq(k,j)/denh2o + h2osoi_ice(k,j)/denice)
  !   &               /(sol_thic(k,j)/1000*watsat(k,j))
   !    satw(k,j) = min(1., satw(k,j))
   !     if (satw(k,j) > .1e-6) then
        !!Kersten number
   !       if (sol_tmp(k,j) >= 0) then       ! Unfrozen soil
   !          dke(k,j) = max(0. , log10(satw(k,j)) + 1.)
   !       else                               ! Frozen soil
    !         dke(k,j) = satw(k,j)
    !      end if
          !! Saturated thermal conductivity W/ m K
    !      fl = (h2osoi_liq(k,j)/(denh2o*sol_thic(k,j)/1000)) 
    ! &         / (h2osoi_liq(k,j)/(denh2o*sol_thic(k,j)/1000)
   !  &              +  h2osoi_ice(k,j)/(denice*sol_thic(k,j)/1000))
   !       dksat(k,j) = tkmg(k,j)*tkwat**(fl*watsat(k,j))*tkice**((1.-fl)
   !  &                         *watsat(k,j))
   !       thk(k,j) = dke(k,j)*dksat(k,j) + (1.-dke(k,j))*tkdry(k,j)
    !    else
    !      thk(k,j) = tkdry(k,j)   !!W/ m K
      !   endif
	
	!k_sol(k,j)=thk(k,j)*864   !!J/ cm C d
	
	! end do
	
	
	
      end select


       
!!------------------------------------------Calibration---------------------------
        
	  do k = 1, nly
       k_sol(k,j)= k_sol(k,j)*k_coe(k,j) 
      end do
!!-------------------------------------------------------------------------------------
 1238 format (4x, f9.2, f9.2, f9.2, f9.2)   !! March 20 2012
      return
      end
