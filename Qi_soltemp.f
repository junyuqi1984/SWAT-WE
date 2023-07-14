      subroutine soltemp  
      use parm
      implicit none
      integer :: j, k, nly 
      
   
       j = 0
       j = ihru
	 nly = 0
       nly = sol_nly(j)
       
       select case (1) 
       
       case(1) !!default
       eff_coe(j) = 50.
       k_coe(1,j) =10
       do k = 2, nly
       k_coe(k,j) = 10.  
	   end do
       ks_coe(j) = 1.
       c_coe(j) = 1.
    
       case(2)  !calibration
       eff_coe(j) = pot_tilemm(j) !50
       k_coe(1,j) = pot_nsed(j)  !! residue calibration
        do k = 2, nly
       k_coe(k,j) = pot_volxmm(j) !10  
        end do
       ks_coe(j) = pot_volmm(j) !1   snow heat conduc.
       c_coe(j) = 1. !pot_nsed(ihru)  !1    soil heat capacity
      
       case(3)  !original calibration
          if ( pot_tilemm(j) <= 0. ) then
       eff_coe(j) = 50.
       else
       eff_coe(j)  = pot_tilemm(j) !50
       end if
        
       if ( pot_volxmm(j) <=0. ) then
         do k = 1, nly
         k_coe(k,j) = 10. 
         end do
       else 
         do k = 1, nly
         k_coe(k,j) = pot_volxmm(j) !10  
         end do
       end if
        
        if ( pot_volmm(j) <=0. ) then
        ks_coe(j) =1.
       else
       ks_coe(j) = pot_volmm(j) !1   snow heat conduc.
      end if
     
       if ( pot_nsed(j)  <=0. ) then
       c_coe(j) =1.
       else
       c_coe(j) = pot_nsed(j)  !1    soil heat capacity
       end if
 
       end select
      
      
	 call soltphys_in
       
       call solthermal      
       
       call soltly  
      
	 call soltbott
       
       call soltsurf
        
	 call soltcal

	 call soltphys_out
	 	   
       call soltout    
   
      !call hrudelt_tmp

      return
      end
