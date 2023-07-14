        subroutine soltphys_out  
        use parm
        implicit none
        integer :: j, k, nly  
        real:: a, b, c
          
          j = 0
          j = ihru
	    nly = 0
          nly = sol_nly(j)
        
         do k = 1,nly
          sol_air(k,j) = sol_pormm(k,j)- sol_wc(k,j)-sol_ice(k,j)   !!mm
          sol_ice(k,j)= max(sol_ice(k,j) , 0.0)  !!mm
          sol_ice(k,j)= min(sol_pormm(k,j) , sol_ice(k,j))
          sol_wc(k,j) = max(sol_wc(k,j) , 0.0)   !!mm
          sol_wc(k,j) = min(sol_pormm(k,j) , sol_wc(k,j)) 
          sol_air(k,j)= max(sol_air(k,j) , 0.0)  !!mm
          sol_air(k,j)= min(sol_pormm(k,j) , sol_air(k,j))  
          
         ! sol_wpmm(k,j) = sol_wp(k,j) * sol_thic(k,j) !!mm
         ! sol_fc(k,j) = sol_thic(k,j) * (sol_up(k,j) - sol_wp(k,j)) !!mm
         if( sol_ice(k,j) <= sol_wpmm(k,j) ) then
	     !sol_wpmm(k,j) = sol_wpmm(k,j)- sol_ice(k,j)
	     sol_st(k,j) = sol_wc(k,j)-(sol_wpmm(k,j) - sol_ice(k,j))
	     !sol_ul(k,j) = sol_pormm(k,j)- (sol_wpmm(k,j) - sol_ice(k,j)) !!mm
	     !sol_fc(k,j)= sol_fc(k,j)/2.
	   else 
           sol_st(k,j) = sol_wc(k,j)
          ! sol_ul(k,j) = sol_pormm(k,j)- sol_ice(k,j)
          ! if( sol_ice(k,j) <= (sol_fc(k,j)+sol_wpmm(k,j)) ) then
          ! sol_fc(k,j) = sol_fc(k,j)-( sol_ice(k,j)-sol_wpmm(k,j) ) !1mm
          ! sol_wpmm(k,j) = 0.1
          ! else
         ! a=0.
         ! b=0.
         ! c=0.
         ! a = sol_fc(k,j)
         ! c = (sol_por(k,j)-sol_wp(k,j))*sol_thic(k,j)
         ! b = (c-sol_fc(k,j)) / sol_fc(k,j) 
          !sol_fc(k,j) = a+ b*sol_ice(k,j)
         ! sol_fc(k,j) = min (sol_ul(k,j),sol_fc(k,j)) 
          ! sol_fc(k,j) = min (sol_ul(k,j),3*sol_fc(k,j)) !sol_st(k,j)  !mm cannot be 0 because "/sol_fc" in other files
          ! sol_wpmm(k,j) = 0.1
          ! end if
	    
	    endif
		  
		  sol_st(k,j) = max(sol_st(k,j),0.0)
		  sol_st(k,j)= min(sol_pormm(k,j)-sol_wpmm(k,j) , sol_st(k,j)) 
		  
          !sol_ul(k,j) = max(sol_ul(k,j),0.0)
	   ! sol_ul(k,j)= min(sol_pormm(k,j)-sol_wpmm(k,j) , sol_ul(k,j)) 
	
         end do
         
         
         return
         end