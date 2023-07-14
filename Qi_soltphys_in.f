      subroutine soltphys_in
      use parm
      implicit none
      integer :: j, k, nly
     
       j = 0
       j = ihru
       nly = 0
       nly = sol_nly(j) 
       
       sol_thic(1,j)=sol_z(1,j)    
       sol_zc(1,j)=sol_z(1,j)/2
	 do k= 2, nly
         sol_thic(k,j)= sol_z(k,j) - sol_z(k-1,j) !!mm
         sol_zc(k,j)=(sol_z(k,j)+sol_z(k-1,j))/2
       end do    
       !! depth to point k from soil surface mm
       sol_nd(1,j)=sol_z(1,j)*1/2
       do k=2,nly
       sol_nd(k,j)=sol_z(k-1,j)+sol_thic(k,j)/2
	 end do
 
 
 
      do k = 1, nly
         sol_pormm(k,j)=sol_por(k,j)*sol_thic(k,j)  !!mm
         if(sol_ice(k,j) <= sol_wpmm(k,j) ) then
	   sol_wc(k,j) = sol_st(k,j)+sol_wpmm(k,j)-sol_ice(k,j)
	   else 
         sol_wc(k,j) = sol_st(k,j)
	   endif
       sol_wcv(k,j) = sol_wc(k,j)/sol_thic(k,j)  !!mm/mm
       sol_org(k,j) = (sol_rsd(k,j)+sol_hum(k,j))/(0.5*10000) !!mm
       sol_orgv(k,j) = sol_org(k,j)/(sol_thic(k,j))!!mm/mm
       sol_minv(k,j) = sol_bd(k,j)/2.65-sol_orgv(k,j)
       sol_icev(k,j) = sol_ice(k,j)/sol_thic(k,j) !!mm/mm
       sol_air(k,j) = sol_pormm(k,j)- sol_wc(k,j)-sol_ice(k,j)

      end do
  

      return
      end
