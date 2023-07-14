      subroutine hru_surfwtmp
      use parm
      implicit none
      integer :: j, k
      real :: tmpav_mean,temp_equ,delta_wtmp,wtmp_temp
      real :: tmp_dew,pwater,cpwater,beta,tdw,fwind,kt,qsw
      real :: wtmp_add , tmp_melt
      real:: t,rh,em,e   
       j = 0
       j = ihru
   

     If(isurface_tmp == 0) Then
      
      if (qday>0.) then
       !wtmp_surq(j)= sur_tmp(j)
      wtmp_surq(j)= tmpav(j)  
      if (snoco(j)>0.5 )wtmp_surq(j)= 0.1  
      else
       wtmp_surq(j)= 0.
      end if 
  
     Else
 
      if (snoco(j)>0.5 )then   
  
      wtmp_surq(j)= 0.1
  
      else
  
      tmpav_mean = tmpav(j)  
      tmp_melt =0.1
      wtmp_temp=tmpav(j)
      pwater=999.973*(1.0-(wtmp_temp+288.9414)*(wtmp_temp-3.9863)**2/508929.2/(wtmp_temp+68.12693)) ! desity of water(kg/m3)
      cpwater=4186 ! specific heat capacity of water,J/kg/¡æ
  

      SELECT CASE(2)

      CASE(1)
    
      select case(1)
    
      case(1) 
      !wtmp_add=-2.2    !! Du used
      !wtmp_add= 3.2    !! Du  Paper
      wtmp_add= 0.       !! Default
      tmp_dew=tmpav_mean+wtmp_add

      case(2)   !! Junyu Qi
      t=tmpav(j)
      rh=rhd(j) 
      em=0.
      e=0.
      if(t>=0) then
      em=611.*exp( (17.27*t)/(t+237.3) )   !!pa
      else
      em=611.*exp( (21.87*t)/(t+265.5) )
      end if
      e=rh*em
      tmp_dew=(log(e)-6.415)/(0.0999-0.00421*log(e))

      end select
 
      tdw=(tmpav_mean+tmp_dew)/2
      beta=0.35+0.015*tdw+0.0012*tdw**2
      fwind=9.2+0.46*(  u10(j)  )**2                          
      kt=4.5+0.05*wtmp_temp+beta*fwind+0.47*fwind !calculating the overall heat exchange coeffcient                                          

      qsw=hru_ra(j)/11.574            ! solar radiation unit conversion from MJ/m2*day to W/m2
      temp_equ=0.
      temp_equ=tmp_dew+qsw/kt           ! calculating water equilibrium temprature

      if(surfq(j)/1000.>0.0001) then  !!  assuming that when water depth <0.0001m, water temperature=equilibrium temprature to reduce numeric error
      delta_wtmp=kt*(temp_equ-wtmp_temp)/(pwater*cpwater*surfq(j)/1000.)*min(tconc(j),1.0)*86400 !*(top_width(jrch)*ch_l2(jrch)/(rchstor(jrch)+0.01))
!if (delta_wtmp> 10.0) delta_wtmp=6.0   
      wtmp_temp=wtmp_temp+delta_wtmp   !add the wtmp change
      else
      wtmp_temp=temp_equ
      end if 

 
      CASE(2)
 
      kt=sol_ksat(1,j)  *(24.*60.*60.)/100. / (hru_km(j)*1000000.)         ! J/(cm d C)->W/ m2 C   
      temp_equ=sur_tmp(j)  
      if(surfq(j)/1000.>0.0) then 
      delta_wtmp=kt*(temp_equ-wtmp_temp)/(pwater*cpwater*surfq(j)/1000.)*min(tconc(j)/24,1.0)*86400 
      else
      delta_wtmp=0.
      end if
      wtmp_temp=wtmp_temp+delta_wtmp   

      END SELECT

      wtmp_surq(j)=wtmp_temp
    
      end if     ! if (snoco(j)>0.5 )then   
        
      End if     !! If(isurface_tmp==0) Then
  
      if(wtmp_surq(j)<-1.0) wtmp_surq(j)=-1.0
  
  
  
  
  return
  end 