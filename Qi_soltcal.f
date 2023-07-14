      subroutine soltcal
      use parm
      implicit none
      integer :: j, k, nly,solft
	real :: snot

       j = 0
       j = ihru
       nly = 0
       nly = sol_nly(j)

       
       !! d/dx(k*dT/dx)+ S=0

      !! ai*Ti=bi*T(i+1)+ci*T(i-1) +di
      !! ai=bi+ci
	!! bi=k(i+1)/dx(i+1)
	!! ci=k(i-1)/dx(i-1)
      !! di=S*dx
	!! S=f(T)
	!! Pi=bi/( ai-ci*P(i-1) )
	!! Qi=(di+ci*Q(i-1))/(ai-ci*P(i-1))

       if (snoco(j)>0.5 )then
       !!if (sno_hru(j)>0 )then
        k_ss(j)=k_sno(j)*k_sol(1,j)*(sno_dep(j)/10/2+sol_thic(1,j)/10/2)
     &         /(k_sno(j)*sol_thic(1,j)/10/2+k_sol(1,j)*sno_dep(j)/10/2)
       else
	  k_ss(j)=k_sol(1,j)
       end if

       do k = 1, nly1-1
            k_int(k)=k_sol(k,j)*k_sol(k+1,j)*(sol_thic(k,j)/10/2
     &		  +sol_thic(k+1,j)/10/2)  /  (k_sol(k,j)*sol_thic(k+1,j)
     &             /10/2+k_sol(k+1,j)*sol_thic(k,j)/10/2)
            b_sol(k)=k_int(k)              !! j/(cm^2.d.C)
     &             /(sol_thic(k,j)/10/2+sol_thic(k+1,j)/10/2) 
            c_sol(k+1)=k_int(k)            !! j/(cm^2.d.C) 
     &             /(sol_thic(k,j)/10/2+sol_thic(k+1,j)/10/2) 
       end do

            k_int(nly1)=k_sol(nly1,j)/(sol_thic(nly1,j)/10/2)
            b_sol(nly1)=k_int(nly1)/(sol_thic(nly1,j)/10/2) 

	 if ( snoco(j)>0.5 ) then
       !!if (sno_hru(j)>0 )then
            c_sol(1)=k_ss(j)/(sno_dep(j)/10/2+sol_thic(1,j)/10/2)  
       else
	      c_sol(1)=k_int(1)/(sol_thic(1,j)/10/2) 
       end if
!!------------------------------------------------------------------------------------
      solft=1 !! solft=0 no freeze and thaw; solft=1 with freeze and thaw
      if(solft==0) then
       do k=1,nly1
          a_sol(k)=b_sol(k)+c_sol(k)+ca_sol(k,j)*(sol_thic(k,j)/10)/1 !!J/(cm^2 d C)
          d_sol(k)=ca_sol(k,j)*(sol_thic(k,j)/10)*sol_tmp1(k,j)/1  !!J/(cm^2 d)
       end do
	end if

	if(solft==1) then 
       do k=1,nly1
          a_sol(k)=b_sol(k)+c_sol(k)+ca_sol(k,j)*(sol_thic(k,j)/10)/1 !!J/(cm^2 d C)
	 call soltfretha(k)!! latent heat source    
       end do
	end if
       

!!-------------------------------------------------------------------------------------

         !!calculation of ai,bi,ci di, Pi ,Qi for equations
	!!Ti=Pi*T(i+1)+Qi
	!!Pi=bi/(ai-ci*P(i-1) )
	!!Qi=(di+ci*Q(i-1))/(ai-ci*P(i-1))
	!!with surface temperature To:
	!!P1=b1/a1
	!!Q1=(c1*To+d1)/a1

      !!with bottom temperautre T(n+1):
	!! Tn=Pn*T(n+1)+Qn
	!! then we can calcualte T(n-1) ,T(n-2), T(n-2)...T2, T1

       if ( snoco(j)>0.5 ) then
       !!if (sno_hru(j)>0 )then
            b_sno(j)=k_ss(j)/(sno_dep(j)/10/2+sol_thic(1,j)/10/2) 
            c_sno(j)=k_sno(j)/(sno_dep(j)/10/2) 
            
		  a_sno(j)=b_sno(j)+c_sno(j)+ca_sno(j)*(sno_dep(j)/10)/1    !!J/(cm^2 d C)
            !!call snomt
		  d_sno(j)=ca_sno(j)*(sno_dep(j)/10)*snotmp1(j)/1


            p_sno(j)=b_sno(j) / (a_sno(j)-c_sno(j)*0)
            q_sno(j)=(d_sno(j)+c_sno(j)*sur_tmp(j))
     &          / (a_sno(j)-c_sno(j)*0.)
       else
	      b_sno(j)=0
            c_sno(j)=0
            a_sno(j)=0
            d_sno(j)=0
            p_sno(j)=0
            q_sno(j)=0
       end if


       if (snoco(j)>0.5 ) then
       !!if (sno_hru(j)>0 )then
            p_sol(1)=b_sol(1)/(a_sol(1)-c_sol(1)*p_sno(j))
            q_sol(1)=(d_sol(1)+c_sol(1)*q_sno(j))
     &		  / (a_sol(1)-c_sol(1)*p_sno(j))
       else
	      p_sol(1)=b_sol(1)/(a_sol(1)-c_sol(1)*0.)
            q_sol(1)=(d_sol(1)+c_sol(1)*sur_tmp(j))
     &  		  / (a_sol(1)-c_sol(1)*0. )
       end if

       do k=2,nly1
            p_sol(k)=b_sol(k) / (a_sol(k)-c_sol(k)*p_sol(k-1))
            q_sol(k)=(d_sol(k)+c_sol(k)*q_sol(k-1))
     &                         /(a_sol(k)-c_sol(k)*p_sol(k-1))
       end do

            sol_tmp1(nly1,j)=p_sol(nly1)*bot_tmp(j)+q_sol(nly1)

       do k=nly1-1,1,-1
            sol_tmp1(k,j)=p_sol(k)*sol_tmp1(k+1,j)+q_sol(k)
       end do
       
    
		    do k=1,nly1
		    sol_tmp(k,j)=sol_tmp1(k,j)
            end do
               

	    if(nly1<nly)then
	    do k=nly1+1,nly
          sol_tmp(k,j)= bot_tmp(j)
	    end do 
	end if

       !!------------------Snowpackage temperature update--------------------------
            presnotmp(j)=sol_tmp1(1,j)
		  snot=0
        if ( snoco(j)>0.5 ) then
        !!if (sno_hru(j)>0 )then
		  snot=p_sno(j)*sol_tmp1(1,j)+q_sno(j) 
	 else 
	     snotmp1(j)=0
       end if  
            snotmp1(j)=Min(0.*snot,snot)

             
       return
      end
       
