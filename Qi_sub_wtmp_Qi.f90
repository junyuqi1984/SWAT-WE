     subroutine sub_wtmp_Qi 
     use parm
     implicit none
     integer ::sb
       !!    sub_qd(:)   |mm H2O        |surface runoff loading on day in subbasin
       !!    sub_gwq(:)  |mm H2O        |groundwater loading on day in subbasin
       !!    sub_wyld(:) |mm H2O        |water yield on day in subbasin
       !!    qdr(:)      |mm H2O        |total amount of water entering main channel
       !!    qdr(j) = qday + latq(j) + gw_q(j) + qtile + gw_qdeep(j)
       sb = inum1
     
       !sub_wtmp_surq(sb)= sub_wtmp_surq(sb) + wtmp_surq(j)* hru_fr(j)
       !sub_wtmp_latq(sb)= sub_wtmp_latq(sb) + wtmp_latq(j)* hru_fr(j)
       !sub_wtmp_gwq(sb)= sub_wtmp_gwq(sb) + wtmp_gwq(j)* hru_fr(j)
       !sub_wtmp_gwdp(sb)= sub_wtmp_gwdp(sb) + wtmp_gwq(j)* hru_fr(j)
       !sub_gwq_deep(sb)= sub_gwq_deep(sb) + gw_qdeep(j)* hru_fr(j) 
     
       !sub_wtmp(sb)=(  sub_wtmp_surq(sb) *sub_qd(sb)  + sub_wtmp_latq(sb)* sub_latq(sb)+ sub_wtmp_gwq(sb)*sub_gwq(sb) + sub_wtmp_gwdp(sb)* sub_gwq_deep(sb) ) / sub_wyld(sb) 
    
       if(sub_qd(sb) > 0.000001) then
       sub_wtmp_surq(sb)= sub_wtmp_surq(sb)/sub_qd(sb)  
       else
       sub_wtmp_surq(sb)= 0.
       end if 
       
       if(sub_latq(sb) > 0.000001) then
       sub_wtmp_latq(sb)= sub_wtmp_latq(sb)/sub_latq(sb) 
       else
       sub_wtmp_latq(sb)= 0.
       end if 
       
       if(sub_gwq(sb) > 0.000001) then
       sub_wtmp_gwq(sb)= sub_wtmp_gwq(sb)/sub_gwq(sb) 
       else
       sub_wtmp_gwq(sb)= 0.
       end if 
                   
       if(sub_gwq_d(sb)  > 0.000001) then
       sub_wtmp_gwdp(sb)= sub_wtmp_gwdp(sb)/ sub_gwq_d(sb) 
       else
       sub_wtmp_gwdp(sb)= 0.
       end if                         
                 
         
       if(sub_wyld(sb)>0.000001) then
       sub_wtmp(sb)=(  sub_wtmp_surq(sb) *sub_qd(sb) + sub_wtmp_latq(sb)* sub_latq(sb)+ sub_wtmp_gwq(sb)*sub_gwq(sb) + sub_wtmp_gwdp(sb)* sub_gwq_d(sb)   ) / sub_wyld(sb) 
       else
       sub_wtmp(sb) = 0.
       endif 
       if(sub_wtmp(sb) <= -1.0) sub_wtmp(sb) = -1.0
       
       if(sub_wtmp_surq(sb) <= -1.0) sub_wtmp_surq(sb)= -1.0
       if(sub_wtmp_latq(sb) <= -1.0) sub_wtmp_latq(sb)= -1.0
       if(sub_wtmp_gwq(sb) <= -1.0) sub_wtmp_gwq(sb)= -1.0   
       if(sub_wtmp_gwdp(sb) <= -1.0) sub_wtmp_gwdp(sb)= -1.0 
       !sub_wtmp(sb)=( sub_wtmp_surq(sb) + sub_wtmp_latq(sb)+ sub_wtmp_gwq(sb)+ sub_wtmp_gwdp(sb) ) / sub_wyld(sb) 
     
     
     
     return 
     end 