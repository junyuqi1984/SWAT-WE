   subroutine hru_wtmp_Ficklin
   !! developed by Junyu Qi at HRU scale based on the subbasin algorithm of Ficlin

   use parm
   implicit none
   integer:: j, sb,phru
   integer:: wtmp_lag,iwtmpset,datefrom,dateto,plag
   real :: gwtmp_ann
   real :: wtmp_alpha       ! alpha   snowmelt water temperature     0.1
   real :: wtmp_mu          ! mu    ground water temperature   
   real :: wtmp_gamma       ! gamma                1.   
   real :: wtmp_beta        ! beta           K     0.25 
   real :: wtmp_eta         ! eta            e      0. 
   real :: tmpav_lag_mean
   real :: suro_snowmelt
    j = 0
    j = ihru
   sb=inum1
   
   do iwtmpset=1,3
    datefrom = wtmppara(sb,iwtmpset,1)
    dateto = wtmppara(sb,iwtmpset,2)
    if(iida>=datefrom.AND.iida<=dateto) exit
   end do
   
   
   wtmp_alpha = 0.      ! alpha   snowmelt water temperature     0.1
   wtmp_mu = 0.         ! mu      ground water temperature   
   wtmp_gamma = 0.      ! gamma                 1.   
   wtmp_beta = 0.       ! beta           K      0.25 
   wtmp_eta = 0.        ! eta            e      0. 
   wtmp_lag = 0         ! lag   
   
   wtmp_alpha = wtmppara(sb,iwtmpset,3)      ! alpha   snowmelt water temperature     0.1
   wtmp_mu = wtmppara(sb,iwtmpset,4)         ! mu      ground water temperature   
   wtmp_gamma = wtmppara(sb,iwtmpset,5)      ! gamma                 1.   
   wtmp_beta = wtmppara(sb,iwtmpset,6)       ! beta           K      0.25 
   wtmp_eta = wtmppara(sb,iwtmpset,7)        ! eta            e      0. 
   wtmp_lag = wtmppara(sb,iwtmpset,8)        ! lag                   7

   tmpav_lag_mean=0.
   do plag = 1, wtmp_lag
   tmpav_lag_mean = tmpav_lag_mean + tmpav_lag(plag,sb)
   end do
   tmpav_lag_mean = tmpav_lag_mean/wtmp_lag   

    if(snomlt <0.000001 .or. qday <0.000001) then
    suro_snowmelt = 0.
    else
    suro_snowmelt = qday*snomlt/(snomlt+ subp(j))    
    end if             
    
    if (qday>0.) then
     wtmp_surq(j)= (max(tmpav_lag_mean,-1.0)*wtmp_gamma*(qday-suro_snowmelt) + suro_snowmelt*wtmp_alpha)/ qday !! mixing snowmelt and runoff   
    else
     wtmp_surq(j)= 0.
    end if 

    if(latq(j) > 0.000001) then
     wtmp_latq(j)= max(tmpav_lag_mean,-1.0)*wtmp_gamma   
    else 
     wtmp_latq(j)= 0.
    end if

    if(gw_q(j)>0.) then
     wtmp_gwq(j)= wtmp_mu
    else
     wtmp_gwq(j)=0.
    end if
    
    if(wtmp_surq(j)<-1.0) wtmp_surq(j)=-1.0
    if(wtmp_gwq(j)<-1.0) wtmp_gwq(j) = -1.0
    if(wtmp_latq(j) <-1.0) wtmp_latq(j) = -1.0            
                
   return
   end