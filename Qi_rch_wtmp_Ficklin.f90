function rch_wtmp_Ficklin()

!incomming variables
!!    sub_fr(:)     |none          |fraction of watershed area in subbasin
!!    da_ha         |ha            |area of watershed in hectares
!!    inum1        |none          |reach number
!!    inum2        |none          |inflow hydrograph storage location number
!!    rnum1        |none          |fraction of overland flow
!!    rtwtr        |m^3 H2O       |flow out of reach
!!    rchwtr       |m^3 H2O       |water stored in reach at beginning of day
!! hru_sub

!local
!!    wtrin       |m^3 H2O       |water flowing into reach on day
!!    sb          |none          |subbasin number
!!    jrch        |none          |reach number
!!sub_suro_snowmelt |mm, surface runoff (subbasin) contributed by snow melt
!!wtmp_temp         |C, temporary water temperature
!! phru             |, index of hru (ihru was used in "parm")
!!    wtrtot      |m^3 H2O       |inflow + storage water
!! tmpav_lag_mean       |C              lagged average Tair for this jrch
!  tmpav_lag_std, lagged std of Tair 
! plag, running index of lag days

!wtmppara(1): date from
!wtmppara(2): date to
!wtmppara(3): alpha
!wtmppara(4): mu (u)
!wtmppara(5): gamma
!wtmppara(6): beta
!wtmppara(7): eta (n)
!wtmppara(8): lag (day)

use parm
real rch_wtmp_Ficklin,tmpav_lag_mean
integer jrch,phru,sb,plag
real wtrin,rtwtr_cms,wtmp_temp
integer datefrom, dateto,iwtmpset,wtmp_lag
real wtmp_alpha,wtmp_mu,wtmp_gamma,wtmp_beta,wtmp_eta,wtmp_X
real sub_gwq_m3,sub_wyld_m3

jrch = inum1
sb=inum1

!this function called by [watqual] when:
!if (rtwtr / 86400. > 0.01 .and. wtrin > 1.e-4)

!the procedure of water temperature calculation and use is
![1] calculate the inital river temperature based on upstream and local contribution
![2] adjust the temperare by heat transfer
![3] use the adjusted temperature for tomorrow's WQ (?)

wtrin = varoute(2,inum2) * (1. - rnum1)
!upstream temperature by wtrin
heatin = varoute(1,inum2) * (1. - rnum1)        !temp in, C
!local water contribution
sub_wyld_m3=sub_wyld(sb)*sub_fr(sb)*da_ha*10
!initial Tw is determined by remote and local contributions
wtmp_temp=(heatin*(wtrin-sub_wyld_m3)+sub_wtmp(sb)*sub_wyld_m3)/wtrin
!wtmp_temp=(heatin*(wtrin-sub_wyld_m3)+sub_wtmp(sb)*sub_wyld_m3+wattemp(jrch)*(rtwtr-wtrin))/(rchstor(jrch)+rtwtr)

!SWAT use "tmpav(jrch)" in "watqual", which is wrong. tmpav is arranged for hru, not for subbasin/reach
    
!locate the set of parameter to be used
do iwtmpset=1, 3    !wtmpset(jrch)
    datefrom=wtmppara(jrch,iwtmpset,1)
    dateto=wtmppara(jrch,iwtmpset,2)
    if(iida>=datefrom.AND.iida<=dateto) exit
end do
    
!if the input date range is not closed (not from 1 to 366)
!if(iwtmpset>wtmpset(jrch)) iwtmpset=wtmpset(jrch)
  if(iwtmpset>3) iwtmpset=  3  !wtmpset(jrch)
!fill parameters
wtmp_alpha=wtmppara(jrch,iwtmpset,3)
wtmp_mu=wtmppara(jrch,iwtmpset,4)
wtmp_gamma=wtmppara(jrch,iwtmpset,5)
wtmp_beta=wtmppara(jrch,iwtmpset,6)
wtmp_eta=wtmppara(jrch,iwtmpset,7)
wtmp_lag=wtmppara(jrch,iwtmpset,8)

    
!calculate lagged average Tair
tmpav_lag_mean=0
do plag=1,wtmp_lag
    tmpav_lag_mean=tmpav_lag_mean+tmpav_lag(plag,jrch)
end do
tmpav_lag_mean=tmpav_lag_mean/wtmp_lag    
    
    
!adjustment by air-water heat transfer, ver.4
!wtmp_temp=wtmp_temp+(tmpav_lag_mean-wtmp_temp)*min(rttime,24.0)*wtmp_beta
if(tmpav(hru1(jrch))>-5) then
    wtmp_temp=wtmp_temp+(tmpav_lag_mean-wtmp_temp)*min(rttime,24.0)*wtmp_beta
else
    wtmp_temp=wtmp_temp+(tmpav_lag_mean+wtmp_eta-wtmp_temp)*min(rttime,24.0)*wtmp_beta
end if


!a new model for testing, ver.5
!wtmp_temp=wtmp_temp+exp((tmpav_lag_mean-wtmp_temp)*wtmp_beta)*min(rttime,24.0)
    
!if (wtmp_temp <= 0.1) wtmp_temp = 0.1 !see "subwq" ! disabled by Du
if (wtmp_temp <= -0.4) wtmp_temp = -0.4 ! modified by Du,avioding some unrelealistic simulations
rch_wtmp_Ficklin=wtmp_temp

return

end function
