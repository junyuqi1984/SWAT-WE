    function rch_wtmp_Du()

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
use parm
real :: rch_wtmp_Du,tmpav_mean,temp_equ,delta_wtmp
real :: tmp_dew,pwater,cpwater,beta,tdw,fwind,kt,qsw,flowin,coef      
integer jrch,phru,sb,plag
real wtrin,rtwtr_cms,wtmp_temp,wtmp_temp1,wtmp_temp2
integer datefrom, dateto,iwtmpset,wtmp_lag
real wtmp_alpha,wtmp_mu,wtmp_gamma,wtmp_beta,wtmp_eta,wtmp_X
real sub_gwq_m3,sub_wyld_m3
real :: wtmp_add 
real:: t,rh,em,e   !! Junyu Qi

    jrch = inum1
    sb=inum1

!this function called by [watqual] when:
!if (rtwtr / 86400. > 0.01 .and. wtrin > 1.e-4)

!the procedure of water temperature calculation and use is
![1] calculate the inital river temperature based on mixing upstream and local contribution
![2] adjust the temperare by heat transfer
![3] use the adjusted temperature for tomorrow's WQ (?)

!
    tmpav_mean=tmpav(hru1(jrch))
    wtrin = 0.
    wtrin = varoute(2,inum2) * (1. - rnum1)
!
!upstream temperature by wtrin
    heatin =0.
    heatin = varoute(1,inum2) * (1. - rnum1)        !temp in, C
!local water contribution
    sub_wyld_m3=sub_wyld(sb)*sub_fr(sb)*da_ha*10
!initial Tw is determined by remote and local contributions
    wtmp_temp=(heatin*(wtrin-sub_wyld_m3)+sub_wtmp(sb)*sub_wyld_m3+wattemp(jrch)*rchwtr)/(wtrin + rchwtr) ! modified by Du


!fill parameters

!tmp_dew=tmpav_mean-(100-100*rhd(hru1(jrch)))/5  ! calculating dew point temperature based on air temperature  
!if (iida>91.and. iida<273 .and. tmpav_mean>0.) tmp_dew=tmpav_mean+wtmp_add
    select case(1)

    case(1) 
    !wtmp_add=-2.2    !! Du used
     !wtmp_add= 3.2    !! Du  Paper
    wtmp_add= 0.     !! Default
    tmp_dew=tmpav_mean+wtmp_add

    case(2)   !! Junyu Qi
    t=tmpav(hru1(jrch))
    rh=rhd(hru1(jrch)) 
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

    !tmp_dew=tmpav_mean
    pwater=999.973*(1.0-(wtmp_temp+288.9414)*(wtmp_temp-3.9863)**2/508929.2/(wtmp_temp+68.12693)) ! desity of water(kg/m3)
    cpwater=4186 ! specific heat capacity of water,J/kg/℃

    tdw=(tmpav_mean+tmp_dew)/2
    beta=0.35+0.015*tdw+0.0012*tdw**2
    fwind=9.2+0.46*(u10(hru1(jrch)))**2
    !fwind=3.3*u10(hru1(jrch))                                   
    kt=4.5+0.05*wtmp_temp+beta*fwind+0.47*fwind !calculating the overall heat exchange coeffcient                                          
!
    qsw=hru_ra(hru1(jrch))/11.574  ! solar radiation unit conversion from MJ/m2*day to W/m2
    temp_equ=0.
    temp_equ=tmp_dew+qsw/kt           ! calculating water equilibrium temprature
!
!adjustment by air-water heat transfer, modified by Du using equilibrium temprature method

  !! Junyu Qi---------------------------
    if(rchdep>0.05) then  !! added by Junyu Qi  assuming that when water depth <0.05m, water temperature=equilibrium temprature to reduce numeric error
    delta_wtmp=kt*(temp_equ-wtmp_temp)/(pwater*cpwater*rchdep)*min(rttime/24,1.0)*86400 !*(top_width(jrch)*ch_l2(jrch)/(rchstor(jrch)+0.01))
    if (delta_wtmp> 10.0) delta_wtmp=6.0   
    wtmp_temp=wtmp_temp+delta_wtmp   !add the wtmp change
    else
    wtmp_temp=temp_equ
    end if 
!! Junyu Qi--------------------------

!
    if (wtmp_temp <= -1.0) wtmp_temp = -1.0 ! modified by Du, avioding some unrelealistic simulations
    if (wtmp_temp> 40.0) wtmp_temp = 40.0  ! modified by Du,avioding some unrelealistic simulations
    rch_wtmp_Du=wtmp_temp

    return

    end function
