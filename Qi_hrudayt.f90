      subroutine hrudayt

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes daily HRU output to the output.hru file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    aird(:)       |mm H2O        |amount of water applied to HRU on current
!!                                 |day
!!    auton         |kg N/ha       |amount of nitrogen applied in auto-fert
!!                                 |application
!!    autop         |kg P/ha       |amount of phosphorus applied in auto-fert
!!                                 |application
!!    bactrolp      |# colonies/ha |less persistent bacteria transported to main
!!                                 |channel with surface runoff
!!    bactrop       |# colonies/ha |persistent bacteria transported to main
!!                                 |channel with surface runoff
!!    bactsedlp     |# colonies/ha |less persistent bacteria transported with
!!                                 |sediment in surface runoff
!!    bactsedp      |# colonies/ha |persistent bacteria transported with
!!                                 |sediment in surface runoff
!!    bio_ms(:)     |kg/ha         |land cover/crop biomass (dry weight)
!!    cfertn        |kg N/ha       |amount of nitrogen added to soil in
!!                                 |continuous fertilizer operation on day
!!    cfertp        |kg P/ha       |amount of phosphorus added to soil in
!!                                 |continuous fertilizer operation on day
!!    cnday(:)      |none          |curve number for current day, HRU and at
!!                                 |current soil moisture
!!    cpnm(:)       |NA            |four character code to represent crop name
!!    deepirr(:)    |mm H2O        |amount of water removed from deep aquifer
!!                                 |for irrigation
!!    deepst(:)     |mm H2O        |depth of water in deep aquifer
!!    etday         |mm H2O        |actual amount of evapotranspiration that
!!                                 |occurs on day in HRU
!!    fertn         |kg N/ha       |total amount of nitrogen added to soil in
!!                                 |HRU on day in fertilizer application
!!    fertp         |kg P/ha       |total amount of phosphorus added to soil in
!!                                 |HRU on day in fertilizer application
!!    fixn          |kg N/ha       |amount of nitrogen added to plant biomass
!!                                 |via fixation on the day in HRU
!!    grazn         |kg N/ha       |amount of nitrogen added to soil in grazing
!!                                 |on the day in HRU
!!    grazp         |kg P/ha       |amount of phosphorus added to soil in
!!                                 |grazing on the day in HRU
!!    gw_q(:)       |mm H2O        |groundwater contribution to streamflow from
!!                                 |HRU on current day
!!    gwseep        |mm H2O        |amount of water recharging deep aquifer on
!!                                 |current day
!!    hmntl         |kg N/ha       |amount of nitrogen moving from active
!!                                 |organic to nitrate pool in soil profile
!!                                 |on current day in HRU
!!    hmptl         |kg P/ha       |amount of phosphorus moving from the
!!                                 |organic to labile pool in soil profile
!!                                 |on current day in HRU
!!    hru_ha(:)     |ha            |area of HRU in hectares
!!    hru_km(:)     |km^2          |area of HRU in square kilometers
!!    hru_ra(:)     |MJ/m^2        |solar radiation for the day in HRU
!!    hrugis(:)     |none          |GIS code printed to output files
!!                                 |(output.hru, output.rch)
!!    icr(:)        |none          |sequence number of crop grown within the
!!                                 |current year
!!    iida          |julian date   |current day of simulation
!!    idplt(:)      |none          |land cover code from crop.dat
!!    ihru          |none          |HRU number
!!    ipdvas(:)     |none          |output variable codes for output.hru file
!!    isproj        |none          |special project code:
!!                                 |1 test rewind (run simulation twice)
!!    itots         |none          |number of output variables printed 
!!                                 |(output.hru)
!!    laiday(:)     |none          |leaf area index for HRU
!!    latno3(:)     |kg N/ha       |amount of NO3-N in lateral flow in HRU for
!!                                 |the day
!!    latq(:)       |mm H2O        |amount of water in lateral flow in HRU for
!!                                 |the day
!!    mhruo         |none          |maximum number of variables written to
!!                                 |HRU output file (output.hru)
!!    minpgw(:)     |kg P/ha       |soluble P loading to reach in groundwater
!!    nmgt(:)       |none          |management code (for GIS output only)
!!    no3gw(:)      |kg N/ha       |nitrate loading to reach in groundwater
!!    no3pcp        |kg N/ha       |nitrate added to the soil in rainfall
!!    nro(:)        |none          |sequence number of year in rotation
!!    nplnt(:)      |kg N/ha       |plant uptake of nitrogen in HRU for the day
!!    percn(:)      |kg N/ha       |NO3-N leached from soil profile during the
!!                                 |day
!!    pet_day       |mm H2O        |potential evapotranspiration for day in HRU
!!    pplnt(:)      |kg P/ha       |plant uptake of phosphorus in HRU for the 
!!                                 |day
!!    qday          |mm H2O        |surface runoff loading to main channel for
!!                                 |day in HRU
!!    qdr(:)        |mm H2O        |total amount of water entering main channel
!!                                 |for day from HRU
!!    rchrg(:)      |mm H2O        |amount of water recharging both aquifers on
!!                                 |current day in HRU
!!    revapday      |mm H2O        |amount of water moving from the shallow
!!                                 |aquifer into the soil profile or being taken
!!                                 |up by plant roots in the shallow aquifer
!!    rmn2tl        |kg N/ha       |amount of nitrogen moving from the fresh
!!                                 |organic (residue) to the nitrate(80%) and
!!                                 |active organic(20%) pools in soil profile
!!                                 |on current day in HRU
!!    rmp1tl        |kg P/ha       |amount of phosphorus moving from the labile
!!                                 |mineral pool to the active mineral pool in
!!                                 |the soil profile on the current day in the
!!                                 |HRU
!!    rmptl         |kg P/ha       |amount of phosphorus moving from the
!!                                 |fresh organic (residue) to the labile(80%)
!!                                 |and organic(20%) pools in soil profile
!!                                 |on current day in HRU
!!    roctl         |kg P/ha       |amount of phosphorus moving from the active
!!                                 |mineral pool to the stable mineral pool
!!                                 |in the soil profile on the current day in
!!                                 |the HRU
!!    rwntl         |kg N/ha       |amount of nitrogen moving from active
!!                                 |organic to stable organic pool in soil
!!                                 |profile on current day in HRU
!!    sedminpa(:)   |kg P/ha       |amount of active mineral phosphorus sorbed
!!                                 |to sediment in surface runoff in HRU for day
!!    sedminps(:)   |kg P/ha       |amount of stable mineral phosphorus sorbed
!!                                 |to sediment in surface runoff in HRU for day
!!    sedorgn(:)    |kg N/ha       |amount of organic nitrogen in surface runoff
!!                                 |in HRU for the day
!!    sedorgp(:)    |kg P/ha       |amount of organic phosphorus in surface
!!                                 |runoff in HRU for the day
!!    sedyld(:)     |metric tons   |daily soil loss caused by water erosion
!!    sepbtm(:)     |mm H2O        |seepage leaving the bottom of the soil
!!                                 |profile on day in HRU
!!    shallirr(:)   |mm H2O        |amount of water removed from shallow aquifer
!!                                 |for irrigation
!!    shallst(:)    |mm H2O        |depth of water in shallow aquifer
!!    snofall       |mm H2O        |amount of precipitation falling as freezing
!!                                 |rain/snow on day in HRU
!!    snomlt        |mm H2O        |amount of water in snow melt for the day in
!!                                 |HRU
!!    sol_cnsw(:)   |mm H2O        |soil water content used to calculate daily
!!                                 |CN value (initial soil wter content for day)
!!    sol_sw(:)     |mm H2O        |amount of water stored in the soil profile
!!                                 |at end of any given day
!!    sol_tmp(2,:)  |deg C         |daily average temperature of second soil 
!!                                 |layer
!!    strsn(:)      |none          |fraction of potential plant growth achieved
!!                                 |on the day where the reduction is caused by
!!                                 |nitrogen stress
!!    strsp(:)      |none          |fraction of potential plant growth achieved
!!                                 |on the day where the reduction is caused by
!!                                 |phosphorus stress
!!    strstmp(:)    |none          |fraction of potential plant growth achieved
!!                                 |on the day in HRU where the reduction is
!!                                 |caused by temperature stress
!!    strsw(:)      |none          |fraction of potential plant growth achieved
!!                                 |on the day where the reduction is caused by
!!                                 |water stress
!!    subp(:)       |mm H2O        |precipitation for the day in HRU
!!    surfq(:)      |mm H2O        |surface runoff generated on day in HRU
!!    surqno3(:)    |kg N/ha       |amount of NO3-N in surface runoff in HRU for
!!                                 |the day
!!    surqsolp(:)   |kg P/ha       |amount of soluble phosphorus in surface
!!                                 |runoff in HRU for the day
!!    tloss         |mm H2O        |amount of water removed from surface runoff
!!                                 |via transmission losses on day in HRU
!!    tmn(:)        |deg C         |minimum temperature for the day in HRU
!!    tmpav(:)      |deg C         |average temperature for the day in HRU
!!    tmx(:)        |deg C         |maximum temperature for the day in HRU
!!    usle          |metric tons   |daily soil loss predicted with USLE equation
!!    wdntl         |kg N/ha       |amount of nitrogen lost from nitrate pool
!!                                 |by denitrification in soil profile on
!!                                 |current day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    j           |none          |HRU number
!!    pdvas(:)    |varies        |array to hold HRU output values
!!    pdvs(:)     |varies        |array to hold selected HRU output values
!!                               |when user doesn't want to print all
!!    sb          |none          |subbasin number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, sb, ii, iflag
      real, dimension (mhruo) :: pdvas, pdvs
      character (len=4) :: cropname
      j = 0
      j = ihru
      sb = hru_sub(j)
      iflag = 0
      do ii = 1, itoth
        if (ipdhru(ii) == j) iflag = 1
      end do
      if (iflag == 0) return

      pdvas = 0.
      pdvs = 0.

      pdvas(1) = ca_sol(1,j)
      pdvas(2) = ca_sol(2,j)
      pdvas(3) = ca_sol(3,j)
      pdvas(4) = ca_sol(4,j)
      pdvas(5) = ca_sol(5,j)
      pdvas(6) = ca_sol(6,j)
      pdvas(7) = k_sol(1,j)
      pdvas(8) = k_sol(2,j)
      pdvas(9) = k_sol(3,j)
      pdvas(10) = k_sol(4,j)
      pdvas(11) = k_sol(5,j)
      pdvas(12) = k_sol(6,j)
      pdvas(13) = ca_sno(j)
      pdvas(14) = k_sno(j)
      pdvas(15) = sur_tmp(j)
      pdvas(16) = bot_tmp(j)
      pdvas(17) = snotmp1(j)
      pdvas(18) = t_bare(j)
      pdvas(19) = ambtmp(j)
      pdvas(20) = snosurtmp(j)   
      pdvas(21) = sol_tmp(1,j)               
      pdvas(22) = sol_tmp(2,j)              
      pdvas(23) = sol_tmp(3,j)              
      pdvas(24) = sol_tmp(4,j)               
      pdvas(25) = sol_tmp(5,j)               
      pdvas(26) = sol_tmp(6,j)
      pdvas(27) = sol_tmp(7,j)
      pdvas(28) = sol_tmp(8,j)
      pdvas(29) = sol_tmp(9,j)
      pdvas(30) = sol_tmp(10,j)
      pdvas(31) = sol_icev(1,j)
      pdvas(32) = sol_icev(2,j)
      pdvas(33) = sol_icev(3,j)
      pdvas(34) = sol_icev(4,j)
      pdvas(35) = sol_icev(5,j)
      pdvas(36) = sol_wcv(1,j) 
      pdvas(37) = sol_wcv(2,j) 
      pdvas(38) = sol_wcv(3,j) 
      pdvas(39) = sol_wcv(4,j) 
      pdvas(40) = sol_wcv(5,j)
      pdvas(41) = sol_wpmm(1,j)/sol_thic(1,j)
      pdvas(42) = sol_wpmm(2,j)/sol_thic(2,j)
      pdvas(43) = sol_wpmm(3,j)/sol_thic(3,j)
      pdvas(44) = sol_wpmm(4,j)/sol_thic(4,j)  
      pdvas(45) = sol_wpmm(5,j)/sol_thic(5,j)
      pdvas(46) = 1984
      pdvas(47) = sol_ul(1,j)/sol_thic(1,j) 
      pdvas(48) = sol_ul(2,j)/sol_thic(2,j) 
      pdvas(49) = sol_ul(3,j)/sol_thic(3,j)  
      pdvas(50) = sol_ul(4,j)/sol_thic(4,j) 
      pdvas(51) = sol_ul(5,j)/sol_thic(5,j)
      pdvas(52) = sol_st(1,j)/sol_thic(1,j)
      pdvas(53) = sol_st(2,j)/sol_thic(2,j)  
      pdvas(54) = sol_st(3,j)/sol_thic(3,j)
      pdvas(55) = sol_st(4,j)/sol_thic(4,j)
      pdvas(56) = sol_st(5,j)/sol_thic(5,j)
      pdvas(57) = sol_air(1,j)/sol_thic(1,j)
      pdvas(58) = sol_air(2,j)/sol_thic(2,j)
      pdvas(59) = sol_air(3,j)/sol_thic(3,j)
      pdvas(60) = sol_air(4,j)/sol_thic(4,j)
      pdvas(61) = sol_air(5,j)/sol_thic(5,j)
      pdvas(62) = sol_orgv(1,j)
      pdvas(63) = sol_orgv(2,j)
      pdvas(64) = sol_orgv(3,j)
      pdvas(65) = sol_orgv(4,j)
      pdvas(66) = sol_orgv(5,j)      
      pdvas(67) = sol_minv(1,j)
      pdvas(68) = sol_minv(2,j)
      pdvas(69) = sol_minv(3,j) 
      pdvas(70) = sol_minv(4,j)   
      pdvas(71) = sol_minv(5,j)

!!    added current soil carbon for first layer
      pdvas(72) = eff_conr !! first soil layer only
!!    added current soil carbon integrated - aggregating all soil layers
      pdvas(73) = tmx(j)
      
!!    adding qtile to output.hru write 3/2/2010 gsm
      pdvas(74) = tmn(j)
!    tileno3 - output.hru
      pdvas(75) = laiday(j)
!    latno3 - output.hru
      pdvas(76) = blai(idplt(j)) 
!    groundwater deep
      pdvas(77) = igro(j)
      pdvas(78) = idplt(j)
!!    phos due to crack flow (tvap)
      pdvas(79) = tmpav(j)

      call xmon 
          
      if (ipdvas(1) > 0) then
        do ii = 1, itots
          pdvs(ii) = pdvas(ipdvas(ii))
        end do

      idplant = idplt(j)
      if (idplant > 0) then
        cropname = cpnm(idplant)
      else
        cropname = "NOCR"
      endif
      
      if (iscen == 1 .and. isproj == 0) then
        if (icalen == 0) write (1984,1001) cropname, j, subnum(j),        &
     &      hruno(j), sb, nmgt(j), iida, hru_km(j),                     &
     &       (pdvs(ii), ii = 1, itots)
        if (icalen == 1) write (1984,1002) cropname, j, subnum(j),        &
     &      hruno(j), sb, nmgt(j), i_mo, icl(iida), iyr, hru_km(j),     &
     &       (pdvs(ii), ii = 1, itots)
1002  format (a4,i5,1x,a5,a4,i5,1x,i4,1x,i2,1x,i2,1x,i4,1x,e10.5,       &
     & 66f10.3,1x,e10.5,1x,e10.5,8e10.3,3f10.3)
      

        else if (isproj == 1) then
        write (21,1000) cropname, j, subnum(j), hruno(j), sb,           &
     &          nmgt(j), iida, hru_km(j), (pdvs(ii), ii = 1, itots)
        else if (iscen == 1 .and. isproj == 2) then
        if(icalen == 0)write (28,1000) cropname, j, subnum(j), hruno(j),&
     &      sb, nmgt(j), iida, hru_km(j), (pdvs(ii), ii = 1, itots), iyr
        if(icalen == 1)write (28,1003) cropname, j, subnum(j), hruno(j),&
     &      sb, nmgt(j), i_mo, icl(iida), iyr, hru_km(j),               &
     &      (pdvs(ii), ii = 1, itots), iyr
1003  format(a4,i5,1x,a5,a4,i5,1x,i4,1x,i2,1x,i2,1x,i4,1x,e10.5,66f10.3,&
     &1x,e10.5,1x,e10.5,8e10.3,f10.3,1x,i4)
        end if
      else
        if (iscen == 1 .and. isproj == 0) then
        if(icalen == 0)write (28,1000) cropname, j, subnum(j), hruno(j),&
     &        sb,nmgt(j), iida, hru_km(j), (pdvas(ii), ii = 1, mhruo)
        if(icalen == 1)write (28,1003) cropname, j, subnum(j), hruno(j),&
     &        sb,nmgt(j), i_mo, icl(iida), iyr, hru_km(j),              &
     &        (pdvas(ii), ii = 1, mhruo)


        else if (isproj == 1) then
        write (21,1000) cropname, j, subnum(j), hruno(j), sb,           &
     &              nmgt(j), iida, hru_km(j), (pdvas(ii), ii = 1, mhruo)
        else if (iscen == 1 .and. isproj == 2) then
        if(icalen == 0)write (28,1000) cropname, j, subnum(j), hruno(j),& 
     &      sb,nmgt(j), iida, hru_km(j), (pdvas(ii), ii = 1, mhruo), iyr
         if(icalen == 1)write(28,1000) cropname, j, subnum(j), hruno(j),& 
     &      sb,nmgt(j), i_mo, icl(iida), iyr, hru_km(j),                &
     &      (pdvas(ii), ii = 1, mhruo), iyr
        end if
      end if 

      return

1000  format (a4,i5,1x,a5,a4,i5,1x,i4,1x,i4,e10.5,79f10.3)
1001  format (a4,i5,1x,a5,a4,i5,1x,i4,1x,i4,e10.5,79f10.3)
9000  format(i4,i4,i2,i8,21(f16.3))
9001  format(i4,i4,i8,48(f16.3))
      end