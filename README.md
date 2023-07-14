# SWAT-WE
SWAT for coupled Water Energy cycling from soil to stream (SWAT-WE)
Read relevant papers:
Qi, J., Lee, S., Du, X., Ficklin, D.L., Wang, Q., Myers, D., Singh, D., Moglen, G.E., McCarty, G.W., Zhou, Y. and Zhang, X., 2021. Coupling terrestrial and aquatic thermal processes for improving stream temperature modeling at the watershed scale. Journal of Hydrology, 603, p.126983.
Qi, J., Li, S., Li, Q., Xing, Z., Bourque, C.-A., Meng, F.-R., 2016. A new soil-temperature module for SWAT application in regions with seasonal snow cover. J. Hydrol. 538, 863–877.
Du, X., Shrestha, N.K., Ficklin, D.L., Wang, J., 2018. Incorporation of the equilibrium temperature approach in a Soil and Water Assessment Tool hydroclimatological
stream temperature model. Hydrol. Earth Syst. Sci. 22 (4), 2343–2357.
Ficklin, D.L., Luo, Y., Stewart, I.T., Maurer, E.P., 2012. Development and application of a hydroclimatological stream temperature model within the Soil and Water
Assessment Tool. Water Resour. Res. 48 (1) https://doi.org/10.1029/2011WR011256.


Change module options in modparm.f as shown here:
       !!============= Soil temperature Control ===============
       ! tswat ::      0 = original soil temperature module; 1 = heat transfer/soil temperature module; 
       !!========= Stream Water Temperature Control ====================
       ! iwtmp_rch ::   0=default stream water temperature model; 1=new stream water temperature model
       ! wtmp_option ::  1=Ficklin's stream temp model; 2=Du's stream temp model 
       !! ========= Subbasin Water Temperature Control ===========================
       ! iwtmp_sub ::    0=default ; 1= Ficklin's  subbasin water temperature; 2= Qi's hru/subbasin water temperature WHEN tswat=1
       ! isurface_tmp ::   0= soil surface temperature; 1= equilibrium temperature
       !NOTE: need to adjust parameters in reasbsn.f for Ficklin's method.
