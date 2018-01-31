program modif

USE netcdf

IMPLICIT NONE

INTEGER :: fidA, status, dimID_t, dimID_z, dimID_y, dimID_x, mt, mz, my, mx, &
&          tmask_ID, nav_lev_ID, nav_lat_ID, nav_lon_ID, votemper_ID, fidM,  &
&          i, j, k, l, gdept_0_ID, vosaline_ID, kyear


CHARACTER(LEN=5) :: charexp

CHARACTER(LEN=150) :: file_in, file_out_T_EXP, file_out_S_EXP

REAL*4,ALLOCATABLE,DIMENSION(:) :: nav_lev           

REAL*8,ALLOCATABLE,DIMENSION(:) :: time_counter          

REAL*4,ALLOCATABLE,DIMENSION(:,:) :: nav_lat, nav_lon         

!INTEGER*1,ALLOCATABLE,DIMENSION(:,:,:,:) :: tmask   

REAL*4,ALLOCATABLE,DIMENSION(:,:,:,:) :: gdept_0, vosaline_EXP, votemper_EXP

REAL*4 :: Tsrf, Tsrf0, Ssrf, Ssrf0, Tbot, Tbot0, Sbot, Sbot0, z1_0, z1, z2_0, z2, dTsrf, dTbot, dz1, dz2

!---------------------------------------

charexp='EXP23'  ! name of ISOMIP experience : EXP0, EXP1, EXP2, etc...

file_in='mesh_mask_MISOMIP_EXP1x.nc'

102 FORMAT('/scratch/shared/egige60/input/nemo_ISOMIP/DTA_',a,'/dta_temp_y',i4.4,'_ISOMIP_',a,'.nc')
103 FORMAT('/scratch/shared/egige60/input/nemo_ISOMIP/DTA_',a,'/dta_sal_y',i4.4,'_ISOMIP_',a,'.nc')

!---------------------------------------                   
! Read netcdf input file :                                 

status = NF90_OPEN(TRIM(file_in),0,fidA) 
call erreur(status,.TRUE.,"read") 
     
status = NF90_INQ_DIMID(fidA,"t",dimID_t)
call erreur(status,.TRUE.,"inq_dimID_t")
status = NF90_INQ_DIMID(fidA,"z",dimID_z)
call erreur(status,.TRUE.,"inq_dimID_z")
status = NF90_INQ_DIMID(fidA,"y",dimID_y)
call erreur(status,.TRUE.,"inq_dimID_y")
status = NF90_INQ_DIMID(fidA,"x",dimID_x)
call erreur(status,.TRUE.,"inq_dimID_x")

status = NF90_INQUIRE_DIMENSION(fidA,dimID_t,len=mt)
call erreur(status,.TRUE.,"inq_dim_t")
status = NF90_INQUIRE_DIMENSION(fidA,dimID_z,len=mz)
call erreur(status,.TRUE.,"inq_dim_z")
status = NF90_INQUIRE_DIMENSION(fidA,dimID_y,len=my)
call erreur(status,.TRUE.,"inq_dim_y")
status = NF90_INQUIRE_DIMENSION(fidA,dimID_x,len=mx)
call erreur(status,.TRUE.,"inq_dim_x")
    
!ALLOCATE(  tmask(mx,my,mz,mt)  ) 
ALLOCATE(  gdept_0(mx,my,mz,mt)  )
ALLOCATE(  nav_lat(mx,my)  ) 
ALLOCATE(  nav_lon(mx,my)  ) 
ALLOCATE(  nav_lev(mz)  )
      
!status = NF90_INQ_VARID(fidA,"tmask",tmask_ID)
!call erreur(status,.TRUE.,"inq_tmask_ID")
status = NF90_INQ_VARID(fidA,"gdept_0",gdept_0_ID)
call erreur(status,.TRUE.,"inq_gdept_0_ID")
status = NF90_INQ_VARID(fidA,"nav_lev",nav_lev_ID)
call erreur(status,.TRUE.,"inq_nav_lev_ID")
status = NF90_INQ_VARID(fidA,"nav_lat",nav_lat_ID)
call erreur(status,.TRUE.,"inq_nav_lat_ID")
status = NF90_INQ_VARID(fidA,"nav_lon",nav_lon_ID)
call erreur(status,.TRUE.,"inq_nav_lon_ID")
        
!status = NF90_GET_VAR(fidA,tmask_ID,tmask)
!call erreur(status,.TRUE.,"getvar_tmask")
status = NF90_GET_VAR(fidA,gdept_0_ID,gdept_0)
call erreur(status,.TRUE.,"getvar_gdept_0")
status = NF90_GET_VAR(fidA,nav_lev_ID,nav_lev)
call erreur(status,.TRUE.,"getvar_nav_lev")
status = NF90_GET_VAR(fidA,nav_lat_ID,nav_lat)
call erreur(status,.TRUE.,"getvar_nav_lat")
status = NF90_GET_VAR(fidA,nav_lon_ID,nav_lon)
call erreur(status,.TRUE.,"getvar_nav_lon")

status = NF90_CLOSE(fidA)    
call erreur(status,.TRUE.,"fin_lecture")     

!---------------------------------------
! Creation of T,S data :

write(*,*) 'Experience ', TRIM(charexp)

do kyear=0,100

  write(file_out_T_EXP,102) TRIM(charexp), kyear, charexp
  write(file_out_S_EXP,103) TRIM(charexp), kyear, charexp

  ALLOCATE( vosaline_EXP(mx,my,mz,1) )
  ALLOCATE( votemper_EXP(mx,my,mz,1) )
  
  if ( charexp == 'EXP10' ) then
  
    !-- Uniform warming :
    z1_0  = 300.0 ! initial depth of upper thermocline
    z2_0  = 700.0 ! initial depth of lower thermocline
    Tsrf0 =  -1.0 ! initial surface temperature
    Tbot0 =   1.2 ! initial bottom temperature
    Ssrf0 =  34.0 ! initial surface salinity 
    Sbot0 =  34.5 ! initial bottom salinity (an increase of 0.2psu/700m is added below)
    dz1   =   0.0 ! uplift of the upper thermocline over 100years (from year 0 to year 100)
    dz2   =   0.0 ! uplift of the lower thermocline over 100years (from year 0 to year 100)
    dTsrf =   1.0 ! warming over 100years (from year 0 to year 100)
    dTbot =   1.0 !    "            "
  
  elseif ( charexp == 'EXP11' ) then
  
    !-- Deep warming :
    z1_0  = 300.0 ! initial depth of upper thermocline
    z2_0  = 700.0 ! initial depth of lower thermocline
    Tsrf0 =  -1.0 ! initial surface temperature
    Tbot0 =   1.2 ! initial bottom temperature
    Ssrf0 =  34.0 ! initial surface salinity 
    Sbot0 =  34.5 ! initial bottom salinity (an increase of 0.2psu/700m is added below)
    dz1   =   0.0 ! uplift of the upper thermocline over 100years (from year 0 to year 100)
    dz2   =   0.0 ! uplift of the lower thermocline over 100years (from year 0 to year 100)
    dTsrf =   0.0 ! warming over 100years (from year 0 to year 100)
    dTbot =   1.0 !    "            "
  
  elseif ( charexp == 'EXP12' ) then
  
    !-- From cold to warm cavity :
    z1_0  = 300.0 ! initial depth of upper thermocline
    z2_0  = 700.0 ! initial depth of lower thermocline
    Tsrf0 =  -1.5 ! initial surface temperature
    Tbot0 =  -1.5 ! initial bottom temperature
    Ssrf0 =  34.0 ! initial surface salinity 
    Sbot0 =  34.0 ! initial bottom salinity (an increase of 0.2psu/700m is added below)
    dz1   =   0.0 ! uplift of the upper thermocline over 100years (from year 0 to year 100)
    dz2   =   0.0 ! uplift of the lower thermocline over 100years (from year 0 to year 100)
    dTsrf =   0.9 ! warming over 100years (from year 0 to year 100)
    dTbot =   2.7 !    "            "
  
  elseif ( charexp == 'EXP13' ) then
  
    !-- From cold to warm cavity :
    z1_0  = 300.0 ! initial depth of upper thermocline
    z2_0  = 700.0 ! initial depth of lower thermocline
    Tsrf0 =  -1.0 ! initial surface temperature
    Tbot0 =   1.2 ! initial bottom temperature
    Ssrf0 =  34.0 ! initial surface salinity 
    Sbot0 =  34.7 ! initial bottom salinity (an increase of 0.2psu/700m is added below)
    dz1   =-200.0 ! uplift of the upper thermocline over 100years (from year 0 to year 100)
    dz2   =-200.0 ! uplift of the lower thermocline over 100years (from year 0 to year 100)
    dTsrf =   0.0 ! warming over 100years (from year 0 to year 100)
    dTbot =   0.0 !    "            "

  elseif ( charexp == 'EXP20' ) then
  
    !-- Same as EXP10 but constant
    z1_0  = 300.0 ! initial depth of upper thermocline
    z2_0  = 700.0 ! initial depth of lower thermocline
    Tsrf0 =  -1.0 ! initial surface temperature
    Tbot0 =   1.2 ! initial bottom temperature
    Ssrf0 =  34.0 ! initial surface salinity 
    Sbot0 =  34.5 ! initial bottom salinity (an increase of 0.2psu/700m is added below)
    dz1   =   0.0 ! uplift of the upper thermocline over 100years (from year 0 to year 100)
    dz2   =   0.0 ! uplift of the lower thermocline over 100years (from year 0 to year 100)
    dTsrf =   0.0 ! warming over 100years (from year 0 to year 100)
    dTbot =   0.0 !    "            "
  
  elseif ( charexp == 'EXP21' ) then
  
    !-- Same as EXP11 but constant
    z1_0  = 300.0 ! initial depth of upper thermocline
    z2_0  = 700.0 ! initial depth of lower thermocline
    Tsrf0 =  -1.0 ! initial surface temperature
    Tbot0 =   1.2 ! initial bottom temperature
    Ssrf0 =  34.0 ! initial surface salinity 
    Sbot0 =  34.5 ! initial bottom salinity (an increase of 0.2psu/700m is added below)
    dz1   =   0.0 ! uplift of the upper thermocline over 100years (from year 0 to year 100)
    dz2   =   0.0 ! uplift of the lower thermocline over 100years (from year 0 to year 100)
    dTsrf =   0.0 ! warming over 100years (from year 0 to year 100)
    dTbot =   0.0 !    "            "
  
  elseif ( charexp == 'EXP22' ) then
  
    !-- Same as EXP12 but constant
    z1_0  = 300.0 ! initial depth of upper thermocline
    z2_0  = 700.0 ! initial depth of lower thermocline
    Tsrf0 =  -1.5 ! initial surface temperature
    Tbot0 =  -1.5 ! initial bottom temperature
    Ssrf0 =  34.0 ! initial surface salinity 
    Sbot0 =  34.0 ! initial bottom salinity (an increase of 0.2psu/700m is added below)
    dz1   =   0.0 ! uplift of the upper thermocline over 100years (from year 0 to year 100)
    dz2   =   0.0 ! uplift of the lower thermocline over 100years (from year 0 to year 100)
    dTsrf =   0.0 ! warming over 100years (from year 0 to year 100)
    dTbot =   0.0 !    "            "
  
  elseif ( charexp == 'EXP23' ) then
  
    !-- Same as EXP13 but constant
    z1_0  = 300.0 ! initial depth of upper thermocline
    z2_0  = 700.0 ! initial depth of lower thermocline
    Tsrf0 =  -1.0 ! initial surface temperature
    Tbot0 =   1.2 ! initial bottom temperature
    Ssrf0 =  34.0 ! initial surface salinity 
    Sbot0 =  34.7 ! initial bottom salinity (an increase of 0.2psu/700m is added below)
    dz1   =   0.0 ! uplift of the upper thermocline over 100years (from year 0 to year 100)
    dz2   =   0.0 ! uplift of the lower thermocline over 100years (from year 0 to year 100)
    dTsrf =   0.0 ! warming over 100years (from year 0 to year 100)
    dTbot =   0.0 !    "            "

  else
 
    write(*,*) '~!@#$%^* check EXP name >>>>>>>>>>> stop !!!!!!!!!!'
    stop
 
  endif  

  z1 = z1_0 + kyear * dz1 / 100.0
  z2 = z2_0 + kyear * dz2 / 100.0
  Tsrf = Tsrf0 + kyear * dTsrf / 100.0
  Tbot = Tbot0 + kyear * dTbot / 100.0
  Ssrf = Ssrf0  ! no trend on salinity except if z1 and z2 vary
  if ( charexp == 'EXP12' ) then
    Sbot = Sbot0 + kyear * 0.5 / 100.0 ! trend only in this case
  else
    Sbot = Sbot0  ! no trend on salinity except if z1 and z2 vary
  endif
  do i=1,mx
  do j=1,my
  do k=1,mz
    if ( gdept_0(i,j,k,1) .le. z1 ) then
      votemper_EXP(i,j,k,1) = Tsrf
      vosaline_EXP(i,j,k,1) = Ssrf + 0.2 * gdept_0(i,j,k,1) / 700.0
    elseif ( gdept_0(i,j,k,1) .ge. z2 ) then
      votemper_EXP(i,j,k,1) = Tbot
      vosaline_EXP(i,j,k,1) = Sbot + 0.2 * gdept_0(i,j,k,1) / 700.0
    else
      votemper_EXP(i,j,k,1) = (Tbot-Tsrf)*gdept_0(i,j,k,1)/(z2-z1) + Tsrf - (Tbot-Tsrf)*z1/(z2-z1)
      vosaline_EXP(i,j,k,1) = (Sbot-Ssrf)*gdept_0(i,j,k,1)/(z2-z1) + Ssrf - (Sbot-Ssrf)*z1/(z2-z1)  + 0.2 * gdept_0(i,j,k,1) / 700.0
    endif
  enddo
  enddo
  enddo

  !---------------------------------------                      
  ! Writing file_out_T_EXP

  write(*,*) 'Creating ', TRIM(file_out_T_EXP)
                                                              
  status = NF90_CREATE(TRIM(file_out_T_EXP),NF90_NOCLOBBER,fidM)
  call erreur(status,.TRUE.,'create file_out_T_EXP')          
                                                                
         status = NF90_DEF_DIM(fidM,"time",NF90_UNLIMITED,dimID_t)
         call erreur(status,.TRUE.,"def_dimID_t")
         status = NF90_DEF_DIM(fidM,"z",mz,dimID_z)
         call erreur(status,.TRUE.,"def_dimID_z")
         status = NF90_DEF_DIM(fidM,"y",my,dimID_y)
         call erreur(status,.TRUE.,"def_dimID_y")
         status = NF90_DEF_DIM(fidM,"x",mx,dimID_x)
         call erreur(status,.TRUE.,"def_dimID_x")
                                                              
         status = NF90_DEF_VAR(fidM,"votemper",NF90_FLOAT,(/dimID_x,dimID_y,dimID_z,dimID_t/),votemper_ID)
         call erreur(status,.TRUE.,"def_var_votemper_ID")
         status = NF90_DEF_VAR(fidM,"nav_lev",NF90_FLOAT,(/dimID_z/),nav_lev_ID)
         call erreur(status,.TRUE.,"def_var_nav_lev_ID")
         status = NF90_DEF_VAR(fidM,"nav_lat",NF90_FLOAT,(/dimID_x,dimID_y/),nav_lat_ID)
         call erreur(status,.TRUE.,"def_var_nav_lat_ID")
         status = NF90_DEF_VAR(fidM,"nav_lon",NF90_FLOAT,(/dimID_x,dimID_y/),nav_lon_ID)
         call erreur(status,.TRUE.,"def_var_nav_lon_ID")
                                   
         status = NF90_PUT_ATT(fidM,NF90_GLOBAL,"history","Created using build_dta_files_ISOMIP_EXP10-13_CPL.f90")
         call erreur(status,.TRUE.,"put_att_GLOBAL_ID")
                                                      
         status = NF90_ENDDEF(fidM)                   
         call erreur(status,.TRUE.,"fin_definition") 
                                                      
         status = NF90_PUT_VAR(fidM,votemper_ID,votemper_EXP)
         call erreur(status,.TRUE.,"var_votemper_ID")
         status = NF90_PUT_VAR(fidM,nav_lev_ID,nav_lev)
         call erreur(status,.TRUE.,"var_nav_lev_ID")
         status = NF90_PUT_VAR(fidM,nav_lat_ID,nav_lat)
         call erreur(status,.TRUE.,"var_nav_lat_ID")
         status = NF90_PUT_VAR(fidM,nav_lon_ID,nav_lon)
         call erreur(status,.TRUE.,"var_nav_lon_ID")
                                                      
  status = NF90_CLOSE(fidM)                    
  call erreur(status,.TRUE.,"final")         

  !---------------------------------------                      
  ! Writing file_out_S_EXP
                                              
  write(*,*) 'Creating ', TRIM(file_out_S_EXP)
                
  status = NF90_CREATE(TRIM(file_out_S_EXP),NF90_NOCLOBBER,fidM)
  call erreur(status,.TRUE.,'create file_out_S_EXP')
                                                                
         status = NF90_DEF_DIM(fidM,"time",NF90_UNLIMITED,dimID_t)
         call erreur(status,.TRUE.,"def_dimID_t")
         status = NF90_DEF_DIM(fidM,"z",mz,dimID_z)
         call erreur(status,.TRUE.,"def_dimID_z")
         status = NF90_DEF_DIM(fidM,"y",my,dimID_y)
         call erreur(status,.TRUE.,"def_dimID_y")
         status = NF90_DEF_DIM(fidM,"x",mx,dimID_x)
         call erreur(status,.TRUE.,"def_dimID_x")
                                                              
         status = NF90_DEF_VAR(fidM,"vosaline",NF90_FLOAT,(/dimID_x,dimID_y,dimID_z,dimID_t/),vosaline_ID)
         call erreur(status,.TRUE.,"def_var_vosaline_ID")
         status = NF90_DEF_VAR(fidM,"nav_lev",NF90_FLOAT,(/dimID_z/),nav_lev_ID)
         call erreur(status,.TRUE.,"def_var_nav_lev_ID")
         status = NF90_DEF_VAR(fidM,"nav_lat",NF90_FLOAT,(/dimID_x,dimID_y/),nav_lat_ID)
         call erreur(status,.TRUE.,"def_var_nav_lat_ID")
         status = NF90_DEF_VAR(fidM,"nav_lon",NF90_FLOAT,(/dimID_x,dimID_y/),nav_lon_ID)
         call erreur(status,.TRUE.,"def_var_nav_lon_ID")
                                   
         status = NF90_PUT_ATT(fidM,NF90_GLOBAL,"history","build_dta_files_ISOMIP_EXP10-13_CPL.f90")
         call erreur(status,.TRUE.,"put_att_GLOBAL_ID")
                                                      
         status = NF90_ENDDEF(fidM)                   
         call erreur(status,.TRUE.,"fin_definition") 
                                                      
         status = NF90_PUT_VAR(fidM,vosaline_ID,vosaline_EXP)
         call erreur(status,.TRUE.,"var_vosaline_ID")
         status = NF90_PUT_VAR(fidM,nav_lev_ID,nav_lev)
         call erreur(status,.TRUE.,"var_nav_lev_ID")
         status = NF90_PUT_VAR(fidM,nav_lat_ID,nav_lat)
         call erreur(status,.TRUE.,"var_nav_lat_ID")
         status = NF90_PUT_VAR(fidM,nav_lon_ID,nav_lon)
         call erreur(status,.TRUE.,"var_nav_lon_ID")
                                                      
  status = NF90_CLOSE(fidM)                    
  call erreur(status,.TRUE.,"final")         

  DEALLOCATE( votemper_EXP, vosaline_EXP )

enddo ! kyear

end program modif



SUBROUTINE erreur(iret, lstop, chaine)
  ! pour les messages d'erreur
  USE netcdf
  INTEGER, INTENT(in)                     :: iret
  LOGICAL, INTENT(in)                     :: lstop
  CHARACTER(LEN=*), INTENT(in)            :: chaine
  !
  CHARACTER(LEN=80)                       :: message
  !
  IF ( iret .NE. 0 ) THEN
    WRITE(*,*) 'ROUTINE: ', TRIM(chaine)
    WRITE(*,*) 'ERREUR: ', iret
    message=NF90_STRERROR(iret)
    WRITE(*,*) 'CA VEUT DIRE:',TRIM(message)
    IF ( lstop ) STOP
  ENDIF
  !
END SUBROUTINE erreur
