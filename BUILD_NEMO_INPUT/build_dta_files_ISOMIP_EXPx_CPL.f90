program modif                                         
                                                      
USE netcdf                                            
                                                      
IMPLICIT NONE                                         
                                                      
INTEGER :: fidA, status, dimID_t, dimID_z, dimID_y, dimID_x, mt, mz, my, mx, &
&          tmask_ID, nav_lev_ID, nav_lat_ID, nav_lon_ID, votemper_ID, fidM,  &
&          i, j, k, l, gdept_0_ID, vosaline_ID


CHARACTER(LEN=4) :: charexp
                                                      
CHARACTER(LEN=100) :: file_in, file_out_T_EXP_y1, file_out_S_EXP_y1, &
&                              file_out_T_EXP_y2, file_out_S_EXP_y2
                                                      
REAL*4,ALLOCATABLE,DIMENSION(:) :: nav_lev           
                                                      
REAL*8,ALLOCATABLE,DIMENSION(:) :: time_counter          
                                                      
REAL*4,ALLOCATABLE,DIMENSION(:,:) :: nav_lat, nav_lon         
                                                      
INTEGER*1,ALLOCATABLE,DIMENSION(:,:,:,:) :: tmask   
                                                      
REAL*4,ALLOCATABLE,DIMENSION(:,:,:,:) :: gdept_0, vosaline_EXP_y1, votemper_EXP_y1, &
&                                                 vosaline_EXP_y2, votemper_EXP_y2

REAL*4 :: Tbot_cold, Sbot_cold, T0_cold, S0_cold, Tbot_warm, Sbot_warm, T0_warm, S0_warm
                                                      
!---------------------------------------
                          
charexp='EXP4'  ! name of ISOMIP experience : EXP0, EXP1, EXP2, etc...
                                 
write(file_in,101) charexp
101 FORMAT('mesh_mask_ISOMIP_',a,'.nc')
write(file_out_T_EXP_y1,102) charexp
102 FORMAT('dta_temp_y0000_ISOMIP_',a,'.nc')
write(file_out_S_EXP_y1,103) charexp
103 FORMAT('dta_sal_y0000_ISOMIP_',a,'.nc')
write(file_out_T_EXP_y2,104) charexp
104 FORMAT('dta_temp_y0001_ISOMIP_',a,'.nc')
write(file_out_S_EXP_y2,105) charexp
105 FORMAT('dta_sal_y0001_ISOMIP_',a,'.nc')

Tbot_cold = -1.9 
!Sbot_cold = 34.5
Sbot_cold = 34.55
T0_cold   = -1.9
S0_cold   = 33.8

Tbot_warm =  1.0
Sbot_warm = 34.7
T0_warm   = -1.9
S0_warm   = 33.8
                                                           
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
                               
         ALLOCATE(  tmask(mx,my,mz,mt)  ) 
         ALLOCATE(  gdept_0(mx,my,mz,mt)  )
         ALLOCATE(  nav_lat(mx,my)  ) 
         ALLOCATE(  nav_lon(mx,my)  ) 
         ALLOCATE(  nav_lev(mz)  )         
                        
         status = NF90_INQ_VARID(fidA,"tmask",tmask_ID)
         call erreur(status,.TRUE.,"inq_tmask_ID")
         status = NF90_INQ_VARID(fidA,"gdept_0",gdept_0_ID)
         call erreur(status,.TRUE.,"inq_gdept_0_ID")
         status = NF90_INQ_VARID(fidA,"nav_lev",nav_lev_ID)
         call erreur(status,.TRUE.,"inq_nav_lev_ID")
         status = NF90_INQ_VARID(fidA,"nav_lat",nav_lat_ID)
         call erreur(status,.TRUE.,"inq_nav_lat_ID")
         status = NF90_INQ_VARID(fidA,"nav_lon",nav_lon_ID)
         call erreur(status,.TRUE.,"inq_nav_lon_ID")
                                                              
         status = NF90_GET_VAR(fidA,tmask_ID,tmask)
         call erreur(status,.TRUE.,"getvar_tmask")
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

ALLOCATE( vosaline_EXP_y1(mx,my,mz,365) )
ALLOCATE( votemper_EXP_y1(mx,my,mz,365) )
ALLOCATE( vosaline_EXP_y2(mx,my,mz,365) )
ALLOCATE( votemper_EXP_y2(mx,my,mz,365) )

if ( charexp == 'EXP1' ) then

  !-- cold to warm
  do i=1,mx
  do j=1,my
  do k=1,mz

    vosaline_EXP_y1(i,j,k,1) = ( S0_cold + ( Sbot_cold - S0_cold ) * gdept_0(i,j,k,1) / 720.0 ) !* tmask(i,j,k,1)
    votemper_EXP_y1(i,j,k,1) = ( T0_cold + ( Tbot_cold - T0_cold ) * gdept_0(i,j,k,1) / 720.0 ) !* tmask(i,j,k,1)

    do l=2,365
      vosaline_EXP_y1(i,j,k,l) = ( S0_warm + ( Sbot_warm - S0_warm ) * gdept_0(i,j,k,1) / 720.0 ) !* tmask(i,j,k,1)
      votemper_EXP_y1(i,j,k,l) = ( T0_warm + ( Tbot_warm - T0_warm ) * gdept_0(i,j,k,1) / 720.0 ) !* tmask(i,j,k,1)
    enddo

    do l=1,365
      vosaline_EXP_y2(i,j,k,l) = ( S0_warm + ( Sbot_warm - S0_warm ) * gdept_0(i,j,k,1) / 720.0 ) !* tmask(i,j,k,1)
      votemper_EXP_y2(i,j,k,l) = ( T0_warm + ( Tbot_warm - T0_warm ) * gdept_0(i,j,k,1) / 720.0 ) !* tmask(i,j,k,1)
    enddo

  enddo
  enddo
  enddo

elseif ( charexp == 'EXP2' ) then 

  !-- warm to cold
  do i=1,mx
  do j=1,my
  do k=1,mz

    vosaline_EXP_y1(i,j,k,1) = ( S0_warm + ( Sbot_warm - S0_warm ) * gdept_0(i,j,k,1) / 720.0 ) !* tmask(i,j,k,1)
    votemper_EXP_y1(i,j,k,1) = ( T0_warm + ( Tbot_warm - T0_warm ) * gdept_0(i,j,k,1) / 720.0 ) !* tmask(i,j,k,1)

    do l=2,365
      vosaline_EXP_y1(i,j,k,l) = ( S0_cold + ( Sbot_cold - S0_cold ) * gdept_0(i,j,k,1) / 720.0 ) !* tmask(i,j,k,1)
      votemper_EXP_y1(i,j,k,l) = ( T0_cold + ( Tbot_cold - T0_cold ) * gdept_0(i,j,k,1) / 720.0 ) !* tmask(i,j,k,1)
    enddo

    do l=1,365
      vosaline_EXP_y2(i,j,k,l) = ( S0_cold + ( Sbot_cold - S0_cold ) * gdept_0(i,j,k,1) / 720.0 ) !* tmask(i,j,k,1)
      votemper_EXP_y2(i,j,k,l) = ( T0_cold + ( Tbot_cold - T0_cold ) * gdept_0(i,j,k,1) / 720.0 ) !* tmask(i,j,k,1)
    enddo

  enddo
  enddo
  enddo

elseif ( charexp == 'EXP3' ) then

  !-- always warm
  do i=1,mx
  do j=1,my
  do k=1,mz

    do l=1,365
      vosaline_EXP_y1(i,j,k,l) = ( S0_warm + ( Sbot_warm - S0_warm ) * gdept_0(i,j,k,1) / 720.0 ) !* tmask(i,j,k,1)
      votemper_EXP_y1(i,j,k,l) = ( T0_warm + ( Tbot_warm - T0_warm ) * gdept_0(i,j,k,1) / 720.0 ) !* tmask(i,j,k,1)
    enddo

    do l=1,365
      vosaline_EXP_y2(i,j,k,l) = ( S0_warm + ( Sbot_warm - S0_warm ) * gdept_0(i,j,k,1) / 720.0 ) !* tmask(i,j,k,1)
      votemper_EXP_y2(i,j,k,l) = ( T0_warm + ( Tbot_warm - T0_warm ) * gdept_0(i,j,k,1) / 720.0 ) !* tmask(i,j,k,1)
    enddo

  enddo
  enddo
  enddo

elseif ( charexp == 'EXP4' ) then

  !-- always cold
  do i=1,mx
  do j=1,my
  do k=1,mz

    do l=1,365
      vosaline_EXP_y1(i,j,k,l) = ( S0_cold + ( Sbot_cold - S0_cold ) * gdept_0(i,j,k,1) / 720.0 ) !* tmask(i,j,k,1)
      votemper_EXP_y1(i,j,k,l) = ( T0_cold + ( Tbot_cold - T0_cold ) * gdept_0(i,j,k,1) / 720.0 ) !* tmask(i,j,k,1)
    enddo

    do l=1,365
      vosaline_EXP_y2(i,j,k,l) = ( S0_cold + ( Sbot_cold - S0_cold ) * gdept_0(i,j,k,1) / 720.0 ) !* tmask(i,j,k,1)
      votemper_EXP_y2(i,j,k,l) = ( T0_cold + ( Tbot_cold - T0_cold ) * gdept_0(i,j,k,1) / 720.0 ) !* tmask(i,j,k,1)
    enddo

  enddo
  enddo
  enddo

else

  write(*,*) '~!@#$%^* check EXP name >>>>>>>>>>> stop !!!!!!!!!!'
  stop 

endif
        
!---------------------------------------                      
! Writing file_out_T_EXP_y1
                                                              
        status = NF90_CREATE(TRIM(file_out_T_EXP_y1),NF90_NOCLOBBER,fidM)
        call erreur(status,.TRUE.,'create file_out_T_EXP_y1')          
                                                                
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
                                   
         status = NF90_PUT_ATT(fidM,NF90_GLOBAL,"history","Created using build_dta_files_ISOMIP_EXPx.f90")
         call erreur(status,.TRUE.,"put_att_GLOBAL_ID")
                                                      
         status = NF90_ENDDEF(fidM)                   
         call erreur(status,.TRUE.,"fin_definition") 
                                                      
         status = NF90_PUT_VAR(fidM,votemper_ID,votemper_EXP_y1)
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
! Writing file_out_S_EXP_y1
                                                              
        status = NF90_CREATE(TRIM(file_out_S_EXP_y1),NF90_NOCLOBBER,fidM)
        call erreur(status,.TRUE.,'create file_out_S_EXP_y1')          
                                                                
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
                                   
         status = NF90_PUT_ATT(fidM,NF90_GLOBAL,"history","Created using build_dta_files_ISOMIP_EXPx.f90")
         call erreur(status,.TRUE.,"put_att_GLOBAL_ID")
                                                      
         status = NF90_ENDDEF(fidM)                   
         call erreur(status,.TRUE.,"fin_definition") 
                                                      
         status = NF90_PUT_VAR(fidM,vosaline_ID,vosaline_EXP_y1)
         call erreur(status,.TRUE.,"var_vosaline_ID")
         status = NF90_PUT_VAR(fidM,nav_lev_ID,nav_lev)
         call erreur(status,.TRUE.,"var_nav_lev_ID")
         status = NF90_PUT_VAR(fidM,nav_lat_ID,nav_lat)
         call erreur(status,.TRUE.,"var_nav_lat_ID")
         status = NF90_PUT_VAR(fidM,nav_lon_ID,nav_lon)
         call erreur(status,.TRUE.,"var_nav_lon_ID")
                                                      
         status = NF90_CLOSE(fidM)                    
         call erreur(status,.TRUE.,"final")         

!---------------------------------------                      
! Writing file_out_T_EXP_y2
                                                              
        status = NF90_CREATE(TRIM(file_out_T_EXP_y2),NF90_NOCLOBBER,fidM)
        call erreur(status,.TRUE.,'create file_out_T_EXP_y2')          
                                                                
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
                                   
         status = NF90_PUT_ATT(fidM,NF90_GLOBAL,"history","Created using build_dta_files_ISOMIP_EXPx.f90")
         call erreur(status,.TRUE.,"put_att_GLOBAL_ID")
                                                      
         status = NF90_ENDDEF(fidM)                   
         call erreur(status,.TRUE.,"fin_definition") 
                                                      
         status = NF90_PUT_VAR(fidM,votemper_ID,votemper_EXP_y2)
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
! Writing file_out_S_EXP_y2
                                                              
        status = NF90_CREATE(TRIM(file_out_S_EXP_y2),NF90_NOCLOBBER,fidM)
        call erreur(status,.TRUE.,'create file_out_S_EXP_y2')          
                                                                
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
                                   
         status = NF90_PUT_ATT(fidM,NF90_GLOBAL,"history","Created using build_dta_files_ISOMIP_EXPx.f90")
         call erreur(status,.TRUE.,"put_att_GLOBAL_ID")
                                                      
         status = NF90_ENDDEF(fidM)                   
         call erreur(status,.TRUE.,"fin_definition") 
                                                      
         status = NF90_PUT_VAR(fidM,vosaline_ID,vosaline_EXP_y2)
         call erreur(status,.TRUE.,"var_vosaline_ID")
         status = NF90_PUT_VAR(fidM,nav_lev_ID,nav_lev)
         call erreur(status,.TRUE.,"var_nav_lev_ID")
         status = NF90_PUT_VAR(fidM,nav_lat_ID,nav_lat)
         call erreur(status,.TRUE.,"var_nav_lat_ID")
         status = NF90_PUT_VAR(fidM,nav_lon_ID,nav_lon)
         call erreur(status,.TRUE.,"var_nav_lon_ID")
                                                      
         status = NF90_CLOSE(fidM)                    
         call erreur(status,.TRUE.,"final")         

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
