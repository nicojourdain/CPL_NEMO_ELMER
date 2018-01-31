program modif                                         
                                                      
USE netcdf                                            
                                                      
IMPLICIT NONE                                         
                                                      
INTEGER :: fidA, status, dimID_t, dimID_z, dimID_y, dimID_x, mt, mz, my, mx, &
&          tmask_ID, nav_lev_ID, nav_lat_ID, nav_lon_ID, resto_ID, fidM, i, j, k
                                                      
CHARACTER(LEN=100) :: file_in, file_out                     
                                                      
REAL*4,ALLOCATABLE,DIMENSION(:) :: nav_lev           
                                                      
REAL*8,ALLOCATABLE,DIMENSION(:) :: time_counter          
                                                      
REAL*4,ALLOCATABLE,DIMENSION(:,:) :: nav_lat, nav_lon         
                                                      
INTEGER*1,ALLOCATABLE,DIMENSION(:,:,:,:) :: tmask   
                                                      
REAL*4,ALLOCATABLE,DIMENSION(:,:,:) :: resto

REAL*4 :: gamma0
                                                      
!---------------------------------------
                                                           
file_in  = 'mesh_mask_ISOMIP_EXP1.nc'                                
file_out = 'resto.nc'                               
                                                           
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
         ALLOCATE(  nav_lat(mx,my)  ) 
         ALLOCATE(  nav_lon(mx,my)  ) 
         ALLOCATE(  nav_lev(mz)  )         
                        
         status = NF90_INQ_VARID(fidA,"tmask",tmask_ID)
         call erreur(status,.TRUE.,"inq_tmask_ID")
         status = NF90_INQ_VARID(fidA,"nav_lev",nav_lev_ID)
         call erreur(status,.TRUE.,"inq_nav_lev_ID")
         status = NF90_INQ_VARID(fidA,"nav_lat",nav_lat_ID)
         call erreur(status,.TRUE.,"inq_nav_lat_ID")
         status = NF90_INQ_VARID(fidA,"nav_lon",nav_lon_ID)
         call erreur(status,.TRUE.,"inq_nav_lon_ID")
                                                              
         status = NF90_GET_VAR(fidA,tmask_ID,tmask)
         call erreur(status,.TRUE.,"getvar_tmask")
         status = NF90_GET_VAR(fidA,nav_lev_ID,nav_lev)
         call erreur(status,.TRUE.,"getvar_nav_lev")
         status = NF90_GET_VAR(fidA,nav_lat_ID,nav_lat)
         call erreur(status,.TRUE.,"getvar_nav_lat")
         status = NF90_GET_VAR(fidA,nav_lon_ID,nav_lon)
         call erreur(status,.TRUE.,"getvar_nav_lon")
                                                      
       status = NF90_CLOSE(fidA)                      
       call erreur(status,.TRUE.,"fin_lecture")     
                                                              
!---------------------------------------                      
! Creation of restoring mask :

!gamma0 = 1.0 / ( 10.0 * 86400.0 )  ! = 10 days^-1
gamma0 = 10.0 / ( 86400.0 )

ALLOCATE( resto(mx,my,mz) )

do i=1,mx
do j=1,my
do k=1,mz
  resto(i,j,k) = gamma0 * max( 0.0, ( nav_lon(i,j) - 790.0 ) / ( 800 - 790.0 ) ) * tmask(i,j,k,1)
enddo
enddo
enddo
                                                              
!---------------------------------------                      
! Writing new netcdf file :                                   
                                                              
        status = NF90_CREATE(TRIM(file_out),NF90_NOCLOBBER,fidM)
        call erreur(status,.TRUE.,'create')                     
                                                                
         status = NF90_DEF_DIM(fidM,"z",mz,dimID_z)
         call erreur(status,.TRUE.,"def_dimID_z")
         status = NF90_DEF_DIM(fidM,"y",my,dimID_y)
         call erreur(status,.TRUE.,"def_dimID_y")
         status = NF90_DEF_DIM(fidM,"x",mx,dimID_x)
         call erreur(status,.TRUE.,"def_dimID_x")
                                                              
         status = NF90_DEF_VAR(fidM,"resto",NF90_FLOAT,(/dimID_x,dimID_y,dimID_z/),resto_ID)
         call erreur(status,.TRUE.,"def_var_resto_ID")
         status = NF90_DEF_VAR(fidM,"nav_lev",NF90_FLOAT,(/dimID_z/),nav_lev_ID)
         call erreur(status,.TRUE.,"def_var_nav_lev_ID")
         status = NF90_DEF_VAR(fidM,"nav_lat",NF90_FLOAT,(/dimID_x,dimID_y/),nav_lat_ID)
         call erreur(status,.TRUE.,"def_var_nav_lat_ID")
         status = NF90_DEF_VAR(fidM,"nav_lon",NF90_FLOAT,(/dimID_x,dimID_y/),nav_lon_ID)
         call erreur(status,.TRUE.,"def_var_nav_lon_ID")
                                   
         status = NF90_PUT_ATT(fidM,NF90_GLOBAL,"history","Created using build_resto_coeff.f90")
         call erreur(status,.TRUE.,"put_att_GLOBAL_ID")
                                                      
         status = NF90_ENDDEF(fidM)                   
         call erreur(status,.TRUE.,"fin_definition") 
                                                      
         status = NF90_PUT_VAR(fidM,resto_ID,resto)
         call erreur(status,.TRUE.,"var_resto_ID")
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
