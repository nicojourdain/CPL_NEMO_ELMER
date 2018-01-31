program modif                                         

USE netcdf                                            

IMPLICIT NONE                                         

INTEGER :: fidA, status, dimID_y, dimID_x, my, mx, openOceanMask_ID, groundedMask_ID, &
& floatingMask_ID, bedrockTopography_ID, lowerSurface_ID, upperSurface_ID, y_ID, x_ID,&
& fidM, fidN, Bathymetry_ID, isf_draft_ID, ii, jj, i, j, irs, jrs , jl

CHARACTER(LEN=100) :: file_in, file_out_bathy, file_out_isf                     

REAL*8,ALLOCATABLE,DIMENSION(:) :: y, x

REAL*4,ALLOCATABLE,DIMENSION(:) ::  y_ISOMIP, x_ISOMIP

REAL*8,ALLOCATABLE,DIMENSION(:,:) :: openOceanMask, groundedMask, floatingMask,&
& bedrockTopography, lowerSurface, upperSurface

REAL*4,ALLOCATABLE,DIMENSION(:,:) :: Bathymetry_ISOMIP, isf_draft_ISOMIP, isf_thick_ISOMIP, &
& tmp_Bathymetry_ISOMIP, tmp_isf_draft_ISOMIP, maskisf_ISOMIP, maskoce_ISOMIP

REAL*4 :: minthic, Dtmp, Dtmp2

REAL*8 :: eps

file_in        = 'Ocean1_input_geom_v1.01.nc'     !! 1km resolution
file_out_bathy = 'bathy_meter_ISOMIP_EXP1.nc'     !! 2km resolution
file_out_isf   = 'isf_draft_meter_ISOMIP_EXP1.nc' !! 2km resolution

minthic=40.0 ! minimum thickness of the water column

eps=1.e-9

!---------------------------------------                   
! Read original ISOMIP+ bathymetry/draft

status = NF90_OPEN(TRIM(file_in),0,fidA)          
call erreur(status,.TRUE.,"read") 

status = NF90_INQ_DIMID(fidA,"y",dimID_y)
call erreur(status,.TRUE.,"inq_dimID_y")
status = NF90_INQ_DIMID(fidA,"x",dimID_x)
call erreur(status,.TRUE.,"inq_dimID_x")

status = NF90_INQUIRE_DIMENSION(fidA,dimID_y,len=my)
call erreur(status,.TRUE.,"inq_dim_y")
status = NF90_INQUIRE_DIMENSION(fidA,dimID_x,len=mx)
call erreur(status,.TRUE.,"inq_dim_x")

ALLOCATE(  openOceanMask(mx,my)  ) 
ALLOCATE(  groundedMask(mx,my)  ) 
ALLOCATE(  floatingMask(mx,my)  ) 
ALLOCATE(  bedrockTopography(mx,my)  ) 
ALLOCATE(  lowerSurface(mx,my)  ) 
ALLOCATE(  upperSurface(mx,my)  ) 
ALLOCATE(  y(my)  ) 
ALLOCATE(  x(mx)  ) 

status = NF90_INQ_VARID(fidA,"openOceanMask",openOceanMask_ID)
call erreur(status,.TRUE.,"inq_openOceanMask_ID")
status = NF90_INQ_VARID(fidA,"groundedMask",groundedMask_ID)
call erreur(status,.TRUE.,"inq_groundedMask_ID")
status = NF90_INQ_VARID(fidA,"floatingMask",floatingMask_ID)
call erreur(status,.TRUE.,"inq_floatingMask_ID")
status = NF90_INQ_VARID(fidA,"bedrockTopography",bedrockTopography_ID)
call erreur(status,.TRUE.,"inq_bedrockTopography_ID")
status = NF90_INQ_VARID(fidA,"lowerSurface",lowerSurface_ID)
call erreur(status,.TRUE.,"inq_lowerSurface_ID")
status = NF90_INQ_VARID(fidA,"upperSurface",upperSurface_ID)
call erreur(status,.TRUE.,"inq_upperSurface_ID")
status = NF90_INQ_VARID(fidA,"y",y_ID)
call erreur(status,.TRUE.,"inq_y_ID")
status = NF90_INQ_VARID(fidA,"x",x_ID)
call erreur(status,.TRUE.,"inq_x_ID")

status = NF90_GET_VAR(fidA,openOceanMask_ID,openOceanMask)
call erreur(status,.TRUE.,"getvar_openOceanMask")
status = NF90_GET_VAR(fidA,groundedMask_ID,groundedMask)
call erreur(status,.TRUE.,"getvar_groundedMask")
status = NF90_GET_VAR(fidA,floatingMask_ID,floatingMask)
call erreur(status,.TRUE.,"getvar_floatingMask")
status = NF90_GET_VAR(fidA,bedrockTopography_ID,bedrockTopography)
call erreur(status,.TRUE.,"getvar_bedrockTopography")
status = NF90_GET_VAR(fidA,lowerSurface_ID,lowerSurface)
call erreur(status,.TRUE.,"getvar_lowerSurface")
status = NF90_GET_VAR(fidA,upperSurface_ID,upperSurface)
call erreur(status,.TRUE.,"getvar_upperSurface")
status = NF90_GET_VAR(fidA,y_ID,y)
call erreur(status,.TRUE.,"getvar_y")
status = NF90_GET_VAR(fidA,x_ID,x)
call erreur(status,.TRUE.,"getvar_x")

status = NF90_CLOSE(fidA)                      
call erreur(status,.TRUE.,"fin_lecture")     

!------------------------------------------
! Dig bathymetry (50%) and ice draft (50%):
! NB: do it on original bathy to help smooth transition
!     and to keep information on original ice-shelf mask

!! NB : we remove the ice shelf if less than 100m thickness to account for calving
where ( upperSurface(:,:) - lowerSurface(:,:) .lt. 100.0 .and. floatingMask(:,:) .gt. 0.5 )
  floatingMask (:,:) = 0.0
  openOceanMask(:,:) = 1.0
  lowerSurface (:,:) = 0.0
  upperSurface (:,:) = 0.0
endwhere

do i=1,mx
do j=1,my
  Dtmp = - bedrockTopography(i,j) + lowerSurface(i,j)
  if (       floatingMask (i,j) .gt. 0.5              &
     & .and. Dtmp               .gt. 0.0              &
     & .and. Dtmp               .lt. minthic          ) then
      lowerSurface     (i,j) = MIN(lowerSurface(i,j)+0.5*(minthic-Dtmp), 0.0)
      bedrockTopography(i,j) = bedrockTopography(i,j)-0.5*(minthic-Dtmp)
  elseif ( groundedMask(i,j) .gt. 0.5 ) then
      lowerSurface     (i,j) = bedrockTopography(i,j) 
  elseif ( openOceanMask(i,j) .gt. 0.5 ) then
      upperSurface     (i,j) = 0.0
      lowerSurface     (i,j) = 0.0
  endif
enddo
enddo

!---------------------------------------                      
! Modification of the fields :
! -> 1-grid point extension all around
! -> names as in NEMO

ALLOCATE( Bathymetry_ISOMIP     ( INT(mx/2)+2, INT(my/2)+2  ) )
ALLOCATE( isf_draft_ISOMIP      ( INT(mx/2)+2, INT(my/2)+2  ) )
ALLOCATE( isf_thick_ISOMIP      ( INT(mx/2)+2, INT(my/2)+2  ) )
ALLOCATE( maskisf_ISOMIP        ( INT(mx/2)+2, INT(my/2)+2  ) )
ALLOCATE( maskoce_ISOMIP        ( INT(mx/2)+2, INT(my/2)+2  ) )
ALLOCATE( tmp_Bathymetry_ISOMIP ( INT(mx/2)+2, INT(my/2)+2  ) )
ALLOCATE( tmp_isf_draft_ISOMIP  ( INT(mx/2)+2, INT(my/2)+2  ) )
ALLOCATE( x_ISOMIP              ( INT(mx/2)+2               ) )
ALLOCATE( y_ISOMIP              (              INT(my/2)+2  ) )

do ii=2,INT(mx/2)+1
 do jj=2,INT(my/2)+1

  maskisf_ISOMIP(ii,jj) = (   floatingMask(2*ii-3,2*jj-3) &
  &                         + floatingMask(2*ii-2,2*jj-3) &
  &                         + floatingMask(2*ii-3,2*jj-2) &
  &                         + floatingMask(2*ii-2,2*jj-2) ) * 0.25

  maskoce_ISOMIP(ii,jj) = (   openOceanMask(2*ii-3,2*jj-3) &
  &                         + openOceanMask(2*ii-2,2*jj-3) &
  &                         + openOceanMask(2*ii-3,2*jj-2) &
  &                         + openOceanMask(2*ii-2,2*jj-2) ) * 0.25

  Bathymetry_ISOMIP(ii,jj) = (   bedrockTopography(2*ii-3,2*jj-3) &
  &                            + bedrockTopography(2*ii-2,2*jj-3) &
  &                            + bedrockTopography(2*ii-3,2*jj-2) &
  &                            + bedrockTopography(2*ii-2,2*jj-2) ) * (-0.25)


  ! We mask ocean values for the ice-shelf interpolation :
  isf_draft_ISOMIP(ii,jj) = (   lowerSurface(2*ii-3,2*jj-3) * (1 - openOceanMask(2*ii-3,2*jj-3))          &
  &                           + lowerSurface(2*ii-2,2*jj-3) * (1 - openOceanMask(2*ii-2,2*jj-3))          &
  &                           + lowerSurface(2*ii-3,2*jj-2) * (1 - openOceanMask(2*ii-3,2*jj-2))          &
  &                           + lowerSurface(2*ii-2,2*jj-2) * (1 - openOceanMask(2*ii-2,2*jj-2)) ) * (-1) &
  &                       / (                                 (1 - openOceanMask(2*ii-3,2*jj-3))          &
  &                           +                               (1 - openOceanMask(2*ii-2,2*jj-3))          &
  &                           +                               (1 - openOceanMask(2*ii-3,2*jj-2))          &
  &                           +                               (1 - openOceanMask(2*ii-2,2*jj-2)) + eps ) 

  ! We mask ocean values for the ice-shelf interpolation :
  isf_thick_ISOMIP(ii,jj) = (   ( upperSurface(2*ii-3,2*jj-3) - lowerSurface(2*ii-3,2*jj-3) ) * (1 - openOceanMask(2*ii-3,2*jj-3))          &
  &                           + ( upperSurface(2*ii-2,2*jj-3) - lowerSurface(2*ii-2,2*jj-3) ) * (1 - openOceanMask(2*ii-2,2*jj-3))          &
  &                           + ( upperSurface(2*ii-3,2*jj-2) - lowerSurface(2*ii-3,2*jj-2) ) * (1 - openOceanMask(2*ii-3,2*jj-2))          &
  &                           + ( upperSurface(2*ii-2,2*jj-2) - lowerSurface(2*ii-2,2*jj-2) ) * (1 - openOceanMask(2*ii-2,2*jj-2)) ) * (-1) &
  &                       / (                                                                   (1 - openOceanMask(2*ii-3,2*jj-3))          &
  &                           +                                                                 (1 - openOceanMask(2*ii-2,2*jj-3))          &
  &                           +                                                                 (1 - openOceanMask(2*ii-3,2*jj-2))          &
  &                           +                                                                 (1 - openOceanMask(2*ii-2,2*jj-2)) + eps ) 

 enddo
 x_ISOMIP(ii) = ( x(2*ii-3) + x(2*ii-2) ) * 0.5
enddo

do jj=2,INT(my/2)+1 
  y_ISOMIP(jj) = ( y(2*jj-3) + y(2*jj-2) ) * 0.5
enddo

maskisf_ISOMIP(          1,2:INT(my/2)+1) = maskisf_ISOMIP(          2,2:INT(my/2)+1)
maskisf_ISOMIP(INT(mx/2)+2,2:INT(my/2)+1) = maskisf_ISOMIP(INT(mx/2)+1,2:INT(my/2)+1)

maskisf_ISOMIP(:,INT(my/2)+2) = maskisf_ISOMIP(:,INT(my/2)+1)
maskisf_ISOMIP(:,          1) = maskisf_ISOMIP(:,          2)

maskoce_ISOMIP(          1,2:INT(my/2)+1) = maskoce_ISOMIP(          2,2:INT(my/2)+1)
maskoce_ISOMIP(INT(mx/2)+2,2:INT(my/2)+1) = maskoce_ISOMIP(INT(mx/2)+1,2:INT(my/2)+1)

maskoce_ISOMIP(:,INT(my/2)+2) = maskoce_ISOMIP(:,INT(my/2)+1)
maskoce_ISOMIP(:,          1) = maskoce_ISOMIP(:,          2)

Bathymetry_ISOMIP(          1,2:INT(my/2)+1) = Bathymetry_ISOMIP(          2,2:INT(my/2)+1)
Bathymetry_ISOMIP(INT(mx/2)+2,2:INT(my/2)+1) = Bathymetry_ISOMIP(INT(mx/2)+1,2:INT(my/2)+1)

Bathymetry_ISOMIP(:,INT(my/2)+2) = Bathymetry_ISOMIP(:,INT(my/2)+1)
Bathymetry_ISOMIP(:,          1) = Bathymetry_ISOMIP(:,          2)

isf_draft_ISOMIP(          1,2:INT(my/2)+1) = isf_draft_ISOMIP(          2,2:INT(my/2)+1)
isf_draft_ISOMIP(INT(mx/2)+2,2:INT(my/2)+1) = isf_draft_ISOMIP(INT(mx/2)+1,2:INT(my/2)+1)

isf_draft_ISOMIP(:,INT(my/2)+2) = isf_draft_ISOMIP(:,INT(my/2)+1)
isf_draft_ISOMIP(:,          1) = isf_draft_ISOMIP(:,          2)

isf_thick_ISOMIP(          1,2:INT(my/2)+1) = isf_thick_ISOMIP(          2,2:INT(my/2)+1)
isf_thick_ISOMIP(INT(mx/2)+2,2:INT(my/2)+1) = isf_thick_ISOMIP(INT(mx/2)+1,2:INT(my/2)+1)

isf_thick_ISOMIP(:,INT(my/2)+2) = isf_thick_ISOMIP(:,INT(my/2)+1)
isf_thick_ISOMIP(:,          1) = isf_thick_ISOMIP(:,          2)

x_ISOMIP(          1) = x_ISOMIP(          2) - ( x_ISOMIP( 3) - x_ISOMIP( 2) )
x_ISOMIP(INT(mx/2)+2) = x_ISOMIP(INT(mx/2)+1) + ( x_ISOMIP( 3) - x_ISOMIP( 2) )

y_ISOMIP(          1) = y_ISOMIP(          2) - ( y_ISOMIP( 3) - y_ISOMIP( 2) )
y_ISOMIP(INT(my/2)+2) = y_ISOMIP(INT(my/2)+1) + ( y_ISOMIP( 3) - y_ISOMIP( 2) )

write(*,*) 'check1 : ', maskisf_ISOMIP(108,38), Bathymetry_ISOMIP(108,38), isf_draft_ISOMIP(108,38)

!------------------------------------------------------
! Further dig bathymetry (50%) and ice draft (50%):
! where more than 50% isf

do i=1,mx
do j=1,my
  Dtmp = Bathymetry_ISOMIP(i,j) - isf_draft_ISOMIP(i,j)
  if ( maskisf_ISOMIP(i,j) .ge. 0.5 .and. Dtmp .lt. minthic ) then ! dig isf and bathy
      isf_draft_ISOMIP (i,j) = isf_draft_ISOMIP (i,j) - 0.5*(minthic-Dtmp)
      Bathymetry_ISOMIP(i,j) = Bathymetry_ISOMIP(i,j) + 0.5*(minthic-Dtmp)
      maskisf_ISOMIP(i,j) = 1.0
      maskoce_ISOMIP(i,j) = 0.0
  elseif ( maskisf_ISOMIP(i,j) .ge. 0.5 ) then
      maskisf_ISOMIP(i,j) = 1.0
      maskoce_ISOMIP(i,j) = 0.0
  elseif ( maskisf_ISOMIP(i,j) .lt. 0.5 .and. maskoce_ISOMIP(i,j) .ge. 0.5 ) then ! remove ice if mostly ocean
      isf_draft_ISOMIP(i,j) = 0.0
      maskisf_ISOMIP(i,j) = 0.0
      maskoce_ISOMIP(i,j) = 1.0
  elseif ( maskisf_ISOMIP(i,j) .lt. 0.5 ) then ! fill the gap if mostly grounded ice
      isf_draft_ISOMIP(i,j) = Bathymetry_ISOMIP(i,j)
      maskisf_ISOMIP(i,j) = 0.0
      maskoce_ISOMIP(i,j) = 0.0
  endif
enddo
enddo

write(*,*) 'check2 : ', maskisf_ISOMIP(108,38), Bathymetry_ISOMIP(108,38), isf_draft_ISOMIP(108,38)

DO jl=1,100
  do irs=-1,1,2
    tmp_Bathymetry_ISOMIP(:,:) = Bathymetry_ISOMIP(:,:)
    tmp_isf_draft_ISOMIP (:,:) = isf_draft_ISOMIP (:,:)
    do i=2,mx-1
    do j=2,my-1
      Dtmp = Bathymetry_ISOMIP(i,j) - isf_draft_ISOMIP(i,j)
      Dtmp2 = Bathymetry_ISOMIP(i+irs,j) - isf_draft_ISOMIP(i+irs,j)
      if ( maskisf_ISOMIP(i,j) .gt. 0.1 .and. maskisf_ISOMIP(i+irs,j) .gt. 0.1  &
         & .and. Bathymetry_ISOMIP(i,j) .le. isf_draft_ISOMIP(i+irs,j)+0.4*minthic  &
         & .and. Bathymetry_ISOMIP(i,j) .gt. isf_draft_ISOMIP(i+irs,j)          ) then
         tmp_Bathymetry_ISOMIP(i,j) = Bathymetry_ISOMIP(i,j) + max(0.0, isf_draft_ISOMIP(i+irs,j)+0.05*minthic - Bathymetry_ISOMIP(i,j) ) 
      elseif ( maskisf_ISOMIP(i,j) .gt. 0.1 .and. maskisf_ISOMIP(i+irs,j) .gt. 0.1 &
             & .and. isf_draft_ISOMIP(i,j) .ge. Bathymetry_ISOMIP(i+irs,j)-0.4*minthic &
             & .and. isf_draft_ISOMIP(i,j) .lt. Bathymetry_ISOMIP(i+irs,j)         ) then
         tmp_isf_draft_ISOMIP(i,j) = isf_draft_ISOMIP(i,j) - max(0.0, isf_draft_ISOMIP(i,j) - (Bathymetry_ISOMIP(i+irs,j)-0.05*minthic) )
      elseif ( maskisf_ISOMIP(i,j) .gt. 0.1 .and. maskoce_ISOMIP(i+irs,j) .gt. 0.1 &
             & .and. isf_draft_ISOMIP(i,j) .ge. Bathymetry_ISOMIP(i+irs,j)-0.4*minthic &
             & .and. isf_draft_ISOMIP(i,j) .lt. Bathymetry_ISOMIP(i+irs,j)         ) then
         tmp_isf_draft_ISOMIP(i,j) = isf_draft_ISOMIP(i,j) - max(0.0, isf_draft_ISOMIP(i,j) - (Bathymetry_ISOMIP(i+irs,j)-0.05*minthic) )
      endif 
    enddo
    enddo
    Bathymetry_ISOMIP(:,:) = tmp_Bathymetry_ISOMIP(:,:)
    isf_draft_ISOMIP (:,:) = tmp_isf_draft_ISOMIP (:,:)
  enddo
  !-
  do jrs=-1,1,2
    tmp_Bathymetry_ISOMIP(:,:) = Bathymetry_ISOMIP(:,:)
    tmp_isf_draft_ISOMIP (:,:) = isf_draft_ISOMIP (:,:)
    do i=2,mx-1
    do j=2,my-1
      Dtmp = Bathymetry_ISOMIP(i,j) - isf_draft_ISOMIP(i,j)
      Dtmp2 = Bathymetry_ISOMIP(i,j+jrs) - isf_draft_ISOMIP(i,j+jrs)
      if ( maskisf_ISOMIP(i,j) .gt. 0.1 .and. maskisf_ISOMIP(i,j+jrs) .gt. 0.1 &
         & .and. Bathymetry_ISOMIP(i,j) .le. isf_draft_ISOMIP(i,j+jrs)+0.4*minthic &
         & .and. Bathymetry_ISOMIP(i,j) .gt. isf_draft_ISOMIP(i,j+jrs)         ) then
         tmp_Bathymetry_ISOMIP(i,j) = Bathymetry_ISOMIP(i,j) + max(0.0, isf_draft_ISOMIP(i,j+jrs)+0.05*minthic - Bathymetry_ISOMIP(i,j) ) 
      elseif ( maskisf_ISOMIP(i,j) .gt. 0.1 .and. maskisf_ISOMIP(i,j+jrs) .gt. 0.1 &
             & .and. isf_draft_ISOMIP(i,j) .ge. Bathymetry_ISOMIP(i,j+jrs)-0.4*minthic &
             & .and. isf_draft_ISOMIP(i,j) .lt. Bathymetry_ISOMIP(i,j+jrs)         ) then
         tmp_isf_draft_ISOMIP(i,j) = isf_draft_ISOMIP(i,j) - max(0.0, isf_draft_ISOMIP(i,j) - (Bathymetry_ISOMIP(i,j+jrs)-0.05*minthic) )
      elseif ( maskisf_ISOMIP(i,j) .gt. 0.1 .and. maskoce_ISOMIP(i,j+jrs) .gt. 0.1 &
             & .and. isf_draft_ISOMIP(i,j) .ge. Bathymetry_ISOMIP(i,j+jrs)-0.4*minthic &
             & .and. isf_draft_ISOMIP(i,j) .lt. Bathymetry_ISOMIP(i,j+jrs)         ) then
         tmp_isf_draft_ISOMIP(i,j) = isf_draft_ISOMIP(i,j) - max(0.0, isf_draft_ISOMIP(i,j) - (Bathymetry_ISOMIP(i,j+jrs)-0.05*minthic) )
      endif 
    enddo
    enddo
    Bathymetry_ISOMIP(:,:) = tmp_Bathymetry_ISOMIP(:,:)
    isf_draft_ISOMIP (:,:) = tmp_isf_draft_ISOMIP (:,:)
  enddo
ENDDO

write(*,*) 'check3 : ', maskisf_ISOMIP(108,38), Bathymetry_ISOMIP(108,38), isf_draft_ISOMIP(108,38)

where( Bathymetry_ISOMIP(:,:) .lt. 0.0 )
  Bathymetry_ISOMIP(:,:) = 0.e0
endwhere

write(*,*) 'check4 : ', maskisf_ISOMIP(108,38), Bathymetry_ISOMIP(108,38), isf_draft_ISOMIP(108,38)

where( isf_draft_ISOMIP(:,:) .lt. 0.0 .or. maskoce_ISOMIP(:,:) .ge. 0.5 )
  isf_draft_ISOMIP(:,:) = 0.e0
endwhere

write(*,*) 'check5 : ', maskisf_ISOMIP(108,38), Bathymetry_ISOMIP(108,38), isf_draft_ISOMIP(108,38)

where( Bathymetry_ISOMIP(:,:) .lt. isf_draft_ISOMIP(:,:) )
  isf_draft_ISOMIP(:,:) = Bathymetry_ISOMIP(:,:)
endwhere

write(*,*) 'check6 : ', maskisf_ISOMIP(108,38), Bathymetry_ISOMIP(108,38), isf_draft_ISOMIP(108,38)

!!! NB : we remove the ice shelf if less than 100m thickness to account for calving
!where( Bathymetry_ISOMIP(:,:) .gt. isf_draft_ISOMIP(:,:) .and. abs(isf_thick_ISOMIP(:,:)) .lt. 100.0 )
!  isf_draft_ISOMIP(:,:) = 0.e0
!endwhere

!---------------------------------------                      
! Writing new bathymetry netcdf file :                                   
                                            
write(*,*) 'Writing ', TRIM(file_out_bathy)

status = NF90_CREATE(TRIM(file_out_bathy),NF90_NOCLOBBER,fidM)
call erreur(status,.TRUE.,'create bathy')  

status = NF90_DEF_DIM(fidM,"x",INT(mx/2)+2,dimID_x)
call erreur(status,.TRUE.,"def_dimID_x")
status = NF90_DEF_DIM(fidM,"y",INT(my/2)+2,dimID_y)
call erreur(status,.TRUE.,"def_dimID_y")

status = NF90_DEF_VAR(fidM,"x",NF90_FLOAT,(/dimID_x/),x_ID)
call erreur(status,.TRUE.,"def_var_x_ID")
status = NF90_DEF_VAR(fidM,"y",NF90_FLOAT,(/dimID_y/),y_ID)
call erreur(status,.TRUE.,"def_var_y_ID")
status = NF90_DEF_VAR(fidM,"Bathymetry_isf",NF90_FLOAT,(/dimID_x,dimID_y/),Bathymetry_ID)
call erreur(status,.TRUE.,"def_var_Bathymetry_ID")

status = NF90_PUT_ATT(fidM,Bathymetry_ID,"valid_max",10565.)
call erreur(status,.TRUE.,"put_att_Bathymetry_ID")
status = NF90_PUT_ATT(fidM,Bathymetry_ID,"valid_min",0.)
call erreur(status,.TRUE.,"put_att_Bathymetry_ID")
status = NF90_PUT_ATT(fidM,Bathymetry_ID,"long_name","Mean depth by area")
call erreur(status,.TRUE.,"put_att_Bathymetry_ID")
status = NF90_PUT_ATT(fidM,Bathymetry_ID,"units","meters")
call erreur(status,.TRUE.,"put_att_Bathymetry_ID")

status = NF90_PUT_ATT(fidM,NF90_GLOBAL,"history","Created using build_bathy_isf_ISOMIP_EXP1_from_STD_grid.f90")
call erreur(status,.TRUE.,"att_GLOB")

status = NF90_ENDDEF(fidM)
call erreur(status,.TRUE.,"fin_definition") 

status = NF90_PUT_VAR(fidM,x_ID,x_ISOMIP)
call erreur(status,.TRUE.,"var_x_ID")
status = NF90_PUT_VAR(fidM,y_ID,y_ISOMIP)
call erreur(status,.TRUE.,"var_y_ID")
status = NF90_PUT_VAR(fidM,Bathymetry_ID,Bathymetry_ISOMIP)
call erreur(status,.TRUE.,"var_Bathymetry_ID")

status = NF90_CLOSE(fidM)
call erreur(status,.TRUE.,"final")         

!---------------------------------------                      
! Writing new ice shelf draft netcdf file :                                   
                                          
write(*,*) 'Writing ', TRIM(file_out_isf)
  
status = NF90_CREATE(TRIM(file_out_isf),NF90_NOCLOBBER,fidN)
call erreur(status,.TRUE.,'create isf draft') 

status = NF90_DEF_DIM(fidN,"x",INT(mx/2)+2,dimID_x)
call erreur(status,.TRUE.,"def_dimID_x")
status = NF90_DEF_DIM(fidN,"y",INT(my/2)+2,dimID_y)
call erreur(status,.TRUE.,"def_dimID_y")

status = NF90_DEF_VAR(fidN,"x",NF90_FLOAT,(/dimID_x/),x_ID)
call erreur(status,.TRUE.,"def_var_x_ID")
status = NF90_DEF_VAR(fidN,"y",NF90_FLOAT,(/dimID_y/),y_ID)
call erreur(status,.TRUE.,"def_var_y_ID")
status = NF90_DEF_VAR(fidN,"isf_draft",NF90_FLOAT,(/dimID_x,dimID_y/),isf_draft_ID)
call erreur(status,.TRUE.,"def_var_isf_draft_ID")

status = NF90_PUT_ATT(fidN,isf_draft_ID,"valid_max",10565.)
call erreur(status,.TRUE.,"put_att_isf_draft_ID")
status = NF90_PUT_ATT(fidN,isf_draft_ID,"valid_min",0.)
call erreur(status,.TRUE.,"put_att_isf_draft_ID")
status = NF90_PUT_ATT(fidN,isf_draft_ID,"long_name","Depth of ice shelf draft")
call erreur(status,.TRUE.,"put_att_isf_draft_ID")
status = NF90_PUT_ATT(fidN,isf_draft_ID,"units","meters")
call erreur(status,.TRUE.,"put_att_isf_draft_ID")

status = NF90_PUT_ATT(fidN,NF90_GLOBAL,"history","Created using build_bathy_isf_ISOMIP_EXP1_from_STD_grid.f90")
call erreur(status,.TRUE.,"att_GLOB")

status = NF90_ENDDEF(fidN)
call erreur(status,.TRUE.,"fin_definition") 

status = NF90_PUT_VAR(fidN,x_ID,x_ISOMIP)
call erreur(status,.TRUE.,"var_x_ID")
status = NF90_PUT_VAR(fidN,y_ID,y_ISOMIP)
call erreur(status,.TRUE.,"var_y_ID")
status = NF90_PUT_VAR(fidN,isf_draft_ID,isf_draft_ISOMIP)
call erreur(status,.TRUE.,"var_isf_draft_ID")

status = NF90_CLOSE(fidN)
call erreur(status,.TRUE.,"final")         

end program modif

!=========================================

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
