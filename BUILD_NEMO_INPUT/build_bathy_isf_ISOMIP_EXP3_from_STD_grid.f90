program modif                                         

USE netcdf                                            

IMPLICIT NONE                                         

INTEGER :: fidA, status, dimID_y, dimID_x, my, mx, openOceanMask_ID, groundedMask_ID, &
& floatingMask_ID, bedrockTopography_ID, lowerSurface_ID, upperSurface_ID, y_ID, x_ID,&
& fidM, fidN, Bathymetry_ID, isf_draft_ID, ii, jj, i, j, irs, jrs , jl, dimID_t, mt,  &
& mtime, kt, ll, mm

INTEGER,ALLOCATABLE,DIMENSION(:) :: month, year

CHARACTER(LEN=100) :: file_in, file_out_bathy, file_out_isf, tmpchar 

REAL*8,ALLOCATABLE,DIMENSION(:) :: y, x, time, HFtime

REAL*4,ALLOCATABLE,DIMENSION(:) ::  y_ISOMIP, x_ISOMIP

!- yearly MISOMIP+ Geometry :
REAL*8,ALLOCATABLE,DIMENSION(:,:,:) :: openOceanMask, groundedMask, floatingMask,&
& bedrockTopography, lowerSurface, upperSurface

!- recalculated at a monthly frequency :
REAL*8,ALLOCATABLE,DIMENSION(:,:,:) :: HFopenOceanMask, HFgroundedMask, HFfloatingMask,&
& HFbedrockTopography, HFlowerSurface, HFupperSurface

REAL*4,ALLOCATABLE,DIMENSION(:,:) :: Bathymetry_ISOMIP, isf_draft_ISOMIP, isf_thick_ISOMIP, &
& tmp_Bathymetry_ISOMIP, tmp_isf_draft_ISOMIP, maskisf_ISOMIP, maskoce_ISOMIP

REAL*4 :: minthic, Dtmp, Dtmp2

REAL*8 :: eps, facinf, facsup

file_in = 'Ocean3_input_geom_v1.01.nc' !! 1km resolution
! Format for file_out_bathy :
345 FORMAT('BATHY_ISF_EXP3/bathy_meter_ISOMIP_EXP3_',i2.2,'_',i4.4,'.nc')
! Format for file_out_isf :
346 FORMAT('BATHY_ISF_EXP3/isf_draft_meter_ISOMIP_EXP3_',i2.2,'_',i4.4,'.nc')

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
status = NF90_INQ_DIMID(fidA,"t",dimID_t)
call erreur(status,.TRUE.,"inq_dimID_t")

status = NF90_INQUIRE_DIMENSION(fidA,dimID_y,len=my)
call erreur(status,.TRUE.,"inq_dim_y")
status = NF90_INQUIRE_DIMENSION(fidA,dimID_x,len=mx)
call erreur(status,.TRUE.,"inq_dim_x")
status = NF90_INQUIRE_DIMENSION(fidA,dimID_t,len=mt)
call erreur(status,.TRUE.,"inq_dim_t")

ALLOCATE(  openOceanMask(mx,my,mt)  ) 
ALLOCATE(  groundedMask(mx,my,mt)  ) 
ALLOCATE(  floatingMask(mx,my,mt)  ) 
ALLOCATE(  bedrockTopography(mx,my,mt)  ) 
ALLOCATE(  lowerSurface(mx,my,mt)  ) 
ALLOCATE(  upperSurface(mx,my,mt)  ) 
ALLOCATE(  HFopenOceanMask(mx,my,12*mt)  )
ALLOCATE(  HFgroundedMask(mx,my,12*mt)  )
ALLOCATE(  HFfloatingMask(mx,my,12*mt)  )
ALLOCATE(  HFbedrockTopography(mx,my,12*mt)  )
ALLOCATE(  HFlowerSurface(mx,my,12*mt)  )
ALLOCATE(  HFupperSurface(mx,my,12*mt)  )
ALLOCATE(  y(my)  ) 
ALLOCATE(  x(mx)  ) 
ALLOCATE(  time(mt)  )
ALLOCATE(  HFtime(12*mt) )
ALLOCATE(  month(12*mt), year(12*mt) )

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

DO kt=1,12*mt
  HFtime(kt) = (kt-0.5) * 365. * 86400. / 12.
ENDDO

!--

kt=0
DO ll=1,mt

  do mm=1,6
    kt=kt+1    
    year(kt)  = ll-1
    month(kt) = mm
    facinf = (6-mm+0.5)/12
    facsup = (6+mm-0.5)/12
    HFopenOceanMask    (:,:,kt) = facinf * openOceanMask    (:,:,MAX(1,ll-1)) + facsup * openOceanMask    (:,:,ll)
    HFgroundedMask     (:,:,kt) = facinf * groundedMask     (:,:,MAX(1,ll-1)) + facsup * groundedMask     (:,:,ll)
    HFfloatingMask     (:,:,kt) = facinf * floatingMask     (:,:,MAX(1,ll-1)) + facsup * floatingMask     (:,:,ll)
    HFbedrockTopography(:,:,kt) = facinf * bedrockTopography(:,:,MAX(1,ll-1)) + facsup * bedrockTopography(:,:,ll)
    HFlowerSurface     (:,:,kt) = facinf * lowerSurface     (:,:,MAX(1,ll-1)) + facsup * lowerSurface     (:,:,ll)
    HFupperSurface     (:,:,kt) = facinf * upperSurface     (:,:,MAX(1,ll-1)) + facsup * upperSurface     (:,:,ll)
  enddo

  do mm=7,12
    kt=kt+1
    year(kt)  = ll-1
    month(kt) = mm
    facinf = (18-mm+0.5)/12
    facsup = (mm-6-0.5)/12
    HFopenOceanMask    (:,:,kt) = facinf * openOceanMask    (:,:,ll) + facsup * openOceanMask    (:,:,MIN(mt,ll+1))
    HFgroundedMask     (:,:,kt) = facinf * groundedMask     (:,:,ll) + facsup * groundedMask     (:,:,MIN(mt,ll+1))
    HFfloatingMask     (:,:,kt) = facinf * floatingMask     (:,:,ll) + facsup * floatingMask     (:,:,MIN(mt,ll+1))
    HFbedrockTopography(:,:,kt) = facinf * bedrockTopography(:,:,ll) + facsup * bedrockTopography(:,:,MIN(mt,ll+1))
    HFlowerSurface     (:,:,kt) = facinf * lowerSurface     (:,:,ll) + facsup * lowerSurface     (:,:,MIN(mt,ll+1))
    HFupperSurface     (:,:,kt) = facinf * upperSurface     (:,:,ll) + facsup * upperSurface     (:,:,MIN(mt,ll+1))
  enddo

ENDDO
mtime=kt

!------------------------------------------

DO kt=1,mtime

   !------------------------------------------
   ! Dig bathymetry (50%) and ice draft (50%):
   ! NB: do it on original bathy to help smooth transition
   !     and to keep information on original ice-shelf mask
   
   !! NB : we remove the ice shelf if less than 100m thickness to account for calving
   where ( HFupperSurface(:,:,kt) - HFlowerSurface(:,:,kt) .lt. 100.0 .and. HFfloatingMask(:,:,kt) .gt. 0.5 )
     HFfloatingMask (:,:,kt) = 0.0
     HFopenOceanMask(:,:,kt) = 1.0
     HFlowerSurface (:,:,kt) = 0.0
     HFupperSurface (:,:,kt) = 0.0
   endwhere
  
   do i=1,mx
   do j=1,my
     Dtmp = - HFbedrockTopography(i,j,kt) + HFlowerSurface(i,j,kt)
     if (       HFfloatingMask (i,j,kt) .gt. 0.5              &
        & .and. Dtmp               .gt. 0.0              &
        & .and. Dtmp               .lt. minthic          ) then
         HFlowerSurface     (i,j,kt) = MIN(HFlowerSurface(i,j,kt)+0.5*(minthic-Dtmp), 0.0)
         HFbedrockTopography(i,j,kt) = HFbedrockTopography(i,j,kt)-0.5*(minthic-Dtmp)
     elseif ( HFgroundedMask(i,j,kt) .gt. 0.5 ) then
         HFlowerSurface     (i,j,kt) = HFbedrockTopography(i,j,kt) 
     elseif ( HFopenOceanMask(i,j,kt) .gt. 0.5 ) then
         HFupperSurface     (i,j,kt) = 0.0
         HFlowerSurface     (i,j,kt) = 0.0
     endif
   enddo
   enddo
  
   !---------------------------------------                      
   ! Modification of the fields :
   ! -> 1-grid point extension all around
   ! -> names as in NEMO
  
   if ( kt == 1 ) then 
     ALLOCATE( Bathymetry_ISOMIP     ( INT(mx/2)+2, INT(my/2)+2  ) )
     ALLOCATE( isf_draft_ISOMIP      ( INT(mx/2)+2, INT(my/2)+2  ) )
     ALLOCATE( isf_thick_ISOMIP      ( INT(mx/2)+2, INT(my/2)+2  ) )
     ALLOCATE( maskisf_ISOMIP        ( INT(mx/2)+2, INT(my/2)+2  ) )
     ALLOCATE( maskoce_ISOMIP        ( INT(mx/2)+2, INT(my/2)+2  ) )
     ALLOCATE( tmp_Bathymetry_ISOMIP ( INT(mx/2)+2, INT(my/2)+2  ) )
     ALLOCATE( tmp_isf_draft_ISOMIP  ( INT(mx/2)+2, INT(my/2)+2  ) )
     ALLOCATE( x_ISOMIP              ( INT(mx/2)+2               ) )
     ALLOCATE( y_ISOMIP              (              INT(my/2)+2  ) )
   endif   

   do ii=2,INT(mx/2)+1
    do jj=2,INT(my/2)+1
   
     maskisf_ISOMIP(ii,jj) = (   HFfloatingMask(2*ii-3,2*jj-3,kt) &
     &                         + HFfloatingMask(2*ii-2,2*jj-3,kt) &
     &                         + HFfloatingMask(2*ii-3,2*jj-2,kt) &
     &                         + HFfloatingMask(2*ii-2,2*jj-2,kt) ) * 0.25
   
     maskoce_ISOMIP(ii,jj) = (   HFopenOceanMask(2*ii-3,2*jj-3,kt) &
     &                         + HFopenOceanMask(2*ii-2,2*jj-3,kt) &
     &                         + HFopenOceanMask(2*ii-3,2*jj-2,kt) &
     &                         + HFopenOceanMask(2*ii-2,2*jj-2,kt) ) * 0.25
   
     Bathymetry_ISOMIP(ii,jj) = (   HFbedrockTopography(2*ii-3,2*jj-3,kt) &
     &                            + HFbedrockTopography(2*ii-2,2*jj-3,kt) &
     &                            + HFbedrockTopography(2*ii-3,2*jj-2,kt) &
     &                            + HFbedrockTopography(2*ii-2,2*jj-2,kt) ) * (-0.25)
   
   
     ! We mask ocean values for the ice-shelf interpolation :
     isf_draft_ISOMIP(ii,jj) = (   HFlowerSurface(2*ii-3,2*jj-3,kt) * (1 - HFopenOceanMask(2*ii-3,2*jj-3,kt))          &
     &                           + HFlowerSurface(2*ii-2,2*jj-3,kt) * (1 - HFopenOceanMask(2*ii-2,2*jj-3,kt))          &
     &                           + HFlowerSurface(2*ii-3,2*jj-2,kt) * (1 - HFopenOceanMask(2*ii-3,2*jj-2,kt))          &
     &                           + HFlowerSurface(2*ii-2,2*jj-2,kt) * (1 - HFopenOceanMask(2*ii-2,2*jj-2,kt)) ) * (-1) &
     &                       / (                                      (1 - HFopenOceanMask(2*ii-3,2*jj-3,kt))          &
     &                           +                                    (1 - HFopenOceanMask(2*ii-2,2*jj-3,kt))          &
     &                           +                                    (1 - HFopenOceanMask(2*ii-3,2*jj-2,kt))          &
     &                           +                                    (1 - HFopenOceanMask(2*ii-2,2*jj-2,kt)) + eps ) 

    if ( year(kt) .eq. 8 .and. month(kt) .eq. 7 .and. jj.eq.21 .and. ii.ge.69 .and. ii.le.73 ) then
      write(*,*) '===================',ii,'===================='
      write(*,*) ' isf_draft_ISOMIP = ', isf_draft_ISOMIP(ii,jj)
      write(*,*) ' HFlowerSurface(2*ii-3,2*jj-3,kt) ', HFlowerSurface(2*ii-3,2*jj-3,kt)
      write(*,*) ' HFlowerSurface(2*ii-2,2*jj-3,kt) ', HFlowerSurface(2*ii-2,2*jj-3,kt)
      write(*,*) ' HFlowerSurface(2*ii-3,2*jj-2,kt) ', HFlowerSurface(2*ii-3,2*jj-2,kt)
      write(*,*) ' HFlowerSurface(2*ii-2,2*jj-2,kt) ', HFlowerSurface(2*ii-2,2*jj-2,kt)
      write(*,*) ' HFopenOceanMask(2*ii-3,2*jj-3,kt) ', HFopenOceanMask(2*ii-3,2*jj-3,kt)
      write(*,*) ' HFopenOceanMask(2*ii-2,2*jj-3,kt) ', HFopenOceanMask(2*ii-2,2*jj-3,kt)
      write(*,*) ' HFopenOceanMask(2*ii-3,2*jj-2,kt) ', HFopenOceanMask(2*ii-3,2*jj-2,kt)
      write(*,*) ' HFopenOceanMask(2*ii-2,2*jj-2,kt) ', HFopenOceanMask(2*ii-2,2*jj-2,kt)
    endif
   
     ! We mask ocean values for the ice-shelf interpolation :
     isf_thick_ISOMIP(ii,jj) = (   ( HFupperSurface(2*ii-3,2*jj-3,kt) - HFlowerSurface(2*ii-3,2*jj-3,kt) ) * (1 - HFopenOceanMask(2*ii-3,2*jj-3,kt))          &
     &                           + ( HFupperSurface(2*ii-2,2*jj-3,kt) - HFlowerSurface(2*ii-2,2*jj-3,kt) ) * (1 - HFopenOceanMask(2*ii-2,2*jj-3,kt))          &
     &                           + ( HFupperSurface(2*ii-3,2*jj-2,kt) - HFlowerSurface(2*ii-3,2*jj-2,kt) ) * (1 - HFopenOceanMask(2*ii-3,2*jj-2,kt))          &
     &                           + ( HFupperSurface(2*ii-2,2*jj-2,kt) - HFlowerSurface(2*ii-2,2*jj-2,kt) ) * (1 - HFopenOceanMask(2*ii-2,2*jj-2,kt)) ) * (-1) &
     &                       / (                                                                             (1 - HFopenOceanMask(2*ii-3,2*jj-3,kt))          &
     &                           +                                                                           (1 - HFopenOceanMask(2*ii-2,2*jj-3,kt))          &
     &                           +                                                                           (1 - HFopenOceanMask(2*ii-3,2*jj-2,kt))          &
     &                           +                                                                           (1 - HFopenOceanMask(2*ii-2,2*jj-2,kt)) + eps ) 
   
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
  
   !write(*,*) 'check2 : ', maskisf_ISOMIP(108,38), Bathymetry_ISOMIP(108,38), isf_draft_ISOMIP(108,38)
   
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
   
   where( Bathymetry_ISOMIP(:,:) .lt. 0.0 )
     Bathymetry_ISOMIP(:,:) = 0.e0
   endwhere
   
   where( isf_draft_ISOMIP(:,:) .lt. 0.0 .or. maskoce_ISOMIP(:,:) .ge. 0.5 )
     isf_draft_ISOMIP(:,:) = 0.e0
   endwhere
   
   where( Bathymetry_ISOMIP(:,:) .lt. isf_draft_ISOMIP(:,:) )
     isf_draft_ISOMIP(:,:) = Bathymetry_ISOMIP(:,:)
   endwhere
   
   !---------------------------------------                      
   ! Writing new bathymetry netcdf file :                                   
  
   write(file_out_bathy,345) month(kt), year(kt)
                                             
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
   
   status = NF90_PUT_ATT(fidM,NF90_GLOBAL,"history","Created using build_bathy_isf_ISOMIP_EXP3_from_STD_grid.f90")
   call erreur(status,.TRUE.,"att_GLOB")
   888 FORMAT('Bathy/Ice-shelf draft geometry corresponding to ',f13.1,' seconds after 01-JAN-0000')
   write(tmpchar,888) HFtime(kt)
   status = NF90_PUT_ATT(fidM,NF90_GLOBAL,"description",tmpchar)
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
                               
   write(file_out_isf,346) month(kt), year(kt)
              
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
   
   status = NF90_PUT_ATT(fidN,NF90_GLOBAL,"history","Created using build_bathy_isf_ISOMIP_EXP3_from_STD_grid.f90")
   call erreur(status,.TRUE.,"att_GLOB")
   status = NF90_PUT_ATT(fidN,NF90_GLOBAL,"description",tmpchar)
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

ENDDO !! DO kt=1,mt

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
