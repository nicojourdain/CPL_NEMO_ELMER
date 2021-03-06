module subs
contains
subroutine check(status)
    USE NETCDF
    integer, intent ( in) :: status

    if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if

end subroutine check

SUBROUTINE BilinealInterp(xP,yP,NETCDFValues,meltInterp, dxarg, dyarg, xInitarg, yInitarg)

        USE types
        USE CoordinateSystems
        USE SolverUtils
        USE ElementDescription
        USE DefUtils

        REAL(KIND=dp), INTENT(IN):: xP,yP
        REAL(Kind=dp), INTENT(IN) :: NETCDFValues(:,:)
        REAL(KIND=dp), INTENT(OUT) :: meltInterp
        REAL(KIND=dp) :: iFract, jFract, melt11, melt12, melt21, melt22
        INTEGER :: iIndex, jIndex
        REAL(KIND=dp) :: DX , DY, xInit, yInit
        REAL(KIND=dp), INTENT(IN), OPTIONAL ::  DXarg , DYarg, xInitarg, yInitarg


        !Default values for MISOMIP
        if( .not. present(DXarg)) then
            DX=2000.0
        else
            DX = DXarg
        end if

        if( .not. present(DYarg)) then
            DY=2000.0
        else
            DY = DYarg
        end if

        if( .not. present(xInitarg)) then
            xInit = 319000.0
        else 
            xInit = xInitarg
        end if

        if( .not. present(yInitarg)) then
            yInit = -1000.0
        else
            yInit = yInitarg
        end if

        !Where the node is found in the NetCDF grid?

        iIndex = int(floor((xP - xInit ) / DX)+1)
        iFract = (xP - xInit) - (iIndex-1) * DX

        jIndex = int(floor((yP - (yInit)) / DY)+1)
        jFract = (yP - (yInit)) - (jIndex-1) * DY

        if (iIndex .gt. 0) then
                melt11 = NETCDFValues(iIndex,jIndex)
                melt12 = NETCDFValues(iIndex,jIndex+1)
                melt21 = NETCDFValues(iIndex+1,jIndex)
                melt22 = NETCDFValues(iIndex+1,jIndex+1)
                meltInterp = 1/(DX*DY) * ( &
                        melt11*(DX-iFract)*(DY-jFract) + &
                        melt21*(iFract)*(DY-jFract) + &
                        melt12*(DX-iFract)*(jFract) + &
                        melt22*(iFract)*(jFract) &
                        )
        else    
                meltInterp = 0.0_dp
        end if


End subroutine BilinealInterp

SUBROUTINE NEAREST_Melt_Rate(node, MeltRates, MeltRatesPerm, Nodes_X , Nodes_Y, NNodes, MeltVal)
        USE types
        USE CoordinateSystems
        USE SolverUtils
        USE ElementDescription
        USE DefUtils

        INTEGER, INTENT(IN) :: node, NNodes
        REAL(KIND=dp), POINTER, INTENT(IN) :: NODES_X(:), NODES_Y(:)
        REAL(KIND=dp), POINTER, INTENT(IN) :: MeltRates(:)
        INTEGER, POINTER, INTENT(IN) :: MeltRatesPerm(:)
        REAL(KIND=dp), INTENT(OUT) :: MeltVal

        INTEGER, ALLOCATABLE:: NearestNodes(:)
        REAL(KIND=dp), ALLOCATABLE :: NearestDIST(:)
        INTEGER :: counter, n, i
        REAL :: distMin, dist

        REAL :: TOL=1000 


        allocate(nearestnodes(nNodes),Nearestdist(nNodes))  !!Too much allocattion, but the size depends on the tolerance, Improve this part of the code later
        nearestnodes (:) = 0.0
        nearestdist (:) = 0.0
        counter = 0
        distMin = 1e15
        dist = 0.0
        MeltVal = 0.0
        Do n = 1, nNodes
                if (MeltRates(MeltRatesPerm(n)) .ne. 0.0_dp) then
                  dist = SQRT((Nodes_X(n) - Nodes_X(node)) * (Nodes_X(n) - Nodes_X(node)) + (Nodes_Y(n) - Nodes_Y(node)) * (Nodes_Y(n) - Nodes_Y(node)))
                  if (dist .lt. distMin) then
        !               Print *, 'Dist LT: ', n, Nodes_X(n), Nodes_Y(n), dist, distMin
                        distMin = dist
                  end if
                end if
        END DO
        counter = 0
        Do n = 1, nNodes
              if (MeltRates(MeltRatesPerm(n)) .ne. 0.0_dp) then
                dist = SQRT((Nodes_X(n) - Nodes_X(node)) * (Nodes_X(n) - Nodes_X(node)) + (Nodes_Y(n) - Nodes_Y(node)) * (Nodes_Y(n) - Nodes_Y(node)))
                if (ABS(dist - distMin) .le. TOL) then
                        counter = counter + 1
                        Print *, 'Dist EQ: ', n, Nodes_X(n), Nodes_Y(n), dist, distMin, MeltRates(MeltRatesPerm(n))
                        nearestnodes(counter) = n
                        nearestdist(counter) = dist
                end if
              end if
        end do

        Print *, 'End of DO: ', nearestnodes, counter, distMin
        TotalDist = sum(nearestdist)
        do i=1,counter
                MeltVal = MeltVal + MeltRates(MeltRatesPerm(nearestnodes(i))) * (TotalDist - nearestdist(i))
                NormInterpol = NormInterpol + (TotalDist - nearestdist(i))
        end do
        MeltVal = MeltVal/ NormInterpol
        Print *, "MeltVal Final: " , MeltVal
                                 
END SUBROUTINE NEAREST_Melt_Rate

end module subs

SUBROUTINE MISOMIP_Melt_Consv( Model,Solver,dt,Transient )
!------------------------------------------------------------------------------
  USE CoordinateSystems
  USE MeshUtils
  USE DefUtils
  USE subs
  USE NETCDF

  IMPLICIT NONE
!------------------------------------------------------------------------------
  TYPE(Model_t)  :: Model
  TYPE(Solver_t), TARGET :: Solver
  LOGICAL ::  Transient
  REAL(KIND=dp) :: dt

!------------------------------------------------------------------------------
!    Local variables
!------------------------------------------------------------------------------
  TYPE(Mesh_t),POINTER :: Mesh
  TYPE(Solver_t),POINTER :: PSolver
  TYPE(Variable_t),POINTER :: MeltVar=>NULL(), GMVar=>NULL()
  TYPE(Nodes_t) :: ElementNodes
  TYPE(GaussIntegrationPoints_t) :: IntegStuff
  TYPE(Element_t),POINTER ::  Element

  REAL(kind=dp),allocatable :: VisitedNode(:),db(:),Basis(:),dBasisdx(:,:) 
  REAL(kind=dp) :: u,v,w,SqrtElementMetric,s



  INTEGER , POINTER :: MeltPerm(:), GMPerm(:), NodeIndexes(:)
  REAL(KIND=dp) , POINTER :: Melt(:),GM(:)
  REAL(KIND=dp) , POINTER ::DATAPointer(:,:)
  INTEGER :: NMax, ncid, node, ncidDraft, e, t, n, i, j
  REAL(KIND=dp) ::  xP , yP, meltInt

  LOGICAL,SAVE :: Initialized = .FALSE.
  LOGICAL,SAVE :: ExtrudedMesh=.False. , FirsTime=.TRUE.
  LOGICAL :: Found, Got, stat, Parallel

  CHARACTER(len = 200) :: FILE_NAME,meltValue
  CHARACTER(len = 200) :: FILE_NAME_DRAFT
  CHARACTER(LEN=MAX_NAME_LEN) :: SolverName='InitMELTMISOMIP'

  CHARACTER(len = 200), parameter :: meltname='fwfisf', xname='x', yname='y'
  REAL(KIND=dp), allocatable, target :: meltvarNC_Av(:,:), meltvarNC(:,:,:), xVarNC(:), yVarNC(:), MeltTemp(:)
  INTEGER, allocatable, save :: GMOLDPerm(:)
  REAL(KIND=dp), allocatable, SAVE :: MELT_NEMO_AREA(:,:), GROUNDED_NODES(:), GMOLD(:)
  INTEGER :: varXid, varYid, varid, dimid1, dimid2, dimid3, lenX, lenY, lenTime, status1, res, ierr

  REAL(KIND=dp) :: x_NC_Init, x_NC_Fin, y_NC_Init, y_NC_Fin, x_NC_Res, y_NC_Res, localInteg, Integ, Integ_Reduced
  REAL(KIND=dp) :: Melt_NEMO_Integ , Melt_NEMO_Integ_Reduced, Factor_Corr, MeltVal, facunits


!------------------------------------------------------------------------------

 SAVE NMAX

 Mesh => Model % Mesh

 GMVar => VariableGet( Model % Mesh % Variables, 'GroundedMask')
 IF (.NOT.ASSOCIATED(GMVar)) THEN
     Message='GroundedMask not found'
     CALL FATAL(SolverName,Message)
 END IF

 GMPerm => GMVar % Perm
 GM => GMVar % Values

 MeltVar => VariableGet( Model % Mesh % Variables, 'Melt')
 IF (.NOT.ASSOCIATED(MeltVar)) THEN
     Message='Melt not found'
     CALL FATAL(SolverName,Message)
 END IF

 MeltPerm => MeltVar % Perm
 Melt => MeltVar % Values

 IF (FIRSTIME) THEN

  NMAX=Solver % Mesh % NumberOfNodes
  ALLOCATE(VisitedNode(NMAX),  &
          Basis(Model % MaxElementNodes),  &
          dBasisdx(Model % MaxElementNodes,3))

  NMax = Solver % Mesh % NumberOfNodes

  FILE_NAME_DRAFT = GetString(Solver % Values,'Draft file',Got)

  IF (.NOT. Got) then
     Message='Draft File not found'
     CALL FATAL(SolverName,Message)
  END IF

  FILE_NAME = GetString(Solver % Values,'Melt rates file',Got)

  IF (.NOT. Got) then
     Message='Melt Rates File not found'
     CALL FATAL(SolverName,Message)
  END IF

  facunits = GetCReal( Model % Constants, 'Factor melt units', Found )
  IF (.NOT.Found) &
             CALL FATAL(SolverName,'<Factor melt units> not found')

  ! GET NTCDF DIMENSIONS FOR ALLOCATION
  CALL check(nf90_open(FILE_NAME,NF90_NOWRITE,ncid))
  CALL check(nf90_open(FILE_NAME_DRAFT,NF90_NOWRITE,ncidDraft))
  status1=nf90_inq_dimid(ncid,"x",dimid1)
  status1=nf90_inquire_dimension(ncid,dimid1,len=lenX)

  status1=nf90_inq_dimid(ncid,"y",dimid2)
  status1=nf90_inquire_dimension(ncid,dimid2,len=lenY)

  status1=nf90_inq_dimid(ncid,"time_counter",dimid3)
  status1=nf90_inquire_dimension(ncid,dimid3,len=lenTime)

  allocate(meltvarNC(lenX,lenY,lenTime))
  allocate(meltvarNC_Av(lenX,lenY))
  allocate(MELT_NEMO_AREA(lenX,lenY))
  allocate(GROUNDED_NODES(NMAX))
  allocate(GMOLD(NMAX),MeltTemp(NMax))
  allocate(GMOLDPERM(NMAX))
  allocate(xVarNC(lenX))
  allocate(yVarNC(lenY))


  !GET Variables

  status1=nf90_inq_varid(ncid,meltname,varid)

  status1=nf90_get_var(ncid,varid,meltVarNC)

  status1=nf90_inq_varid(ncidDraft,xname,varXid)

  status1=nf90_get_var(ncidDraft,varXid,xVarNC)

  status1=nf90_inq_varid(ncidDraft,yname,varYid)

  status1=nf90_get_var(ncidDraft,varYid,yVarNC)

  x_NC_Init = MINVAL(xVarNC)
  x_NC_Fin = MAXVAL(xVarNC)
  y_NC_Init = MINVAL(yVarNC)
  y_NC_Fin = MAXVAL(yVarNC)

  x_NC_Res = xVarNC(2)-xVarNC(1)
  y_NC_Res = yVarNC(2)-yVarNC(1)

  MeltVarNC_Av = SUM(meltvarNC,dim=3)/lenTime

  Melt_NEMO_Integ = 0.0_dp

  DO i=1,lenX 
        DO j=1,lenY
                Melt_NEMO_Area(i,j) = meltvarNC_Av(i,j) * X_NC_Res * Y_NC_Res * facunits ! from kg/m^2/s to meter of ice / yr
        END DO
  END DO

  Melt_NEMO_Integ = SUM(Melt_NEMO_Area)

  DO node=1, nMax 
        xP =  Mesh % Nodes % x(node)
        yP =  Mesh % Nodes % y(node)

        if (xP .gt. x_NC_Fin .or. xP .lt. x_NC_Init) then
                Melt(MeltPerm(node)) = 0.0_dp   
                cycle
        end if

        if (yP .gt. y_NC_Fin .or. yP .lt. y_NC_Init) then
                Melt(MeltPerm(node)) = 0.0_dp
                cycle
        end if

        if (GM(GMPerm(node)) .lt. 0.5) then
                CALL BiLinealInterp(xP,yP,meltvarNC_Av,meltInT, x_NC_Res, y_NC_Res, x_NC_Init, y_NC_Init)
                Melt(MeltPerm(node)) = meltInt * facunits ! from kg/m^2/s to meter of ice / yr
        else
                Melt(MeltPerm(node)) = 0.0_dp
        end if
  end do

  CALL check(nf90_close(ncid))
  CALL check(nf90_close(ncidDraft))

  Integ = 0.0_dp  

  DO e=1,Solver % NumberOfActiveElements
     Element => GetActiveElement(e)
     CALL GetElementNodes( ElementNodes )
     n = GetElementNOFNodes()
     NodeIndexes => Element % NodeIndexes

     VisitedNode(NodeIndexes(1:n))=VisitedNode(NodeIndexes(1:n))+1.0_dp

     localInteg = 0.0_dp

     IntegStuff = GaussPoints( Element )
     DO t=1,IntegStuff % n
        U = IntegStuff % u(t)
        V = IntegStuff % v(t)
        W = IntegStuff % w(t)
        stat = ElementInfo(Element,ElementNodes,U,V,W,SqrtElementMetric, &
             Basis,dBasisdx )

        s = SqrtElementMetric * IntegStuff % s(t)

        localInteg = localInteg + s * SUM(Basis(:) * Melt(MeltPerm(NodeIndexes(:))))

     END DO
     Integ = Integ + localInteg
   END DO

    Parallel = .FALSE.
    IF ( ASSOCIATED( Solver % Matrix % ParMatrix ) ) THEN
            IF ( Solver %  Matrix % ParMatrix % ParEnv % PEs > 1 )  THEN
                    Parallel = .TRUE.
            END IF
    END IF

   IF (Parallel) THEN
        CALL MPI_ALLREDUCE(Integ,Integ_Reduced,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
   END IF

   Factor_Corr = Integ_Reduced / Melt_NEMO_Integ

   Melt(MeltPerm(:)) = Melt(MeltPerm(:)) / Factor_Corr

   IF (Solver % Matrix % ParMatrix % ParEnv % MyPE == 0) then
     WRITE(meltValue,'(F20.2)') Integ_Reduced 
     Message='TOTAL_MELT_RATE: '//meltValue
     CALL INFO(SolverName,Message,Level=1)
   END IF
 
 END IF !!FIRSTIME  

 GMVar => VariableGet( Model % Mesh % Variables, 'GroundedMask')
 IF (.NOT.ASSOCIATED(GMVar)) THEN
     Message='GroundedMask not found'
     CALL FATAL(SolverName,Message)
 END IF

 GMPerm => GMVar % Perm
 GM => GMVar % Values

 MeltVar => VariableGet( Model % Mesh % Variables, 'Melt')
 IF (.NOT.ASSOCIATED(MeltVar)) THEN
     Message='Melt not found'
     CALL FATAL(SolverName,Message)
 END IF

 MeltPerm => MeltVar % Perm
 Melt => MeltVar % Values
 MeltTemp = Melt
    
 IF (.NOT. FIRSTIME) Then
     DO t=1,NMax
         IF ((GMOLD(GMOLDPerm(t)) .gt. 0.5) .AND. (GM(GMPerm(t))) .lt. 0.5) THEN
               CALL NEAREST_Melt_Rate(t , Melt, MeltPerm, Model % Nodes % x, Model % Nodes % y, NMax, MeltVal)
               MeltTemp(MeltPerm(t)) = MeltVal
         END IF
     END DO
 END IF !NOT FIRSTIME

 Melt(:)=MeltTemp(:)

 GMOLD(:) = GM(:)
 GMOLDPerm(:) = GMPerm(:)
 FIRSTIME=.FALSE.
!!!
END SUBROUTINE MISOMIP_Melt_Consv

