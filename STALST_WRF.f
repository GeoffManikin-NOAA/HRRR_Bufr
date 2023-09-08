      PROGRAM STALST
C----------------------------------------------------------------------
!      INCLUDE "parmeta"
      INCLUDE 'wrf_io_flags.h'

!     use kinds, only :: i_llong

C----------------------------------------------------------------------
      PARAMETER (NSTA=1500)
      PARAMETER (D2R=1.745329252E-2,R2D=57.29577951)
C----------------------------------------------------------------------
C      INCLUDE "LOOPS.com"
C----------------------------------------------------------------------
c      INCLUDE "MAPOT.com"
C----------------------------------------------------------------------
C      INCLUDE "MASKS.com"
C----------------------------------------------------------------------
C      INCLUDE "INDX.com"
C----------------------------------------------------------------------

      include 'mpif.h'



                            C H A R A C T E R
     & AN*1,AW*1,CID*8,CIDDUM*8,CIDX*8,VarName*31
     &, FILENAME*90,DateStr*19,startdate*19,varin*31,FILETYPE*90

C----------------------------------------------------------------------
                            D I M E N S I O N
     & RLAT(NSTA),RLON(NSTA),TLAT(NSTA),TLON(NSTA)
     &,RLATD(NSTA),RLOND(NSTA)
     &,IHDUM(NSTA),JHDUM(NSTA),IVDUM(NSTA),JVDUM(NSTA)
     &,IHSTA(NSTA),JHSTA(NSTA),IVSTA(NSTA),JVSTA(NSTA)
     &,KCLAS(NSTA),KCLASD(NSTA),IDDUM(NSTA),ID(NSTA)
     &,CIDDUM(NSTA),CID(NSTA)
C----------------------------------------------------------------------
                            R E A L
     & LAM0,DUM0D
     &,XOUT,YOUT,LATSTART,LATEND
     &,LONSTART,LONEND
     &,RLATIN(1),RLONIN(1),AXOUT(1),AYOUT(1)
C----------------------------------------------------------------------
	INTEGER DataHandle,JS,JE,GDS(200),IM,JM,LM

	INTEGER, allocatable:: IDUMMY(:,:)
	REAL,    allocatable:: SM(:,:),DUMMY(:,:),DUM3D(:,:,:)
	REAL,    allocatable:: GDLAT(:,:),GDLON(:,:),DUM1D(:)

      character*132, allocatable :: datestr_all(:)
      character*132, allocatable :: varname_all(:)
      integer, allocatable       :: domainend_all(:,:)
      integer, allocatable       :: start_block(:)
      integer, allocatable       :: end_block(:)
      integer, allocatable       :: start_byte(:)
      integer, allocatable       :: end_byte(:)
      integer(kind=8), allocatable           :: file_offset(:)
      integer this_offset, this_length


C----------------------------------------------------------------------
      DATA LNHB/12/,LUNLST/15/
C----------------------------------------------------------------------
C***
C***  READ IN THE MASKS THAT ARE NEEDED
C***

      read(105,111) fileName
      read(105,111) fileType
      read(105,112) DateStr
	
!	len=index(filename,' ')-1
!	Datestr(1:19)=fileName(len-18:len)

      read(DateStr,300) iyear,imn,iday,ihrst
 300  format(i4,1x,i2,1x,i2,1x,i2)
 111  format(a90)
 112  format(a19)


	JS=1
	JE=JM
        JEV=JM+1


	write(0,*) 'DateStr= ', DATESTR
	write(0,*) 'trimmed filename: ', trim(fileName), 'END'
	write(0,*) 'filetype: ', filetype

       IF (trim(filetype) .ne. 'netcdf') THEN

         call ext_int_ioinit(Status)
          print*,'called ioinit', Status
         call ext_int_open_for_read( trim(fileName), 0, 0, " ",
     &  DataHandle, Status)
          write(0,*) 'called open for read', Status

       if ( Status /= 0 ) then
         write(0,*) 'error opening ',fileName,' Status = ',Status;stop
       endif

!      call ext_int_get_dom_ti_char(DataHandle
!     1 ,'START_DATE',startdate, status )

!	write(0,*) 'status: ', status
!        write(0,*) 'startdate= ',startdate
!      read(startdate,15)iyear,imn,iday,ihrst
! 15   format(i4,1x,i2,1x,i2,1x,i2)
      write(0,*) 'start yr mo day hr =',iyear,imn,iday,ihrst

        call ext_int_get_dom_ti_integer(DataHandle,
     &   'WEST-EAST_GRID_DIMENSION',itmp
     + ,1,ioutcount,istatus)

	write(0,*) 'istatus for ext_int_get_dom_ti_integer ', istatus

	write(6,*) 'west-east dimension: ', itmp
	IM=itmp-1

        call ext_int_get_dom_ti_integer(DataHandle,
     &   'SOUTH-NORTH_GRID_DIMENSION',itmp
     + ,1,ioutcount,istatus)

	write(6,*) 'south-north dimension: ', itmp
	JM=itmp-1

        call ext_int_get_dom_ti_integer(DataHandle,
     &   'BOTTOM-TOP_GRID_DIMENSION',itmp
     + ,1,ioutcount,istatus)

	write(6,*) 'bottom-top dimension: ', itmp
	LM=itmp

        call ext_int_get_dom_ti_real(DataHandle,'DX',tmp
     + ,1,ioutcount,istatus)
        dxval=nint(tmp)
        write(6,*) 'dxval= ', dxval
        call ext_int_get_dom_ti_real(DataHandle,'DY',tmp
     + ,1,ioutcount,istatus)
        dyval=nint(tmp)
        write(6,*) 'dyval= ', dyval
        call ext_int_get_dom_ti_real(DataHandle,'CEN_LAT',tmp
     + ,1,ioutcount,istatus)
        cenlat=nint(1000.*tmp)
        write(6,*) 'cenlat= ', cenlat
        call ext_int_get_dom_ti_real(DataHandle,'CEN_LON',tmp
     + ,1,ioutcount,istatus)
        cenlon=nint(1000.*tmp)
        write(6,*) 'cenlon= ', cenlon
        call ext_int_get_dom_ti_real(DataHandle,'TRUELAT1',tmp
     + ,1,ioutcount,istatus)
        truelat1=nint(1000.*tmp)
        write(6,*) 'truelat1= ', truelat1
        call ext_int_get_dom_ti_real(DataHandle,'TRUELAT2',tmp
     + ,1,ioutcount,istatus)
        truelat2=nint(1000.*tmp)
        write(6,*) 'truelat2= ', truelat2
        call ext_int_get_dom_ti_integer(DataHandle,'MAP_PROJ',itmp
     + ,1,ioutcount,istatus)
        maptype=itmp
        write(6,*) 'maptype is ', maptype
        write(0,*) 'maptype is ', maptype
!need to get DT
        call ext_ncd_get_dom_ti_real(DataHandle,'DT',tmp
     +    ,1,ioutcount,istatus)
        DT=tmp
        print*,'DT= ',DT
	write(6,*) 'allocate with IM, JM, LM: ', IM, JM, LM
	JS=1
	JE=JM
        JEV=JM+1

        endif

       IF (trim(filetype) .eq. 'binary') THEN

	if (ALLOCATED(IDUMMY)) deallocate(IDUMMY)
	if (ALLOCATED(SM))     deallocate(SM)
	if (ALLOCATED(DUMMY))  deallocate(DUMMY)
	if (ALLOCATED(DUM3D))  deallocate(DUM3D)
	if (ALLOCATED(DUM1D))  deallocate(DUM1D)
	if (ALLOCATED(GDLAT))  deallocate(GDLAT)
	if (ALLOCATED(GDLON))  deallocate(GDLON)

	ALLOCATE(IDUMMY(IM,JM))
	ALLOCATE(SM(IM,JM))
	ALLOCATE(DUMMY(IM,JM))
	ALLOCATE(DUM3D(IM+1,JM+1,LM+1))
	ALLOCATE(DUM1D(LM+1))
	ALLOCATE(GDLAT(IM,JM))
	ALLOCATE(GDLON(IM,JM))

!        call ext_int_get_dom_ti_real(DataHandle,'DX',tmp
!     + ,1,ioutcount,istatus)
!        dxval=nint(tmp)
!        write(6,*) 'dxval= ', dxval
!        call ext_int_get_dom_ti_real(DataHandle,'DY',tmp
!     + ,1,ioutcount,istatus)
!        dyval=nint(tmp)
!        write(6,*) 'dyval= ', dyval
!        call ext_int_get_dom_ti_real(DataHandle,'CEN_LAT',tmp
!     + ,1,ioutcount,istatus)
!        cenlat=nint(1000.*tmp)
!        write(6,*) 'cenlat= ', cenlat
!        call ext_int_get_dom_ti_real(DataHandle,'CEN_LON',tmp
!     + ,1,ioutcount,istatus)
!        cenlon=nint(1000.*tmp)
!        write(6,*) 'cenlon= ', cenlon
!        call ext_int_get_dom_ti_real(DataHandle,'TRUELAT1',tmp
!     + ,1,ioutcount,istatus)
!        truelat1=nint(1000.*tmp)
!        write(6,*) 'truelat1= ', truelat1
!        call ext_int_get_dom_ti_real(DataHandle,'TRUELAT2',tmp
!     + ,1,ioutcount,istatus)
!        truelat2=nint(1000.*tmp)
!        write(6,*) 'truelat2= ', truelat2
!        call ext_int_get_dom_ti_integer(DataHandle,'MAP_PROJ',itmp
!     + ,1,ioutcount,istatus)
!        maptype=itmp
!        write(6,*) 'maptype is ', maptype
!        write(0,*) 'maptype is ', maptype
!!need to get DT
!        call ext_ncd_get_dom_ti_real(DataHandle,'DT',tmp
!     +    ,1,ioutcount,istatus)
!        DT=tmp
!        print*,'DT= ',DT
!
      VarName='LU_INDEX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM+1,1,JM+1,1,IM+1,JS,JE,1)

      VarName='ZNU'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,
     &  1,1,1,LM,1,1,1,LM)

      VarName='ZNW'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,
     &  1,1,1,LM+1,1,1,1,LM+1)

      call getVariableB(fileName,DateStr,DataHandle,'ZS',SLDPTH2,
     & 1,1,1,NSOIL,1,1,1,NSOIL)

      call getVariableB(fileName,DateStr,DataHandle,'DZS',SLDPTH2,
     & 1,1,1,NSOIL,1,1,1,NSOIL)

      VarName='U'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM+1,1,JM+1,LM+1,IM+1,JS,JE,LM)

      VarName='Z_FORCE'

!      VarName='V'
!      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM3D,
!     &  IM+1,1,JM+1,LM+1,IM,JS,JEV,LM)

!      VarName='W'
!      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM3D,
!     &  IM+1,1,JM+1,LM+1,IM,JS,JE,LM+1)

!      VarName='PH'
!      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM3D,
!     &  IM+1,1,JM+1,LM+1,IM,JS,JE,LM+1)

!      VarName='PHB'
!      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM3D,
!     &  IM+1,1,JM+1,LM+1,IM,JS,JE,LM+1)

      VarName='T'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,LM)

      VarName='MU'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='MUB'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='MU0'
      call getVariableB(fileName,DateStr,DataHandle,VarName,PT,
     &  1,1,1,1,1,1,1,1)
      VarName='P'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,LM)
c
      VarName='QVAPOR'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,LM)
      VarName='QCLOUD'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,LM)
      VarName='TSLB'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,NSOIL)
      call getVariableB(fileName,DateStr,DataHandle,'ZS',SLDPTH2,
     & 1,1,1,NSOIL,1,1,1,NSOIL)

      call getVariableB(fileName,DateStr,DataHandle,'DZS',SLDPTH2,
     & 1,1,1,NSOIL,1,1,1,NSOIL)

      VarName='Q2'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='T2'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='TH2'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='U10'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='V10'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='SMOIS'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,NSOIL)
      VarName='SH2O'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,NSOIL)
      VarName='SMSTAV'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='SFROFF'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='UDROFF'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='IVGTYP'
      call getIVariableB(fileName,DateStr,DataHandle,VarName,IDUMMY
     &  ,IM,1,JM,1,IM,JS,JE,1)
      print*,'IVGTYP at ',ii,jj,' = ',IDUMMY(ii,jj)
      VarName='ISLTYP'
      call getIVariableB(fileName,DateStr,DataHandle,VarName,IDUMMY
     &  ,IM,1,JM,1,IM,JS,JE,1)
      VarName='VEGFRA'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='GRDFLX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='ACSNOW'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='ACSNOM'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='SNOW'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='SNOWH'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='CANWAT'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='SST'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='THZ0'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='QZ0'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='UZ0'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='VZ0'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='QSFC'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='AKHS'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='AKMS'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='PB'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,LM)
      VarName='MAPFAC_M'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='MAPFAC_U'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='MAPFAC_V'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='F'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='E'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='SINALPHA'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='COSALPHA'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='HGT'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='TSK'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='P_TOP'
      call getVariableB(fileName,DateStr,DataHandle,VarName,PT,
     &  1,1,1,1,1,1,1,1)
      VarName='FNM'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,
     &  1,1,1,LM,1,1,1,LM)
      VarName='FNP'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,
     &  1,1,1,LM,1,1,1,LM)

      VarName='RDNW'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,
     &  1,1,1,LM,1,1,1,LM)

      VarName='RDN'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,
     &  1,1,1,LM,1,1,1,LM)

      VarName='DNW'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,
     &  1,1,1,LM,1,1,1,LM)

      VarName='DN'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,
     &  1,1,1,LM,1,1,1,LM)

      VarName='CFN'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM0D,
     &  1,1,1,1,1,1,1,1)

      VarName='CFN1'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM0D,
     &  1,1,1,1,1,1,1,1)

      VarName='EPSTS'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM0D,
     &  1,1,1,1,1,1,1,1)
      VarName='RAINC'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='RAINNC'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='GSW'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='GLW'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='XLAT'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

       do j = 1, jm
        do i = 1, im
            GDLAT ( i, j ) = dummy ( i, j )
	if ( i .eq. im/2 .and. mod(J,5) .eq. 0 ) then
		write(6,*) 'i,j,gdlat: ', i,j,dummy(i,j)
	endif
        end do
       end do


      VarName='XLONG'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

       do j = 1, jm
        do i = 1, im
            GDLON ( i, j ) = dummy ( i, j )
	if ( j .eq. jm/2 .and. mod(I,5) .eq. 0 ) then
		write(6,*) 'i,j,gdlon: ', i,j,dummy(i,j)
	endif
        end do
       end do

      VarName='LU_INDEX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='TMN'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
	
c XLAND 1 land 2 sea
      VarName='XLAND'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = 1, jm
        do i = 1, im
            SM ( i, j ) = dummy ( i, j ) - 1.0
        end do
       end do

	write(6,*) 'domain SM vals'
	do J=JM,1,-JM/20
	write(6,633) (SM(I,J),I=1,IM,IM/40)
	enddo
  633	format(50f2.0)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSEIF (trim(filetype) .eq. 'binarympiio') THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	write(0,*) 'binarympiio'

	call MPI_INIT(ierr)
        call mpi_comm_rank(MPI_COMM_WORLD,mype,ierr)

! get 3-D variables
c closing wrf io api
      call ext_int_ioclose ( DataHandle, Status )
c start calling mpi io
      iunit=33
      call count_recs_wrf_binary_file(iunit, fileName, nrecs)
      print*,'- FILE CONTAINS ',nrecs, ' RECORDS'
	write(0,*) 'past count_recs_wrf_binary_file'
      allocate (datestr_all(nrecs))
      allocate (varname_all(nrecs))
      allocate (domainend_all(3,nrecs))
      allocate (start_block(nrecs))
      allocate (end_block(nrecs))
      allocate (start_byte(nrecs))
      allocate (end_byte(nrecs))
      allocate (file_offset(nrecs))

	write(0,*) 'to inventory_wrf_binary_file'
      call inventory_wrf_binary_file(iunit, filename, nrecs,
     +                datestr_all,varname_all,domainend_all,
     +      start_block,end_block,start_byte,end_byte,file_offset)

	write(0,*) 'past inventory_wrf_binary_file'
      close(iunit)

	write(0,*) 'to mpi_file_open'
      call mpi_file_open(mpi_comm_world, filename
     + , mpi_mode_rdonly,mpi_info_null, iunit, ierr)
	write(0,*) 'past mpi_file_open ', ierr 
      if (ierr /= 0) then
       print*,"Error opening file with mpi io"
       write(0,*)'Error opening file with mpi io'
       stop
      end if

	write(0,*) 'im,jm,lm= ',im,jm,lm

      print*,'im,jm,lm= ',im,jm,lm

	if (ALLOCATED(IDUMMY)) deallocate(IDUMMY)
	if (ALLOCATED(SM))     deallocate(SM)
	if (ALLOCATED(DUMMY))  deallocate(DUMMY)
	if (ALLOCATED(DUM3D))  deallocate(DUM3D)
	if (ALLOCATED(DUM1D))  deallocate(DUM1D)
	if (ALLOCATED(GDLAT))  deallocate(GDLAT)
	if (ALLOCATED(GDLON))  deallocate(GDLON)

	ALLOCATE(IDUMMY(IM,JM))
	ALLOCATE(SM(IM,JM))
	ALLOCATE(DUMMY(IM,JM))
	ALLOCATE(DUM3D(IM+1,JM+1,LM+1))
	ALLOCATE(DUM1D(LM+1))
	ALLOCATE(GDLAT(IM,JM))
	ALLOCATE(GDLON(IM,JM))

	write(0,*) 'allocated arrays'


	write(0,*) 'seek out XLAT'
      VarName='XLAT'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
        write(0,*) 'iret from retrieve_index: ', iret

        call mpi_file_read_at(iunit,file_offset(index+1)
     + ,GDLAT,IM*JM,mpi_real4
     + , mpi_status_ignore, ierr)



      VarName='XLONG'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)
        call mpi_file_read_at(iunit,file_offset(index+1)
     + ,GDLON,IM*JM,mpi_real4
     + , mpi_status_ignore, ierr)

c XLAND 1 land 2 sea
      VarName='XLAND'
      call retrieve_index(index,VarName,varname_all,nrecs,iret)

        call mpi_file_read_at(iunit,file_offset(index+1)
     + ,DUMMY,IM*JM,mpi_real4
     + , mpi_status_ignore, ierr)

       do j = 1, jm
        do i = 1, im
            SM ( i, j ) = dummy ( i, j ) - 1.0
        end do
       end do

	do J=JM,1,-JM/30
	write(0,632) (SM(I,J),I=1,IM,IM/35)
        enddo
  632	format(40(F2.0))

	write(0,*) 'done with this'
      call mpi_file_close(mpi_comm_world, filename
     + , mpi_mode_rdonly,mpi_info_null, iunit, ierr)
        CALL MPI_FINALIZE(MPI_COMM_WORLD,IERR)



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	ELSEIF (trim(filetype) .eq. 'netcdf') THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	write(6,*) 'in netcdf branch'

        write(0,*) 'call ext_ncd_ioinit'
!         call ext_ncd_ioinit(Status)
         call ext_ncd_ioinit(SysDepInfo,Status)

        write(0,*) 'return ext_ncd_ioinit'
          print*,'called ioinit', Status
         call ext_ncd_open_for_read( trim(fileName), 0, 0, " ",
     &  DataHandle, Status)
          print*,'called open for read', Status

       if ( Status /= 0 ) then
         print*,'error opening ',fileName, ' Status = ', Status ; stop
       endif

!      call ext_ncd_get_dom_ti_char(DataHandle
!     1 ,'START_DATE',startdate, status )
!        print*,'startdate= ',startdate
!      read(startdate,15)iyear,imn,iday,ihrst

      print*,'start yr mo day hr =',iyear,imn,iday,ihrst

        call ext_ncd_get_dom_ti_integer(DataHandle,
     &   'WEST-EAST_GRID_DIMENSION',itmp
     + ,1,ioutcount,istatus)

	write(6,*) 'west-east dimension: ', itmp
	IM=itmp-1

        call ext_ncd_get_dom_ti_integer(DataHandle,
     &   'SOUTH-NORTH_GRID_DIMENSION',itmp
     + ,1,ioutcount,istatus)

	write(6,*) 'south-north dimension: ', itmp
	JM=itmp-1

        call ext_ncd_get_dom_ti_integer(DataHandle,
     &   'BOTTOM-TOP_GRID_DIMENSION',itmp
     + ,1,ioutcount,istatus)

	write(6,*) 'bottom-top dimension: ', itmp
	LM=itmp

	JS=1
	JE=JM
        JEV=JM+1

	if (ALLOCATED(IDUMMY)) deallocate(IDUMMY)
	if (ALLOCATED(SM))     deallocate(SM)
	if (ALLOCATED(DUMMY))  deallocate(DUMMY)
	if (ALLOCATED(DUM3D))  deallocate(DUM3D)
	if (ALLOCATED(DUM1D))  deallocate(DUM1D)
	if (ALLOCATED(GDLAT))  deallocate(GDLAT)
	if (ALLOCATED(GDLON))  deallocate(GDLON)

	ALLOCATE(IDUMMY(IM,JM))
	ALLOCATE(SM(IM,JM))
	ALLOCATE(DUMMY(IM,JM))
	ALLOCATE(DUM3D(IM+1,JM+1,LM+1))
	ALLOCATE(DUM1D(LM+1))
	ALLOCATE(GDLAT(IM,JM))
	ALLOCATE(GDLON(IM,JM))

	write(6,*) 'allocate with IM, JM, LM: ', IM, JM, LM



        call ext_ncd_get_dom_ti_real(DataHandle,'DX',tmp
     + ,1,ioutcount,istatus)
        dxval=nint(tmp)
        write(6,*) 'dxval= ', dxval
        call ext_ncd_get_dom_ti_real(DataHandle,'DY',tmp
     + ,1,ioutcount,istatus)
        dyval=nint(tmp)
        write(6,*) 'dyval= ', dyval
        call ext_ncd_get_dom_ti_real(DataHandle,'CEN_LAT',tmp
     + ,1,ioutcount,istatus)
        cenlat=nint(1000.*tmp)
        write(6,*) 'cenlat= ', cenlat
        call ext_ncd_get_dom_ti_real(DataHandle,'CEN_LON',tmp
     + ,1,ioutcount,istatus)
        cenlon=nint(1000.*tmp)
        write(6,*) 'cenlon= ', cenlon
        call ext_ncd_get_dom_ti_real(DataHandle,'TRUELAT1',tmp
     + ,1,ioutcount,istatus)
        truelat1=nint(1000.*tmp)
        write(6,*) 'truelat1= ', truelat1
        call ext_ncd_get_dom_ti_real(DataHandle,'TRUELAT2',tmp
     + ,1,ioutcount,istatus)
        truelat2=nint(1000.*tmp)
        write(6,*) 'truelat2= ', truelat2
        call ext_ncd_get_dom_ti_integer(DataHandle,'MAP_PROJ',itmp
     + ,1,ioutcount,istatus)
        maptype=itmp
        write(6,*) 'maptype is ', maptype
!need to get DT
        call ext_ncd_get_dom_ti_real(DataHandle,'DT',tmp
     +    ,1,ioutcount,istatus)
        DT=tmp
        print*,'DT= ',DT
	write(6,*) 'DateStr: ', DateStr

      VarName='XLAT'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

       do j = 1, jm
        do i = 1, im
            GDLAT ( i, j ) = dummy ( i, j )
        if ( i .eq. im/2 ) then
                write(6,*) 'i,j,gdlat: ', i,j,dummy(i,j)
        endif
        end do
       end do


      VarName='XLONG'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

       do j = 1, jm
        do i = 1, im
            GDLON ( i, j ) = dummy ( i, j )
        if ( j .eq. jm/2 ) then
                write(6,*) 'i,j,gdlon: ', i,j,dummy(i,j)
        endif
        end do
       end do
c XLAND 1 land 2 sea
      VarName='XLAND'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
       do j = 1, jm
        do i = 1, im
            SM ( i, j ) = dummy ( i, j ) - 1.0
        end do
       end do

        write(6,*) 'domain SM vals'
        do J=JM,1,-JM/20
        write(6,633) (SM(I,J),I=1,IM,IM/40)
        enddo

	ENDIF

	write(0,*) 'here defining GDS'

C	Define a GDS, then use GDSWIZ to find N.N. point

	latstart=gdlat(1,1)
	lonstart=gdlon(1,1)
	latend=gdlat(im,jm)
	lonend=gdlon(im,jm)

	write(0,*) 'latstart, lonstart: ', latstart, lonstart
	write(0,*) 'latend, lonend: ', latend, lonend

	GDS=-1

	write(6,*) 'maptype: ', maptype

        if(maptype .eq. 1)THEN  ! Lambert conformal
          GDS(1)=3
          GDS(2)=im
          GDS(3)=jm
          GDS(4)=int(LATSTART*1000)
          GDS(5)=int(LONSTART*1000)
          GDS(6)=8
          GDS(7)=CENLON
          GDS(8)=DXVAL
          GDS(9)=DYVAL
          GDS(10)=0
          GDS(11)=64
          GDS(12)=TRUELAT2
          GDS(13)=TRUELAT1


!mptest	if (CENLON .gt. 0) GDS(7)=360000.-CENLON
!mptest	if (LONSTART .gt. 0) GDS(5)=int((360.-LONSTART)*1000)

        ELSE IF(MAPTYPE .EQ. 2)THEN  !Polar stereographic
          GDS(1)=5
          GDS(2)=im
          GDS(3)=jm
          GDS(4)=int(LATSTART*1000)
          GDS(5)=int(LONSTART*1000)
          GDS(6)=8
          GDS(7)=CENLON
          GDS(8)=DXVAL
          GDS(9)=DYVAL
          GDS(10)=0
          GDS(11)=64
        ELSE IF(MAPTYPE .EQ. 3)THEN  !Mercator
          GDS(1)=1
          GDS(2)=im
          GDS(3)=jm
          GDS(4)=int(LATSTART*1000)
          GDS(5)=int(LONSTART*1000)
          GDS(6)=8
          GDS(7)=int(LATEND*1000)
          GDS(8)=int(LONEND*1000)
          GDS(9)=TRUELAT1
          GDS(10)=0
          GDS(11)=64
          GDS(12)=DXVAL
          GDS(13)=DYVAL
        ELSE IF(MAPTYPE .EQ. 6)THEN  ! Rot lat
          GDS(1)=205
          GDS(2)=im
          GDS(3)=jm
          GDS(4)=int(LATSTART*1000)
          GDS(5)=int(LONSTART*1000)
          GDS(6)=8
          GDS(7)=cenlat
          GDS(8)=cenlon
          GDS(9)=122
          GDS(10)=122
          GDS(11)=64
          GDS(12)=int(LATEND*1000)
          GDS(13)=int(LONEND*1000)
        END IF



CWRF	LMH, LMV are LM by default.

C***
C***  CALCULATE THE I-INDEX EAST-WEST INCREMENTS
C***
C      DO J=1,JM
C        IHE(J)=MOD(J+1,2)
C        IHW(J)=IHE(J)-1
C        IVE(J)=MOD(J,2)
C        IVW(J)=IVE(J)-1
C      ENDDO
C

C      write(6,*)' tph0d=',tph0d,' tlm0d=',tlm0d
C	write(6,*) 'dphd, dlmd: ', dlmd,dphd
C      PHI0=TPH0D*D2R
C      LAM0=-1.0*TLM0D*D2R
CC
C      DLAM=DLMD*D2R
C
C   SBDB AND WBDB ARE 2 ROWS INSIDE THE DOMAIN BOUNDARY
C   TO TRY TO EXCLUDE POINTS RIGHT ON THE BOUNDARY
C
C      SBDB=SBD+2.0*DPHD
C      WBDB=WBD+2.0*DLMD
!      write(6,*)' sbdb=',sbdb,' wbdb=',wbdb
C***
C***  LOOP THROUGH ALL THE STATIONS TO COUNT HOW MANY INSIDE DOMAIN
C***
	write(6,*) 'GDS= ', GDS
      NSUM=0
   25 READ(LUNLST,50,END=100)IDX,RLATX,AN,RLONX,AW,CIDX,KCLS
   50 FORMAT(I6,F6.2,A1,F7.2,A1,1X,A4,I3)
      IF (AN.EQ.'S') RLATX=-1.0*RLATX
!mptest      IF (AW.EQ.'E') RLONX=360.0-RLONX
      IF (AW.EQ.'W') RLONX=-RLONX
C      RLATX=RLATX*D2R
C      RLONX=RLONX*D2R

c	rlatin(1)=rlatx
c	rlonin(1)=rlonx

	CALL GDSWIZ(GDS,-1,1,-9999.,xout,yout,
     &			RLONX,RLATX,NRET,0,DUMMY,DUMMY)

c	xout=axout(1)
C	yout=ayout(1)

	if (IDX .eq. 912320) then

	write(6,*) 'RLATX, RLONX, XOUT, YOUT: ', 
     &		RLATX,RLONX,XOUT,YOUT

	endif

	IDUM=int(xout+0.5)
	JDUM=int(yout+0.5)

	if (IDUM .le. 2 .or. IDUM .ge. im-1 .OR.
     &	    JDUM .le. 2 .or. JDUM .ge. jm-1) then
	  goto 25
	endif

C***
C***  CONVERT GEODETIC TO TRANSFORMED COORDINATES OF THE STATION
C***
C      X=COS(PHI0)*COS(RLATX)*COS(RLONX-LAM0)
C     1  +SIN(PHI0)*SIN(RLATX)
C      Y=-COS(RLATX)*SIN(RLONX-LAM0)
C      Z=COS(PHI0)*SIN(RLATX)
C     1  -SIN(PHI0)*COS(RLATX)*COS(RLONX-LAM0)
C      TLATX=R2D*ATAN(Z/SQRT(X*X+Y*Y))
C      TLONX=R2D*ATAN(Y/X)
C      IF(ABS(TLATX).GT.ABS(SBDB).OR.ABS(TLONX).GT.ABS(WBDB))GO TO 25
      NSUM=NSUM+1
      RLATD(NSUM)=RLATX
      RLOND(NSUM)=RLONX
      TLAT(NSUM)=TLATX
      TLON(NSUM)=TLONX
      IDDUM(NSUM)=IDX
      CIDDUM(NSUM)=CIDX
      KCLASD(NSUM)=KCLS
      GOTO 25
  100 CONTINUE
C
C----------------------------------------------------------------------
      DO 200 N2=1,NSUM
	RLATX=RLATD(N2)
	RLONX=RLOND(N2)

	CALL GDSWIZ(GDS,-1,1,-9999.,xout,yout,
     &			RLONX,RLATX,NRET,0,DUMMY,DUMMY)

	IDUM=int(xout+0.5)
	JDUM=int(yout+0.5)
C
      IF (KCLASD(N2).GE.10.AND.KCLASD(N2).LT.20) THEN
C
C   WE WANT THIS TO BE A LAND POINT, IF NOT, FIND THE CLOSEST LAND POINT
C
Cmp         IF (SM(IDUM,JDUM)+SICE(IDUM,JDUM).GT.0.5) THEN
         IF (SM(IDUM,JDUM).GT.0.5) THEN
          DMIN=99999.
          DO J=1,JM
            DO I=1,IM
             IF(SM(I,J).LT.0.5) THEN

C	Crude distance calculation, should be sufficient

	D1= ( (GDLAT(I,J)-GDLAT(IDUM,JDUM))**2. + 
     &	      (GDLON(I,J)-GDLON(IDUM,JDUM))**2. )**(0.5)
               IF(D1.LT.DMIN) THEN
                  DMIN=D1
		  INEW=I
		  JNEW=J
               ENDIF
             ENDIF
            ENDDO
          ENDDO
C	replace with new values
C	write(6,*) 'replace IDUM,JDUM: ', IDUM,JDUM
C	write(6,*) 'with INEW,JNEW: ', INEW,JNEW
	write(6,*) 'change I val from: ',IDUM, 'to: ', INEW
	write(6,*) 'change J val from: ',JDUM, 'to: ', JNEW

	IDUM=INEW
	JDUM=JNEW	
        ENDIF
      ENDIF
C
      IF (KCLASD(N2).GE.20.AND.KCLASD(N2).LT.30) THEN
C
C   WE WANT THIS TO BE A SEA POINT, IF NOT, FIND THE CLOSEST SEA POINT
C
        IF (SM(IDUM,JDUM).LT.0.5) THEN
          DMIN=99999.
          DO J=1,JM
            DO I=1,IM
             IF(SM(I,J).GT.0.5) THEN
	D1= ( (GDLAT(I,J)-GDLAT(IDUM,JDUM))**2. + 
     &	      (GDLON(I,J)-GDLON(IDUM,JDUM))**2. )**(0.5)

               IF(D1.LT.DMIN) THEN
                  DMIN=D1
                  INEW=I
                  JNEW=J
               ENDIF
             ENDIF
            ENDDO
          ENDDO

C	replace with new values

	IDUM=INEW
	JDUM=JNEW	

        ENDIF
      ENDIF

      IF (KCLASD(N2).LT.10.OR.KCLASD(N2).GT.30) THEN
	write(6,*) 'avoiding point : ', CIDDUM(N2), 'due to class', KCLASD(N2)
      ENDIF

	IHDUM(N2)=IDUM
	JHDUM(N2)=JDUM
	IVDUM(N2)=IDUM
	JVDUM(N2)=JDUM
  180 FORMAT(I6,2F8.2,I8,5I4,' NO NEIGHBOR ON SAME STEP')
  200 CONTINUE
C***
      NDUM=0
      DO I=1,NSUM
        IF(IDDUM(I).NE.-99999)THEN
          NDUM=NDUM+1
          IHSTA(NDUM)=IHDUM(I)
          JHSTA(NDUM)=JHDUM(I)
          IVSTA(NDUM)=IVDUM(I)
          JVSTA(NDUM)=JVDUM(I)
          ID(NDUM)=IDDUM(I)
          RLAT(NDUM)=RLATD(I)
          RLON(NDUM)=RLOND(I)
          CID(NDUM)=CIDDUM(I)
          KCLAS(NDUM)=MOD(KCLASD(I),10)
        ENDIF
      ENDDO
C
      NSUM=NDUM
C-----------------------------------------------------------------------
      DTR=1.74532925E-2
      WRITE(63)NSUM,ID,RLAT*DTR,RLON*DTR,IHSTA,JHSTA,IVSTA,JVSTA,CID
      WRITE(6,20)NSUM
   20 FORMAT('STALST:  NUMBER OF PROFILE STATIONS ',I5)
      WRITE(6,30)(ID(N),RLAT(N),RLON(N)
     1,               IHSTA(N),JHSTA(N),IVSTA(N),JVSTA(N)
     2,               CID(N),N=1,NSUM)
   30 FORMAT(2X,I6,2F8.2,4I8,1X,A8)
C-----------------------------------------------------------------------
      STOP
      END
