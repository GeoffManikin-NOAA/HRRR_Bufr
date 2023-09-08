#! /bin/sh -v

# Define the fortran compiler and options

WRFPATH=/gpfs/hps/nco/ops/nwprod/wrf_shared.v1.1.0-intel
FC   = ftn
CC  =
FLAGS= -assume byterecl
CPPFLAGS = -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -DUNDERSCORE

NETCDF=/usrx/local/prod/NetCDF/3.6.3/intel/sandybridge

NCDLIBS=-L$(NETCDF)/lib/ -lnetcdf

INCLD= -I${NETCDF}/include  -I${WRFPATH}/inc -I${WRFPATH}/frame # -I${GSIDIR}/include
LIBS=$(WRFPATH)/external/io_int/libwrfio_int.a \
	$(WRFPATH)/main/libwrflib.a \
        $(WRFPATH)/external/fftpack/fftpack5/libfftpack.a \
        $(WRFPATH)/external/io_netcdf/libwrfio_nf.a \
        $(WRFPATH)/external/io_grib1/libio_grib1.a \
        $(WRFPATH)/external/io_grib_share/libio_grib_share.a \
	$(WRFPATH)/frame/module_internal_header_util.o \
	$(WRFPATH)/frame/pack_utils.o \
    $(WRFPATH)/external/esmf_time_f90/libesmf_time.a \
    $(WRFPATH)/external/RSL_LITE/task_for_point.o \
    $(WRFPATH)/external/RSL_LITE/librsl_lite.a  


WRFFLAGS = -I$(WRFPATH)/external/io_quilt
W3=/gpfs/hps/nco/ops/nwprod/lib
W3LIBS = -L$(W3)/w3emc/v2.2.0/intel -L$(W3)/w3nco/v2.0.6/intel -L$(W3)/ip/v2.0.0/intel -L$(W3)/sp/v2.0.2/intel
CMD=	staids_v2.x
INCS=
FFLAGS=$(NCDFFLAGS) $(WRFFLAGS) -lmpi 
OBJS=	STALST_WRF.o
OBJST=	getIVariableB.o getVariableB.o getVariable.o kinds_mod.o count_recs_wrf_binary_file.o inventory_wrf_binary_file.o next_buf.o retrieve_index.o
####################################################
$(CMD): $(OBJS) $(OBJST)
	$(FC) -o $(CMD) $(OBJS) $(OBJST) $(NCDLIBS) $(WRFLIB) $(NCDLIBS) $(W3LIBS) $(NCDLIBS)
$(OBJS):	$(INCS)
$(OBJST):	$(INCS)
STALST_WRF.o:	STALST_WRF.f retrieve_index.f
	$(FC) -c -g $(FFLAGS) $*.f
getVariable.o:  getVariable.f
	$(FC) -c -FR getVariable.f
getIVariableB.o:  getIVariableB.f
	$(FC) -c -FR getIVariableB.f
getVariableB.o:  getVariableB.f
	$(FC) -c -FR getVariableB.f
kinds_mod.o: kinds_mod.f
	$(FC) -c -FR kinds_mod.f
count_recs_wrf_binary_file.o: count_recs_wrf_binary_file.f
	$(FC) -c -FR count_recs_wrf_binary_file.f
inventory_wrf_binary_file.o: inventory_wrf_binary_file.f
	$(FC) -c -FR inventory_wrf_binary_file.f
next_buf.o: next_buf.f
	$(FC) -c -FR next_buf.f
retrieve_index.o: retrieve_index.f
	$(FC) -c -FR retrieve_index.f
clean:	
	-rm -f $(OBJS)
