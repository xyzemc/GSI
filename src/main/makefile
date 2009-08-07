SHELL=/bin/sh

#==============================================================================
#
# GSI Makefile
#
#==============================================================================

#------------
# Include machine dependent compile & load options
#------------
include ../../configure.gsi

COREROOT = $(COREDIR)
COREBIN  = $(COREROOT)/run
CORELIB  = $(COREROOT)/lib
COREINC  = $(COREROOT)/include
COREETC  = $(COREROOT)/etc
EXE_FILE = gsi.exe


# ---------
# Libraries
# ---------
LIBmpeu   = -L$(CORELIB) -lmpeu
LIBbufr   = -L$(CORELIB) -lbufr
LIBw3     = -L$(CORELIB) -lw3
LIBsp     = -L$(CORELIB) -lsp
LIBbacio  = -L$(CORELIB) -lbacio
LIBsfcio  = -L$(CORELIB) -lsfcio
LIBsigio  = -L$(CORELIB) -lsigio
LIBcrtm   = -L$(CORELIB) -lcrtm_gfsgsi
LIBtransf = -L$(CORELIB) -ltransf
LIBhermes = -L$(CORELIB) -lhermes
LIBgfsio  = -L$(CORELIB) -lgfsio

# --------------------------
# Default Baselibs Libraries
# --------------------------
LIBnetcdf       = -L$(NETCDFPATH)/lib $(NETCDFLIBS)
LIBwrf          = $(WRF_LIB)

# ------------------------
# Default System Libraries
# ------------------------
LIBmpi          = -lmpi
LIBsys          = -lessl_r -lmass -bdatapsize:64K -bstackpsize:64K




# --------------------
# Installing directory
# --------------------

  INSTALL_DIR = $(COREBIN)


# --------
# Log file
# --------

  LOG_FILE = log.make.$(EXE_FILE)


#-----------------------------------------------------------------------------
#                          -- Child make --
#-----------------------------------------------------------------------------

# ------------
# Source files
# ------------

  OBJS	= \
	obsmod.o \
	anberror.o \
	anbkerror_reg.o \
	anisofilter.o \
	balmod.o \
	berror.o \
	bkerror.o \
	bkgcov.o \
	bkgvar.o \
	bkgvar_rewgt.o \
	calctends.o \
	calctends_ad.o \
	calctends_tl.o \
	combine_radobs.o \
	compact_diffs.o \
	compute_derived.o \
	compute_fact10.o \
	constants.o \
	converr.o \
	convinfo.o \
	convthin.o \
	deter_subdomain.o \
	dprodx.o \
	dtast.o \
	fgrid2agrid_mod.o \
	fill_mass_grid2.o \
	fill_nmm_grid2.o \
	fpvsx_ad.o \
	gengrid_vars.o \
	genqsat.o \
	genstats_gps.o \
	gesinfo.o \
	get_derivatives.o \
	get_semimp_mats.o \
	get_tend_derivs.o \
	getprs.o \
	getstvp.o \
	getuv.o \
	getvvel.o \
	glbsoi.o \
	grdcrd.o \
	grid2sub.o \
	gridmod.o \
	gscond_ad.o \
	gsi_io.o \
	gsisub.o \
	guess_grids.o \
	half_nmm_grid2.o \
	init_commvars.o \
	intall.o \
	intdw.o \
	intjc.o \
	intlimq.o \
	intoz.o \
	intpcp.o \
	intps.o \
	intpw.o \
	intq.o \
	intrad.o \
	intgps.o \
	intrp2a.o \
	intrp3oz.o \
	intrppx.o \
	intrw.o \
	intspd.o \
	intsrw.o \
	intsst.o \
	intt.o \
	intw.o \
	jcdivtends.o \
	jcmod.o \
	jfunc.o \
	kinds.o \
	lagmod.o \
	linbal.o \
	looplimits.o \
	m_gsiCheck.o \
	m_gsiGuess.o \
	m_fvAnaGrid.o \
	mod_inmi.o \
	mod_strong.o \
	mod_vtrans.o \
	mp_compact_diffs_mod1.o \
	mp_compact_diffs_support.o \
	mpi_bufr_mod.o \
	mpimod.o \
	ncepgfs_io.o \
	nlmsas_ad.o \
	normal_rh_to_q.o \
	obs_para.o \
	omegas_ad.o \
	oneobmod.o \
	ozinfo.o \
	pcgsoi.o \
	pcp_k.o \
	pcpinfo.o \
	penal.o \
	plib8.o \
	polcarf.o \
	precpd_ad.o \
	prewgt.o \
	prewgt_reg.o \
	psichi2uv_reg.o \
	psichi2uvt_reg.o \
	q_diag.o \
	qcmod.o \
	qcssmi.o \
	radinfo.o \
	raflib.o \
	rdgrbsst.o \
	rdgstat_reg.o \
	read_airs.o \
	read_amsre.o \
	read_avhrr.o \
	read_avhrr_navy.o \
	read_bufrtovs.o \
	read_files.o \
	read_goesimg.o \
	read_goesndr.o \
	read_gps.o \
	read_guess.o \
	read_iasi.o \
	read_l2bufr_mod.o \
	read_lidar.o \
	read_modsbufr.o \
	read_obs.o \
	read_ozone.o \
	read_pcp.o \
	read_prepbufr.o \
	read_radar.o \
	read_ssmi.o \
	read_ssmis.o \
	read_superwinds.o \
	read_wrf_mass_files.o \
	read_wrf_mass_guess.o \
	read_wrf_nmm_files.o \
	read_wrf_nmm_guess.o \
	regional_io.o \
	ret_ssmis.o \
	retrieval_amsre.o \
	retrieval_mi.o \
	rfdpar.o \
	rsearch.o \
	satthin.o \
	setupbend.o \
	setupdw.o \
	setupoz.o \
	setuppcp.o \
	setupps.o \
	setuppw.o \
	setupq.o \
	setuprad.o \
	setupref.o \
	setuprhsall.o \
	setuprw.o \
	setupspd.o \
	setupsrw.o \
	setupsst.o \
	setupt.o \
	setupw.o \
	sfc_model.o \
	simpin1.o \
	simpin1_init.o \
	smooth_polcarf.o \
	smoothrf.o \
	smoothwwrf.o \
	smoothzrf.o \
	specmod.o \
	spectral_transforms.o \
	sst_retrieval.o \
	statsconv.o \
	statsoz.o \
	statspcp.o \
	statsrad.o \
	stop1.o \
	stpcalc.o \
	stpdw.o \
	stpjc.o \
	stplimq.o \
	stpoz.o \
	stppcp.o \
	stpps.o \
	stppw.o \
	stpq.o \
	stprad.o \
	stpgps.o \
	stprw.o \
	stpspd.o \
	stpsrw.o \
	stpsst.o \
	stpt.o \
	stpw.o \
	strong_bal_correction.o \
	strong_baldiag_inc.o \
	strong_fast_global_mod.o \
	strong_slow_global_mod.o \
	stvp2uv_reg.o \
	sub2grid.o \
	support_2dvar.o \
	tendsmod.o \
	tintrp2a.o \
	tintrp3.o \
	tpause.o \
	tpause_t.o \
	transform.o \
	tstvp2uv_reg.o \
	turbl.o \
	turbl_tl.o \
	turbl_ad.o \
	turblmod.o \
	tv_to_tsen.o \
	unfill_mass_grid2.o \
	unfill_nmm_grid2.o \
	unhalf_nmm_grid2.o \
	update_guess.o \
	zrnmi_mod.o \
	wrf_binary_interface.o \
	wrf_netcdf_interface.o \
	write_all.o \
	write_bkgvars_grid.o \
	wrwrfmassa.o \
	wrwrfnmma.o \
	blockIO.o

# ----

LIB =   libgsi.a

# ------------------------
# Call compiler and linker
# ------------------------

##all : $(LIB) $(EXE_FILE)
all : $(LIB) gsi.exe


$(LIB):	 $(OBJS)
	echo $(OBJS)
	$(AR) -ruv $(LIB) $(OBJS)

#$(EXE_FILE) : $(OBJS) $(LIB) gsimain.o
#	$(LD) $(LDFLAGS) -o $@ gsimain.o $(OBJS) $(LIBS)

gsi.exe:  $(OBJS) $(LIB) gsimain.o
	$(F90) $(LDFLAGS) -o gsi.exe gsimain.o libgsi.a $(LIBcrtm) $(LIBsfcio) $(LIBsigio) $(LIBw3) $(LIBbacio) $(LIBbufr) $(LIBsp) $(LIBgfsio) $(LIBwrf) $(LIBnetcdf) $(LIBsys)
	cp gsi.exe $(COREBIN)

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

include Makefile.dependency

clean:
	rm -f *.o *.exe $(LIB)

