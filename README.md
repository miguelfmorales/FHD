Fast Holographic Deconvolution (FHD)
===

FHD notes – commonly used commands

Contents:

1 Overview 

2 Installation instructions

3 Commonly used commands

3a Program control

3b Gridding

3c Deconvolution

3d Output


1 Overview 
FHD is an imaging algorithm for radio interferometers. It uses the full complex gain for every baseline for gridding, and pre-computes the uv-plane covariance matrix (the Holographic Mapping function). In its current implementation, FHD follows a centroided matched pursuit algorithm for deconvolving point sources. While FHD does not refer back to the visibilities once they have been gridded to a dirty image, it is still properly classified as a visibility-based subtraction algorithm since sources are forward-modeled through the mapping function. 

Aside from the basic use of snapshot imaging, there are a number of extensions that expand its capabilities. Most notably, FHD can perform a joint deconvolution of any set of visibilities by regridding each snapshot to a common HEALPix mesh. This allows for long integrations, and even for joint deconvolution of data from different instruments. 

FHD is typically called by writing a script that is a wrapper for general_obs.pro. This script contains the full file paths to all of the uvfits files that will be used, the path to any ancillary catalog that will be used, and the desired paths where it will put the output. 

A good example script for the 128T MWA is test_128t.pro


2 Installation instructions

Please see the separate INSTALL.txt file for the latest installation instructions, including all of the needed libraries. If you encounter difficulty getting FHD to run on your platform, please edit and add to these instructions!


3 Commonly used commands

3a Program control options:

beam_recalculate=recalculate_all ; recalculate the beam model 

channel=121 ;Use only data with this center frequency channel. Used only in the top-level observation wrappers 

cleanup=0 ; Set to delete most of the .sav files when finished 

End_fi ; last file index of the supplied list to deconvolve (zero index)

export_images=1 ; If set, generate numerous output images and data files

Force_data; Force visibility data to be read from file, even if all the needed .sav files are already present

Force_no_data; Force visibility data to not be read from a file, even if needed.

grid_recalculate=recalculate_all ; recalculate (grid) the dirty images

mapfn_recalculate=recalculate_all ;recalculate the mapping function 

recalculate_all=1 ;All other ‘recalculate’ options default to recalculate_all

Silent ; suppress most status messages 

Skip_fi ; If supplied, skip the specified file index (may be a list)

Start_fi ; first file index of the supplied list to deconvolve (zero index) 

version=0 ; Add a version number to the output data directory. Used only in the top-level observation wrappers


3b Gridding options

Calibrate_visibilities ; Set to calculate new calibration solutions

Calibration_catalog_file_path ; catalog of point sources to use for calibration

dimension=1024. 	; Image size, in pixels, used during deconvolution 

double_precison_beam=0 	;force the beam model and mapping function to be computed using double precision

flag=0 ; Flag data based on per-tile and per-frequency standard deviation of the visibilities

FoV=90. ; Field of view to be imaged, in degrees. No anti-aliasing filter is currently used, so FoV should be twice the size of the region of interest

Freq_end ; If set, flag all frequencies above freq_end (in MHz)

Freq_start ; If set, flag all frequencies below freq_start (in MHz)

max_baseline=0. ;	Maximum baseline length to grid, in wavelengths

min_baseline=0. ;	Minimum baseline length to grid, in wavelengths

n_pol=2 ; Grid and deconvolve only n_pol polarizations. Default is to grid all existing. Hard-coded to grid in the order xx, yy, xy, yx

precess=1 ;	Set to apply precession corrections to the coordinates. J2000 are used throughout

Rephase_to_zenith=0 ;Set to correct a zenith observation that was re-phased to another location by another program

Tile_flag_list ; Numeric list of tiles to flag (Index starts at 1!)

Transfer_calibration ; File path of calibration solution to apply. Allowed formats are .sav files with an FHD cal structure, text files, and numpy arrays (not yet supported)
transfer_mapfn=0 ; use a specific mapping function (specified as a string containing the full file path) for deconvolution, and do not calculate a new one. If set to 1 and a list of uvfits files are given, it will calculate the mapping function for the first file and transfer that mapping function for the rest of the files. 


3c Deconvolution options

add_threshold=0.8 ; When fitting source components, fit all sources brighter than this fraction of the brightest pixel

Beam_threshold=0.05 ; Minimum primary beam threshold to use when constructing the source mask

gain_factor=0.15 ; “Clean” gain factor to use 

Galaxy_model_fit=0 ; read in and forward-model the Haslam map of diffuse galactic emission. Fit an overall amplitude and subtract from the dirty images prior to deconvolution of point sources

independent_fit=0 ;set to 1 to fit I, Q, (U, V) seperately. Otherwise, only I (and U) is fit

Local_max_radius=3 ; only fit a source component if it is at least Local_max_radius pixels away from a brighter source

Max_add_souces ; maximum number of sources to be fit in the same deconvolution iteration 

max_sources=10000.	; Maximum number of source components to be fit

Reject_pol_sources=0 ; mask bright pixels with high polarization

Scale_gain=0 ; Set to a value 0<scale_gain<1 to use a larger “clean” gain factor for high signal to noise sources

Sigma_cut=2 ; only include source components detected with signal to noise greater than sigma_cut. Also used when condensing components to sources after deconvolution

Smooth_width=7; width of median filter to use to highlight point sources


3d Output options

Combine_healpix; Set to regrid all supplied snapshots to HEALPix after deconvolution, and create combined maps. Also creates a source list structure 

image_filter_fn='filter_uv_radial' ;name of a function to call to apply different weights to the uv plane when generating output images. NOT used during deconvolution. Current options are ‘’ (empty string) for natural weighting, ‘filter_uv_uniform’ for radially-averaged uniform weighting, ‘filter_uv_radial’ for radially-averaged weighting 

N_avg ; number of fine frequency channels to average together when exporting data cubes for power spectrum estimation

no_ps=1 ;By default, .png, .ps, and .fits copies of the output images are saved. Set no_ps to save only .png and .fits

pad_uv_image=2.	; If set, pad the UV plane with zeroes by this scale factor, to increase the resolution of the output images. Ignored during deconvolution

ps_export=0 ;Set to generate output healpix cubes compatible with Bryna’s Power Spectrum code

Split_ps_export; If set, splits the visibilities into separate cubes for even and odd time samples
