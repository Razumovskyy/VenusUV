# General information about Venus_UV and Credits:
This project is dedicated to calculations of radiative transfer in Venus' atmosphere in ultraviolet region.

**Maintainer of the project**: 

- Mikhail Razumovskiy, Moscow Institute of Physics and Technology, Applied Infrared Spectroscopy Laboratory
(https://www.researchgate.net/profile/Mikhail-Razumovskiy)

**Collaborators and contributors**:

- Boris Fomin, Central Aerological Observatory, Moscow
(https://www.researchgate.net/profile/Boris-Fomin)


- Alexander Rodin, Moscow Institute of Physcis and Technology, R&D Center for Environmental Monitoring
(https://www.researchgate.net/profile/A-Rodin-4)


# What do you need to run the project
 
- codeblocks (preferable) with GNU Fortran Compiler - to run existing .cbp project, otherwise you need manually add files and create project in other IDE.
- Python 3.8 (or may be lower, but not Python 2) for running auxilary scripts.

# Insructions and description of the project

## Description of directories

-  All the files except Readme.md and License are located in **./venus_uv_prj**
-  Directory **./venus_uv_prj/Solar_Irradiance** contains data files with solar spectral irradiances from various sources.
-  Directory **./venus_uv_prj/Atmosph_Models** contains data files with atmospheric profiles: temperature, pressure, density. The data encompasses individual gaseous features as well as cumulative features from VIRA (Venus International Reference Atmosphere).

## Data and sources

### Mixing ratios of gaseous constituents

Data files with mixing ratios of gaseous constituents have extensions .hq and are located in directories named by gases in **./venus_uv_prj/Atmosph_Models**. From 0 to 80 km data is according to Haus2015 and Tsang2008. From 80 to 140 km data is roughly interpolated if mixing ration is constant (for all gases except SO2 and CO). In the case of SO2 and CO data from 80 to 140 km is still questioned.

## File formats

## Examples and some results

## Main output files (for up-to-date version)
**./venus_uv_prj/00&75clo.50500-80000** height, exact downward flux (0 deg), exact downward flux (75 deg), exact upward flux (0 deg), exact upward flux (75 deg) (?)
**./venus_uv_prj/K_FDO_FUP_FUPKD.00deg** (or 75 deg): height, number of molecules through the ray, sigma, exact downward flux, exact upeard flux, approximate upward flux (for one zenith angle)
**./venus_uv_prj/FDO_FUPex_FUPap_Qex_Qap.A&B**: height, exact downward flux, exact upward flux, approximate upward flux (for each zenith angle)

# License

This project is supplied with GNU general public license v3.0
