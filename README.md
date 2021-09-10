# General information about Venus_UV and Credits:
This project is dedicated to creating correlated-k models of radiative transfer in Venus' atmosphere. Spectral region of interest is
0.125-1000 μm. Because of evident difficulties with visible and IR region (line shapes, spectral databases), we firstly concentrated on taking account of UV region (125 - 400 nm). Refernce radiative transfer model is based on line-by-line procedures and Monte Carlo simulations to obtain height-dependend fluxes and temperature changing rates. We focus on studing lower and middle atmosphere (0-100 km), but modeling range of heights is prolonged upward to 140 km.

This README is mainly contained with two sections: 

I. Software section: instructions how to build and run, I/O formats, files and directories description and some examples, etc. 

II. Scientific part: explanation of underlying theory, models description, links to sources of data and to science papers, etc.

**Maintainer of the project**: 

- Mikhail Razumovskiy, Moscow Institute of Physics and Technology, Moscow
(https://www.researchgate.net/profile/Mikhail-Razumovskiy)

**Collaborators and contributors**:

- Boris Fomin, Central Aerological Observatory, Moscow
(https://www.researchgate.net/profile/Boris-Fomin)


- Alexander Rodin, Moscow Institute of Physcis and Technology, R&D Center for Environmental Monitoring
(https://www.researchgate.net/profile/A-Rodin-4)


# I. Software section

## What do you need to build and run the project
 
- codeblocks (preferable) with GNU Fortran Compiler - to run existing .cbp project, otherwise you need manually add files and create project in other IDE.
- Python 3.8 (or may be lower, but not Python 2) for running auxilary scripts.

## Instructions

## File formats

## Examples

# II. Scientific section

## Scientific resources

### Mixing ratios of gaseous constituents

Data files with mixing ratios of gaseous constituents have extensions .hq and are located in directories named by gases in **./venus_uv_prj/Atmosph_Models**. From 0 to 80 km data is according to Haus2015 and Tsang2008. From 80 to 140 km data is roughly interpolated if mixing ration is constant (for all gases except SO2 and CO). In the case of SO2 and CO data from 80 to 140 km is still questioned.

# License

This project is supplied with GNU general public license v3.0
