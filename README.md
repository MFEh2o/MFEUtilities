# MFE_Utilities

This repository is an **R package**. It was created by Kaija Gahm in 2020 and 2021. 

Unfortunately, I (Kaija) wasn't able to finalize the package before leaving. So here, I'm going to outline what I've done so far, my reasons for creating an R package (instead of just a collection of scripts), and some instructions for how to carry this forward if desired.

## Background

When I arrived at MFE, there were two high-level GitHub repositories for interfacing with the MFE database.
The main repository was (and still is) [db](https://github.com/MFEh2o/db). **db** contains the main [issue tracker](https://github.com/MFEh2o/db/issues) for the MFE database.
But it also contains some R scripts

Be sure you have up-to-date versions of the scripts for interfacing with the databases from the MFEh2o/db repository

Functions included in repo:

MFEmetab.R contains a single function:
1. `mfeMetab()` calculates lake metabolism parameters using Chris Solomon's 
for data in the sensor database
*to use this function you'll also need to source the dbUtil.R prior to use

checks.R contains 6 functions, which are now documented fully in `man/", so I'm not going go to in depth here.

QCfuns.R has a bunch of functions that Randi wrote. I haven't gone through them all yet. See more details about what they do [here](https://github.com/MFEh2o/db/issues/99)
