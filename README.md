# utilities
Repository for useful functions custom-built to work with MFE database

Be sure you have up-to-date versions of the scripts for interfacing with the databases from the MFEh2o/db repository

Functions included in repo:

MFEmetab.R contains a single function:
1. `mfeMetab()` calculates lake metabolism parameters using Chris Solomon's 
for data in the sensor database
*to use this function you'll also need to source the dbUtil.R prior to use

checks.R contains 6 functions, which are now documented fully in `man/", so I'm not going go to in depth here.

QCfuns.R has a bunch of functions that Randi wrote. I haven't gone through them all yet. See more details about what they do [here](https://github.com/MFEh2o/db/issues/99)
