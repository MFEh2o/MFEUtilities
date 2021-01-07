# utilities
Repository for useful functions custom-built to work with MFE database

Be sure you have up-to-date versions of the scripts for interfacing with the databases from the MFEh2o/db repository

Functions included in repo:

MFEmetab.R contains a single function:
1. `mfeMetab()` calculates lake metabolism parameters using Chris Solomon's 
for data in the sensor database
*to use this function you'll also need to source the dbUtil.R prior to use

checks.R contains 6 functions:
1. `dfCheck()` checks if an object is a data frame. Silent if it is; throws an error if it isn't.
2. `colsCheck()` checks whether specified columns are present in the data frame passed to the function. Also calls `dfCheck()` to make sure the object passed is indeed a data frame.
3. `dateCheck()` checks whether the `dateSample` date matches the date in `sampleID`. If there are mismatches, returns the row indices of the mismatches. If no mismatches, returns a success message and nothing else (i.e. NULL).
4. `timeCheck()` checks whether the `dateTimeSample` time matches the time in `sampleID`. If there are mismatches, returns the row indices of the mismatches. If no mismatches, returns a success message and nothing else (i.e. NULL).
5. `dateSampleCheck()` checks whether the date in `dateSample` matches the date in `dateTimeSample`. If there are mismatches, returns the row indices of the mismatches. If no mismatches, returns a success message and nothing else (i.e. NULL).
6. `dateSetCheck()` checks whether the date in `dateSet` matches the date in `dateTimeSet`. If there are mismatches, returns the row indices of the mismatches. If no mismatches, returns a success message and nothing else (i.e. NULL).
