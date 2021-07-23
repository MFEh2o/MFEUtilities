# MFEUtilities

This repository is an **R package**. It was created by Kaija Gahm in 2020 and 2021. 

Unfortunately, I (Kaija) wasn't able to finalize the package before leaving. So here, I'm going to outline what I've done so far, my reasons for creating an R package (instead of just a collection of scripts), and some instructions for how to carry this forward if desired.

For a video introduction to the package structure and the basics of adding functions and maintaining the package, see ['MFEUtilities Tutorial.mp4'](https://app.box.com/file/837329933072), which is on Box under 'MFE/Database/Database Management'.

## Background

When I arrived at MFE, there were two high-level GitHub repositories for interfacing with the MFE database.
The main repository was (and still is) [db](https://github.com/MFEh2o/db). **db** contains the main [issue tracker](https://github.com/MFEh2o/db/issues) for the MFE database.
But it also contains some R scripts and functions that are used for interacting with the database. For example, "dbUtil.R" contains the `dbTable` and `dtTableList` functions, as well as some others, which I ended up using almost every day for reading database tables into R.
Standard practice has always been to make sure to download and `source()` the scripts containing those functions before working, in order to then use the functions in your own code. This is fine, if a little cumbersome; among other things it forces you to either source directly from GitHub (which many people don't know how to do, or don't do), or to keep a local copy of the script file(s) in your own repo, which causes problems if anyone wants to update those functions. 

In addition to functions like those in dbUtil.R, which are frequently-used utility functions, the "db" repository also contains functions like `mfeMetab()`, a long and complicated function written by Chris in 2009 (and updated many times since then), which performs lake metabolism calculations. I figured it would be equally desirable for functions like that to be easily accessible, without having to download a separate R script.

## This Package

So, I created MFEUtilities as an R package. It's still a GitHub repo, yes, but it has a particular structure that allows it to be officially "built" as a package and then loaded into R, similarly to how you might install and load any other R package. This package isn't on CRAN, so you can't install it with the usual `install.packages()` command, but there's another way to do it (see **Installation** below).

This package contains some functions that are not available elsewhere, but it also contains *copies* of functions from the "db" repository, with some changes to their formatting to conform to the package structure. I've left the original files in the "db" repository alone for now, so they can still be used the old-fashioned way.

**Ideally**, this package would be updated and maintained. All researchers working with the MFE database would be able to install and load the package, and then use functions from it without downloading and `source`ing any individual R scripts. But because I'm working on this at the eleventh hour when I'm about to leave, I have also left the original files in place. I don't have confidence that the package as it currently exists will work, so feel free to fall back on the scripts if necessary. This package can be left as an optional project for the next person!

Note in particular to Chris and Stuart: it looks like you are already in the habit of documenting your functions! Most of the ones I found contained substantial headers with details about what the function does and what each parameter means. In most cases, all I had to do was change the formatting of the documentation--instead of regular comments, I used `roxygen`-style comments (in the scripts, these are marked with a `#'`). This allows `roxygen` and `devtools` to be used to create official package documentation, which you can access with the usal `?dbTable` or `help(dbTable)` commands when the package is loaded.

## Installation Instructions

1. Install the package `devtools` (if you don't already have it installed). You can do this in the usual way, by typing `install.packages("devtools")` in the console.

2. Install this package, using the `install_github()` function from `devtools`. Like this:

`devtools::install_github("MFEh2o/MFEUtilities")`

You should see a long message like this:
```
Downloading GitHub repo MFEh2o/MFEUtilities@HEAD
✓  checking for file ‘/private/var/folders/sj/b05g_2wn5rvfkw9cfjcf5h8c0000gn/T/RtmpZOIGLu/remotesaa2e42583d43/MFEh2o-MFEUtilities-c8db9f9/DESCRIPTION’ (552ms)
─  preparing ‘MFEUtilities’:
✓  checking DESCRIPTION meta-information ...
─  checking for LF line-endings in source and make files and shell scripts
─  checking for empty or unneeded directories
   Omitted ‘LazyData’ from DESCRIPTION
─  building ‘MFEUtilities_0.0.0.9000.tar.gz’
   
* installing *source* package ‘MFEUtilities’ ...
** using staged installation
** R
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (MFEUtilities)
```

3. Load the package. `library(MFEUtilities)`

4. Now the package is loaded! You should be able to use the functions it contains. As a test, for example, try typing `?dbTable` to access the official help file for the `dbTable()` function!

To see a list of all the functions included in the package and their descriptions, do `help(package = "MFEUtilities")`.

## Maintaining and Updating the Package

This package is an R project, so the best way to start working on it is to [clone the repository](https://happygitwithr.com/new-github-first.html#new-rstudio-project-via-git) to your computer and open it through the .Rproj file.

Writing code for R packages is a little different than writing other R code. The functions are stored in .R scripts, but you'll notice that each function contains lines with special lines for comments, descriptions, parameters, etc. that may not be familiar to you. There are other commands that you'll need to use to properly document and check the package if you're going to update it.

### Resources for Package Development

Here are some resources for getting familiar with R package development if you're new to it. This might seem intimidating. But I will note that I've created this package with very little prior knowledge--I only learned how to make packages a couple months ago. It's not actually as hard as it seems, and I've already done most of the setup!

You will definitely need to install `roxygen2` and `devtools` in order to do any package development.

```
install.packages("roxygen2")
install.packages("devtools")
```

I've created a video introduction to the package structure and the basics of adding functions and maintaining the package, see ['MFEUtilities Tutorial.mp4'](https://app.box.com/file/837329933072), which is on Box under 'MFE/Database/Database Management'.

[R Packages](https://r-pkgs.org/index.html) is a great, accessible resource written by Jenny Bryan and Hadley Wickham. If you're trying to get your head around an already existing package like this one, start from the **Package components** section. 
[Writing an R package from scratch](https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/) by Hilary Parker. 
[Writing R Extensions](https://cran.r-project.org/doc/manuals/r-patched/R-exts.html) is a little more technical, but it has answers to a lot of common questions.
[Developing Packages with the RStudio IDE](https://support.rstudio.com/hc/en-us/articles/200486488-Developing-Packages-with-the-RStudio-IDE) is an accessible resource from RStudio that also links to several of the above.

## Issues/bug reports

If you find problems with the functions contained in this package, or if you have ideas about how to fix them, you can create new issues over at [the issue tracker](https://github.com/MFEh2o/MFEUtilities/issues).
