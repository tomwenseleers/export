## Test environments

* **Windows**

R version 3.4.4 (2018-03-15)

Platform: x86_64-w64-mingw32/x64 (64-bit)

Running under: Windows 7 x64 (build 7601) Service Pack 1

* **Ubuntu**

R version 3.4.2 (2017-09-28)

Platform: x86_64-pc-linux-gnu (64-bit)

Running under: Ubuntu 17.10

## Resubmission


This is the 4th submission.


**Changes since 3rd submission :**

* Changed the way temporary file names are created. We received exactly 
  the same comment as last submission. We updated the example script, but 
  forgot to update the '*.Rd' help file. Our sincere apologies for that.
  
* Meanwhile, a user asked us to add a minor feature when exporting tables. 
  p-values in the exported tables can now be (optionally) trimmed (eg '<0.001').

**Changes since 2nd submission :**


* Changed the way temporary file names are created in the 
  examples. Previously I used the syntax:
  """
  dir <- tempdir()
  filen <- paste0(dir,"\\\\ggplot")
  """
  For portable code, I replaced all instances of the above 
  statement with:
  """
  dir <- tempdir()
  filen <- paste0(dir,"ggplot")
  """


**Changes since 1st submission :**

* Changed function names in the DESCRIPTION by adding  '()'.

* Reorganized the examples and replaced every '\dontrun' 
  satement by the '\donttest' statement. This is needed 
  because some examples require screen devices to be 
  opened which is not allowed for examples (R CMD CHECK 
  gives following error when the example is executed 
  without '\donttest': "screen devices should not be used 
  in examples etc"").
  
* Files in the examples are now written in a temporary folder
  (using tempdir()) instead of the package directory in order
  to comply to CRAN policy.

## R CMD check results

**For both Ubuntu and Windows: **

R CMD check results

0 errors | 0 warnings | 0 notes

R CMD check succeeded


## Downstream dependencies

* No issues found for Windows nor Ubuntu (using devtools::revdep_check())