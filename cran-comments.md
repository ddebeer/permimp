## Resubmission
This is the second submission to CRAN, after a request for three fixes.

## Fixes

1. "Version contains leading zeroes""
version is now 1.0.0

2. References describing describing the methods are added to the description
Debeer and Strobl (2020) is accepted, but does not yet have a doi.

3. "changing the user's par() settings"
The following lines were added to the plot method

    opar <- par(no.readonly =TRUE)       # code line i
    on.exit(par(opar))                   # code line i+1


## Test environments
* local windows x86_64-w64-mingw32, R 4.0.1
* OS X (on travis-ci), release
* linux xenial (on travis-ci), oldrel, release, devel
* windows (on AppVeyor), release

## R CMD check results
0 errors | 0 warnings | 1 note

New submission

## Downstream dependencies

There are currently no downstream dependencies.