## R CMD check results


## Submission

This is a new version which includes:

* The option of using parallel processing (implemented via the pbapply package).
* A new way of setting random seeds per tree to make the results reproducible 
    also when parallel processing is used.
* continuous integration via github actions
* a package website made using pkgdown



## Test environments

* local windows x86_64-w64-mingw32/x64, R 4.4.2
* windows via winbuilder, R4.4.3 alpha
* windows via winbuilder, R4.5.0 alpha
* via Rhub: 
    * linux; R-*; ubuntu-latest on GitHub
    * m1-san; R-*; macos-latest on GitHub, ASAN + UBSAN on macOS
    * macos; R-*; macos-13 on GitHub
    * macos-arm64; R-*; macos-latest on GitHub
    * windows; R-*; windows-latest on GitHub
    * ubuntu-next; R-4.5.0 (2025-03-25 r88054); Ubuntu 22.04.5 LTS
    * ubuntu-release R-4.4.3 (2025-02-28); Ubuntu 22.04.5 LTS
* via R-CMD-check github actions
    * macos-latest; R release
    * windows-latest; R release
    * ubuntu-latest; R devel    
    * ubuntu-latest; R release    
    * ubuntu-latest; R oldrel-1        
    
    
## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.


## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN 
and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

