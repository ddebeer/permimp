## Submission
This is a new version after a bugfix


## Fixes
1. Bug fix in permimp(..., AUC = TRUE)

2. doi of Debeer and Strobl (2020) is now added to the description.


## Test environments
* local windows x86_64-w64-mingw32, R 4.0.3
* OS X (on travis-ci), release
* linux xenial (on travis-ci), oldrel, release, devel
* windows (on AppVeyor), release


## R CMD check results
0 errors | 0 warnings | 1 note


NOTE:

Found the following (possibly) invalid URLs:
  URL: http://www.biomedcentral.com/1471-2105/14/119 (moved to https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-14-119)
    From: man/permimp.Rd
    Status: 200
    Message: OK
  URL: http://www.biomedcentral.com/1471-2105/9/307 (moved to https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-9-307)
    From: man/permimp.Rd
    Status: 200
    Message: OK
  URL: https://rdcu.be/b5CrH (moved to https://link.springer.com/epdf/10.1186/s12859-020-03622-2?sharing_token=inHQK8hnyRIQru7bHcXDDW_BpE1tBhCbnbw3BuzI2RN55W-VwYEoqzfjHjtSVy84ZRZyTXjG4P7uy4HmRSDiEF1RjTISysTSpUnXJ8dlbSwRAIc8DAlBweyqOhuW0y7WvNkjO8RP1q2v-9DBACU0MtNKm1sRdy0GDZVEC5E2wGE%3D)
    From: man/permimp.Rd
    Status: 200
    Message: OK
    
    
Found the following URLs which should use \doi (with the DOI name only):
  File 'permimp.Rd':
    http://dx.doi.org/10.1007/s11222-012-9349-1
    

=> the links are copied from the documentation of the varimp function in the party package. They worked when I checked them.


## Downstream dependencies
There are currently no downstream dependencies.