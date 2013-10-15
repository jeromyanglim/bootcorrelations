### Overview
I developed the `bootcorrelations` package (a) to help me learn about writing packages, and (b) write some functions that perform a bootstrap test of whether the average correlation in one set of variables differ from the correlation in another set. It is assumed that the two sets of variables are measured on the same cases.

The main function is `boot_mean_cor_diff()`.

### Requirements


### Installation:

    library(devtools)
    install_github('bootcorrelations', username='jeromyanglim')

Once installed run the following command to load the package

    library(bootcorrelations)
    
### Further information
See the Vigenette accompanying this package for more information.

    browseVignettes(package='bootcorrelations')