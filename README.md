# OWASA

Open Science to Support Local Water Security in Southern Africa

## Installation

You can install the OWASA R package as follows:
```r
remotes::install_github("obaezvil/OWASA")
```

In case that the download failed with the following error: *Error in utils::download.file*, you can try to increase the timeout for the downloads as follows and try again.
```r
options(timeout=500) 
remotes::install_github("obaezvil/OWASA")
```

To deploy the package in **shinyapps.io**....
