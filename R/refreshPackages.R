# Title: Refresh Packages
# Author: Brandon Edward Rose, Natalie Goulett
# Date edited: 2025/01/17

# I run these functions to keep dev packages up to date. Some package names have
#    been changed.

remotes::install_github("brandonerose/RosyREDCap",upgrade = "never")
remotes::install_github("brandonerose/RosyDB",upgrade = "never")
remotes::install_github("brandonerose/RosyApp",upgrade = "never")
remotes::install_github("brandonerose/RosyUtils",upgrade = "never")
remotes::install_github("brandonerose/RosyDev",upgrade = "never")
remotes::install_github("brandonerose/Rosyverse",upgrade = "never")
remotes::install_github("thecodingdocs/REDCapSync",upgrade = "never")
.rs.restartR()
