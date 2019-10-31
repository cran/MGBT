README (./MGBT/inst/sources/README.md)

Author:           William H. Asquith
Point of contact: William H. Asquith (wasquith@usgs.gov)

------------------------------------------------------------------------------------------

The sources files herein were acquired over a period of time from the late Tim A. Cohn
through written communication between William H. Asquith and John F. England. Several
sources are clearly related to low outliers and scripts for article generation.
Other sources `P3_***_***(R).txt` contain low outlier code but the bulk of which
is used for log-Pearson type III (LPIII), expected moments algorithm (EMA) computation
of peak-streamflow frequency. This extra code is not needed for the MGBT package
and IS NOT TO BE CONSIDERED CANONICAL OF LPIII-EMA-like computations! Please
feel invited to contact John F. England or William H. Asquith for further details about
LPIII-EMA computations.

The sources herein contained in the `*(R).txt` will never be eligible for updates.
They are locked down forever. Finally, the `(R).txt` in the file names is deliberate to
prevent _R_ during `R CMD check check MGBT` from throwing exceptions to _R_ code being
contained within the `./inst` subdirectory of the package.
