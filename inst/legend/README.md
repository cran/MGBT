README (path ./MGBT/inst/legend/README.md)

Author:           William H. Asquith
Point of contact: William H. Asquith (wasquith@usgs.gov)

------------------------------------------------------------------------------------------

This directory contains reference information and portable document format (PDF) files related to the legend or explanation of the `MGBT::plotPeaks()` function. The file manifest follows:

1. `legend_camera.pdf` --- This file is PDF and is the "camera ready" version of the raw output (`peaks_for_legend.pdf`) from `makePeakLegend(R).txt` that is an _R_ script but ends in a `.txt` convention to keep the `R CMD check MGBT` on the package from complaining that _R_ source code exists outside of the `./MGBT/R/` directory.

2. `makePeakLegend(R).txt` --- This file is an _R_ script. (See no. 1.)

3. `peaks_for_legend.pdf` --- The output from `makePeakLegend(R).txt`.

4. `peaks_fot_legend.txt` --- The input for `makePeakLegend(R).txt` to make `makePeakLegend(R).txt`. And the peaks therein and the attendant peak discharge qualification codes have been manually adjusted by the author from a real U.S. Geological Streamflow (USGS) gaging station in order to comprehensively demonstrate most the the possible permutations that could be seen across the entire USGS peak-streamflow database. The sole purpose of this file is to have a source basis on which the `peaks_for_legend.pdf` could be created and then manually edited to create `legend_camera.pdf`.
