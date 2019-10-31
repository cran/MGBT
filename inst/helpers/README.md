README (path ./MGBT/inst/helpers/README.md)

Author:           William H. Asquith
Point of contact: William H. Asquith (wasquith@usgs.gov)

------------------------------------------------------------------------------------------

The files herein are used to provide representative working notes of the author in the development of some tools for data-mining study of the U.S. Geological Survey (USGS) peak-streamflow database. The file manifest follows:

1. `testsites.pdf` --- This file is a portable document format (PDF) graphic from the `MGBT::plotPeaks()` function call made through the _R_ script `vispks(R).txt` where the `.txt` extension is used to keep `R CMD check MGBT` from complaining about _R_ sources in the package that are not in the `R/MGBT` directory.

2. `testsites.txt` --- This file is a short list of various USGS streamflow-gaging stations for which the peaks will be retrieved using the **dataRetrieval** package and plotted by the **MGBT** package in file `testsites.pdf`. The code comments therein provide further details. These sites are somewhat randomly chosen and mostly in Texas, however, some thought in the choice is made to stress the `MGBT::plotPeaks()` alogrithms and the `MGBT()` itself.

3. `vispks(R).txt` --- This is an _R_ script. (See no. 1.)
