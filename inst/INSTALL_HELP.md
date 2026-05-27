# README (path ./MGBT/inst/INSTALL_HELP.md)

#### Author:           William H. Asquith
#### Point of contact: William H. Asquith (wasquith@usgs.gov)

***
***

This file describes one of the more "raw" methods of installing the **MGBT** _R_ package from its original sources or from the download of a tagged version of the repository. This is an obviously helpful tutorial in the event that non-CRAN installation through the **remotes** package does not function properly. (See also the top-level `README.md` of this package.)

The instructions here depict the use of **RStudio** (accessed November 26, 2019, at https://rstudio.com/) on the Windows platform for **MGBT** installation. A note is made that the actual installation simply uses the `install.packages()` function from the standard **utils** package.

***

1. The process begins by downloading the `.tar.gz` format of the source code from the repository, which in this case is the "master" of the repository as shown in the screenshot that follows. Other `.tar.gz` versions could come from any "tagged" version of the repository. The author notes that downloading the default `.zip` format, even though that is a common archive format on standard PCs, does not appear to work.

<img src='inst/www/step1_src_MGBT_install.png' height='320' align="middle" />

***

2. After the file is downloaded, which is titled `MGBT-master.tar.gz`, from the **RStudio** interface, the _Tools --> Install packages_ menu option is selected. Then the _Install from:_ is switched, if needed, to the "Package Archive File (.tgz, .tar.gz)" option. The switch may or may not then spawn a file selector dialog box, but such dialog can be also triggered by the _Browse..._ button. The user needs to navigate their file system to the location of the `MGBT-master.tar.gz` and choose that file. The following screenshot summarizes this step.

<img src='inst/www/step2_src_MGBT_install.png' height='460' align="middle" />

***

3. Inspect the installation process for success and test loading the **MGBT** package. The following screenshot summarizes this "step." It is seen that that the `install.packages()` function was appropriately configured. The author notes the warning message about the **Rtools** executable, which evidently is code specific to the Windows platform (accessed on November 26, 2019, at https://cran.r-project.org/bin/windows/Rtools/). The warning message appears harmless, which might be the case because the **MGBT** package does have compiled code, just native _R_.

<img src='inst/www/step3_src_MGBT_install.png' height='977' align="middle" />
