
[R][r] interface for for [MACRO][macro], a model of water 
flow and solute transport in macroporous soil, 
and its regulatory variant [MACRO In FOCUS][macroinfocus]. 
The [package][r_packages] provides functions to read, write 
and modify the model parameter files (`.par`-files). Parameter 
modifications are done either with low-level generic functions, 
or with high-level functions designed for specific parameters 
or groups of parameters. High-level functions are currently 
limited to modifications of the substance properties.

`rmacrolite` was written as an infrastructure for the R 
packages [macroutils2][] and [macrounchained][]

`rmacrolite` is derived from the R package `rmacro`, 
written by the author while at the Center for Chemical Pesticides 
([CKB][ckb]), Swedish University of Agricultural Sciences ([SLU][slu]), 
during the R&D project PERFORM (2013-2016; Funding: 
"Pour et Sur le Plan Ecophyto" (PSPE)). Compared to `rmacro`, 
the package has been significantly refactored, and there is 
no backward compatibility. Compared to `rmacro`, `rmacrolite` 
does not contain any MACRO-executable, and it uses an existing 
installation of MACRO or MACRO In FOCUS.

*   **Development status**: pre-release. Do not use for 
    production purpose, as the interface may still evolve if 
    needed.
    
*   **General information**: [DESCRIPTION](DESCRIPTION) 
    (including author(s), package-version, minimum R version 
    required, ...).
    
*   **Change log**: [NEWS](NEWS).
    
*   **Operating system**: `rmacrolite` will only work on 
    Windows, because MACRO is a Windows-only 
    program.
    
*   **License**: MIT License. See [LICENSE](LICENSE).

Either MACRO or MACRO In FOCUS or both need to be installed 
for using `rmacrolite` (see below).



Installation
============================================================

End-users should manually install the package from Windows 
binary package (a `.zip`-archive). The binary package 
provided on the website indicated below are presumably 
stable versions of the package, for release or pre-release 
(see status above).

Experienced users and developers may prefer to install the 
development version of the package, from GitHub. The later should not 
be seen as a stable version and may not work at all.

Before you install the package, check in the 
[DESCRIPTION](DESCRIPTION) file what is the minimum version 
of R needed to run this package (field "Depends", see 
"R (>= ...)"). As I don't have time to test the package on 
multiple R major versions, the package will generally require 
the latest major-release at the time of testing the latest 
(pre-)release of the package. If needed, the code can be 
loaded as an R-script instead of installed as a package (see 
below), and used on any R-version presumably compatible with 
the code.



Installing the package from Windows binaries
------------------------------------------------------------

Windows binary-installer (a `.zip` file) and source tar of the 
package (a `.tar.gz` file) can be downloaded from the following 
address: https://rpackages.julienmoeys.info/macrounchained/.

Choose both the binary-installer for `rmacrolite` and 
`macroutils2`.

Save the files to a local folder on your computer.

As I cannot guarantee the integrity of the website above, 
it is recommended to scan the file(s) with an antivirus, not 
least if you work in a corporation or a public institution.

Do not unpack the `.zip` archive (nor the `.tar.gz` archive) 
before installing the package.

See also: https://cran.r-project.org/doc/manuals/r-release/R-admin.html#Windows-packages 



_Method 1_ (R graphical user interface for Windows):

Open R graphical user interface for Windows. Click on the 
'Packages'-menu and select 'Install package(s) from local zip 
file...'. Select the package `.zip` binary package that you just 
downloaded, so that it is installed. 
When done, type `library("macroutils2")` and then 
`library("rmacrolite")` to check if the installation was 
successful.

_Method 2_ (install the zip binary package using the command 
line):

Open R command line prompt or R graphical user interface for 
Windows and type:

```
install.packages( 
    pkgs = "C:/path/to/binary/file/macroutils2_x.y.z.zip", 
    repos = NULL ) 
install.packages( 
    pkgs = "C:/path/to/binary/file/rmacrolite_x.y.z.zip", 
    repos = NULL ) 
```

where `C:/path/to/binary/file/` should be replaced by 
the actual path to the folder where the binary package was 
downloaded and `macroutils2_x.y.z.zip` and `rmacrolite_x.y.z.zip` 
by the actual file-names 
(`x.y.z` being the version number, different for different 
packages). It is important to use a 
slash (`/`) as path separator, or alternatively a double 
backslash (`\\`), instead of a single backslash (`\`; Windows 
standard), as the later is a reserved character in R.



_Method 3_ (install the source package using the command 
line:

Open R command line prompt or R graphical user interface for 
Windows and type:

```
install.packages( 
    pkgs = "C:/path/to/source/file/macroutils2_x.y.z.tar.gz", 
    repos = NULL, type = "source" ) 
install.packages( 
    pkgs = "C:/path/to/source/file/rmacrolite_x.y.z.tar.gz", 
    repos = NULL, type = "source" ) 
```

where `C:/path/to/source/file/` should be replaced by 
the actual path to the folder where the source package was 
downloaded and `macroutils2_x.y.z.tar.gz` and `rmacrolite_x.y.z.tar.gz` 
by the actual file-name 
(`x.y.z` being the version number). See above the remark on 
the path separator.



Installing the package from GitHub
------------------------------------------------------------

This method is reserved for experienced R users and developers. 
If you don't know what you are doing, choose one of the 
installation method above.

The development version of `macroutils2` is publicly available 
on [GitHub][github] ([here][macroutils2]), as well as the 
development version of `rmacrolite` ([here][rmacrolite])

to install the development version of the packages, you will 
need to install the package [devtools][] first. It is 
available on CRAN and can be easily installed. Simply type 
`install.packages("devtools")` in R command prompt. See also 
the package 
[README][https://cran.r-project.org/web/packages/devtools/readme/README.html] 
page.

You can then install the development version of `macroutils2` 
and `rmacrolite` by typing in R command prompt:

```
devtools::install_github("julienmoeys/macroutils2")
devtools::install_github("julienmoeys/rmacrolite")
```



Source the package as an R script instead of installing the package 
------------------------------------------------------------

It is also possible to source the package as an R-script instead 
of installing the package. This method has some drawbacks 
(help pages not available; R workspace polluted with many 
objects otherwise invisible to end-users; sourced-code may 
be accidentally modified by the user; lack of traceability), 
but may be useful to some users, for example with restricted 
possibilities to install new R packages, as a 
[bootstrap][https://en.wikipedia.org/wiki/Bootstrapping].

First, open the following `.r`-file 
https://raw.githubusercontent.com/julienmoeys/rmacrolite/master/R/rmacrolite.r 
and save it on your computer. This file contains the full 
R source code of the package.

Open R command line prompt or R graphical user interface for 
Windows and type:

```
library("macroutils2") 
source( "C:/path/to/file/rmacrolite.r" ) 
```

where `C:/path/to/file/` should be replaced by 
the actual path to the folder where the file was 
downloaded. See above the remark on the path separator.



About
============================================================

This package is a personal project of the author. It is not 
funded or supported by any corporation or public body.



Report issues
------------------------------------------------------------

Your are very welcome to report any (suspected) error or issue 
on this page: https://github.com/julienmoeys/rmacrolite/issues 

Before reporting on this page, try to reproduce the issue on 
a generic example that you can provide together with your 
issue.



User Support
------------------------------------------------------------

Currently, I cannot provide user-support for this tool. In 
my experience, many questions are general R questions 
rather than questions specific to my R packages, so it may 
help to get support from an experienced R programmer.



Credits
------------------------------------------------------------

The original version of this tool (rmacro`) was funded 
by the R&D project PERFORM (2013-2016; Funding: 
"Pour et Sur le Plan Ecophyto" (PSPE)), in a collaboration 
between [INRA][http://www.inra.fr/] Versailles-Grignon 
(Sabine Karen Lammoglia and Laure Mamy) and [SLU][slu] Uppsala 
(Julien Moeys, with a significant contribution from Mats Larsbo 
concerning the core MACRO model).



Disclaimer
------------------------------------------------------------

This tool is **not an official regulatory tool**.

It is **not endorsed** by [FOCUS DG SANTE][focusdgsante], 
[SLU/CKB][ckb] or the author's [employer][kemi]. 

It does not engage these institutions nor reflects 
any official position on regulatory exposure assessment. 

Indeed, the website of [FOCUS DG SANTE][focusdgsante] 
"_is the one and only definitive source of the currently 
approved version of the FOCUS scenarios and associated models 
and input files._". Thus, please refer to 
[FOCUS DG SANTE][focusdgsante] or to the competent authorities 
in each EU regulatory zone for guidance on officially accepted 
tools and methods.

As stated in the [LICENSE](LICENSE), the package is provided 
**without any warranty**.



[r]:                https://www.r-project.org/ "The R Project for Statistical Computing"
[r_packages]:       https://en.wikipedia.org/wiki/R_(programming_language)#Packages "R packages (Wikipedia)"
[macro]:            https://www.slu.se/en/Collaborative-Centres-and-Projects/centre-for-chemical-pesticides-ckb1/models/macro-52/ "MACRO 5.2 (SLU/CKB)"
[macroinfocus]:     https://esdac.jrc.ec.europa.eu/projects/macro "MACRO In FOCUS (FOCUS DG SANTE)"
[slu]:              https://www.slu.se/ "Swedish University of Agricultural Sciences (SLU)"
[ckb]:              https://www.slu.se/ckb "Centre for Chemical Pesticides (CKB)"
[macroutils2]:      https://github.com/julienmoeys/macroutils2 "R package macroutils2 (GitHub)"
[rmacrolite]:       https://github.com/julienmoeys/rmacrolite "R package rmacrolite (GitHub)"
[macrounchained]:   https://github.com/julienmoeys/macrounchained "R package macrounchained (GitHub)"
[macroutils]:       https://github.com/julienmoeys/macroutils "R package macroutils (GitHub)"
[github]:           https://github.com/ "GitHub development platform"
[focusdgsante]:     https://esdac.jrc.ec.europa.eu/projects/focus-dg-sante "FOrum for the Co-ordination of pesticide fate models and their USe"
[devtools]:         https://CRAN.R-project.org/package=devtools "R package devtools (CRAN)"
[cran]:             https://cran.r-project.org/ "The Comprehensive R Archive Network" 
[kemi]:             https://www.kemi.se/en "Swedish Chemicals Agency"