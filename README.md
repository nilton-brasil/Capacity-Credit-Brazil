
# Capacity Credit Brazil

<!-- badges: start -->
<!-- badges: end -->

This repository combine functions developed by CPLEN with available functions in REPRA (NREL) package to make possible calculate capacity value and capacity credit for wind and solar resources in the Brazilian system. 

REPRA package was developed by NREL and it is available at  https://github.com/NREL/repra/blob/master/README.md. Although enormously useful, REPRA has some limitations such as maitenance lack and the focus on thermal systems. We have developed this repository to overcome these limitaions and extend the REPRA approach to thermo-hydro systems.

The main difference between predominantly hydro systems from thermal ones is the variable capacity from hydro systems. This requires that to calculate capacity contribution from variable sources we are able to update not only load and variable sources production but also avaliable capacity from the dispatchable sources.

There is one difficulty that may be encoutered when running the program. Newer versions of data.table may not work adequately. To overcome such a problem it is possible to use older versions of the data.table package or to manually source in RStudio the scripts for the format_timedata function (https://rdrr.io/github/NREL/repra/src/R/timedata.R) and for the has_no_name function (https://rdrr.io/github/NREL/repra/src/R/auxiliary.R). A more robust solution to this issue will be adressed in the future.

Since Capacity Credit Brazil incorporates many functions developed whithin REPRA (NREL), its license is reproduced here (https://github.com/NREL/repra/blob/master/LICENSE):


REPRA Copyright (c) 2014-2015 Alliance for Sustainable Energy, LLC. All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

3. The name of the copyright holder(s), any contributors, the United States Government, the United States Department of Energy, or any of their employees may not be used to endorse or promote products derived from this software without specific prior written permission from the respective party.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER(S) AND ANY CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER(S), ANY CONTRIBUTORS, THE UNITED STATES GOVERNMENT, OR THE UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


