# `intsvy.rho.pv.pisa2015`: Weighted correlation with plausible values with standard errors for PISA data

## Description

An exact copy of the functionality for the intsvy.rho.pv but with the added capability to calculate standard errors
in the event of the configuration having a BRR design.

For descriptions of the input parameters see help for the intsvy.rho.pv function.

Note that this function is only intended for use with PISA data (as opposed to TIMSS or PIRLS).

## See also

intsvy.rho.pv

## Examples

```r 

#get functions
library(intsvy)
source("https://raw.githubusercontent.com/CambridgeAssessmentResearch/intsvyExtras/master/functions/intsvy.rho.pv.pisa2015.R")

#load the PISA data set from local copy
load("pisadat.Rdata")

#correlation of ESCS with reading ability in GBR
intsvy.rho.pv.pisa2015(variable="ESCS", pvlabel="READ"
	,data=pisadat[pisadat$CNT=="GBR",])

#correlation of ESCS with science ability in GBR split by gender
intsvy.rho.pv.pisa2015(variable="ESCS", pvlabel="SCIE"
	,by="ST004D01T",data=pisadat[pisadat$CNT=="GBR",])

``` 
