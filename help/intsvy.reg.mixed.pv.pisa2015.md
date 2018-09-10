# `intsvy.reg.mixed.pv.pisa2015`: Mixed model regression analysis with plausible values, for PISA 2015 data

## Description


 intsvy.reg.mixed.pv.pisa2015 fits linear mixed models via maximum likelihood, using plausible values and replicate weights from PISA 2015 datasets.


## Usage

```r
  intsvy.reg.mixed.pv.pisa2015(x, pvlabel, re_form, by, 
                data, std=FALSE, export = FALSE, 
                name = "output", folder = getwd(), 
                config = intsvy:::pisa2015_conf,
                parallel = TRUE, ncores = detectCores()-1)
```


## Arguments

Argument      |Description
------------- |----------------
```pvlabel```     |      The label corresponding to the achievement variable, for example, "SCIE" for PISA science performance. 
```x```     |      Data labels of independent variables. Specify as a vector of character strings e.g. `c("ESCS", "CULTPOSS")`  
```re_form```     |      Character string specifying the entire random effect structure, as would be specified in a standard lmer model (e.g. `re_form = "(1|CNTSCHID) + (1|CNT)"` or `re_form = "(ESCS|CNTSCHID)"` ). Note that random intercepts and random slopes can be fitted in this manner, but 
```by```     |      The label for the grouping variable, usually the countries (i.e., `by="CNT")` , but could be any other categorical variable. 
```data```     |      An R object, normally a data frame, containing the data from PISA 2015. 
```std```     |      A logical value. If TRUE standardised regression coefficients are calculated. 
```export```     |      A logical value. If TRUE, the output is exported to a file in comma-separated value format (.csv) that can be opened from LibreOffice or Excel. 
```name```     |      The name of the exported file. 
```folder```     |      The folder where the exported file is located. 
```config```     |      Object with configuration of the study. Defaults to intsvy:::pisa2015_conf 
```parallel```     |      If TRUE, the parallel package will be used to loop through model fitting. This can provide substantial speed improvements, but can limit machine performance whilst running. 
```ncores```     |      The number of cores on which to run parallel processes. Defaults to the number of cores-1, but it is advised that users specify a more appropriate number. 

## Value


 intsvy.reg.mixed.pv.pisa2015 prints a data.frame with regression results (i.e., coefficients, standard errors, t-values, R-squared) and stores  different regression output including replicate coefficients, variance within and between, and the regression data.frame in a list object of class "intsvy.reg".
 Unlike intsvy.reg.pv, residuals are not stored to avoid out of memory errors.
 An additional component is present in the results list - the 'raneff' component contains variance components for all random effects, averaged over all PVs. Note that this is derived from the 'varcor' component of the lmer model summary object. Names in this 'raneff' copmonent are simply pasted together from variable names in the 'varcor' object, so if you wish to further interpet them, you can fit one lmer model to a single PV and view the varcor component directly.
 Note that in fitting the lmer models within the function, weights are scaled by dividing through by the mean weight.


## See also

 intsvy.reg.pv


## Examples

```r 
    #get functions
    library(intsvy)
    source("https://upgitap001.ucles.internal/bentot/intsvyExtras/raw/master/functions/intsvy.reg.mixed.pv.pisa2015.R")

    #load the PISA data set from local copy
    load("pisadat.Rdata")

    #run analysis
    re_mod <- intsvy.reg.mixed.pv.pisa2015(pvlabel="SCIE", 
                x="ESCS", re_form = "(1|CNTSCHID)", 
                data=pisadat[pisadat$CNT == "GBR",], 
                config=intsvy:::pisa2015_conf, 
                parallel = TRUE, ncores = 2)
   #view results
   re_mod$reg
   re_mod$raneff

 ``` 

