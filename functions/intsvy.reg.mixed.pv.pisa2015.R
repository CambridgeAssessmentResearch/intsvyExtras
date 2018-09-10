intsvy.reg.mixed.pv.pisa2015 <- function (x, pvlabel, re_form, by, data, std = FALSE, export = FALSE, 
                                          name = "output", folder = getwd(), config = intsvy:::pisa2015_conf,
                                          parallel = TRUE, ncores = detectCores()-1) 
{
  require(lme4)
  
  randvars <- unique(unlist(strsplit(gsub(re_form, 
                                          pattern = "[^[:alpha:]]", 
                                          replacement = " "), split = " ")))
  randvars <- sort(randvars[randvars != ""])
  
  pvnames <- paste("PV", 1:config$parameters$PVreps, pvlabel, sep = "")
  
  reg.pv.input <- function(x, pvlabel, data, std, config,
                           parallel, ncores) {
    
    if (any(sapply(data[x], function(i) all(duplicated(i))))) {
      results <- list(replicates = NA, residuals = NA, 
                      var.w = NA, var.b = NA, reg = NA)
      return(results)
    }
    
    if (config$parameters$weights == "BRR") {
      pvnames <- paste("PV", 1:config$parameters$PVreps, pvlabel, sep = "")

      
      data <- na.omit(data[c(x, pvnames, randvars, 
                             config$variables$weightFinal, 
                             paste0(config$variables$weightBRR, 1:config$parameters$BRRreps))])
      
      regform <- lapply(pvnames, function(i) paste(i, " ~ ", 
                                                   paste(x, collapse = " + "),
                                                   " + ",
                                                   re_form))
      if (std) {
        data <- cbind(scale(data[c(pvnames, x)]), data[!names(data) %in% c(pvnames, x)])
      }
      
      if(parallel){
        require(parallel)
        if(is.null(ncores)){
          stop("If running in parallel, you must set the number of cores; see detectCores()")
        }
        cl <- makeCluster(ncores)
        clusterEvalQ(cl, library("lme4"))
        clusterExport(cl, c("data", "config"),envir=environment())
        reg.rep <- lapply(regform, 
                          function(pv) parLapply(cl = cl, X = 1:config$parameters$BRRreps, 
                                                 fun = function(rep)
                                                   summary(lmer(formula = as.formula(pv),
                                                                data = data,
                                                                weights = data[[paste0(config$variables$weightBRR, rep)]]/mean(data[[paste0(config$variables$weightBRR, rep)]]),
                                                                REML = FALSE))[c("coefficients", "varcor")]))
        stopCluster(cl)
      } else {
        reg.rep <- lapply(regform, 
                          function(pv) lapply(X = 1:config$parameters$BRRreps, 
                                              FUN = function(rep) 
                                                summary(lmer(formula = as.formula(pv), 
                                                             data = data, 
                                                             weights = data[[paste0(config$variables$weightBRR, rep)]]/mean(data[[paste0(config$variables$weightBRR, rep)]]),
                                                             REML = FALSE))[c("coefficients", "varcor")]))
        
      }
      
      coe.rep <- lapply(1:config$parameters$PVreps, 
                        function(pv) sapply(1:config$parameters$BRRreps,
                                            function(rep) {
                                              maineff <- reg.rep[[pv]][[rep]]$coefficients[, 1] 
                                              raneff <- as.data.frame(reg.rep[[pv]][[rep]]$varcor)[,"vcov"]
                                              re_names <- as.data.frame(reg.rep[[pv]][[rep]]$varcor)
                                              re_names <- re_names[,!grepl(x = names(re_names), pattern = "vcov|sdcor")]
                                              re_names <- apply(re_names, 1, FUN = function(x) paste0(x, collapse = "_"))
                                              re_names <- gsub(re_names, pattern = "_NA|_NA$", replacement = "")
                                              re_names <- gsub(re_names, pattern = "[(]Intercept[)]", replacement = "(Int)")
                                              names(raneff) <- re_names
                                              return(c(maineff, raneff))
                                            }))
      
      # resid <- lapply(1:config$parameters$PVreps, 
      #                 function(pv) sapply(1:config$parameters$BRRreps,
      #                                     function(rep) reg.rep[[pv]][[rep]]$residuals))
      
      reg.pv <- lapply(regform, 
                       function(pv) summary(lmer(formula = as.formula(pv), 
                                                 data = data, 
                                                 weights = data[[config$variables$weightFinal]]/mean(data[[config$variables$weightFinal]]),
                                                 REML = FALSE)))
      
      coe.tot <- sapply(1:config$parameters$PVreps, 
                        function(pv) {
                          maineff <- reg.pv[[pv]]$coefficients[, 1] 
                          raneff <- as.data.frame(reg.pv[[pv]]$varcor)[,"vcov"]
                          re_names <- as.data.frame(reg.pv[[pv]]$varcor)
                          re_names <- re_names[,!grepl(x = names(re_names), pattern = "vcov|sdcor")]
                          re_names <- apply(re_names, 1, FUN = function(x) paste0(x, collapse = "_"))
                          re_names <- gsub(re_names, pattern = "_NA|_NA$", replacement = "")
                          re_names <- gsub(re_names, pattern = "[(]Intercept[)]", replacement = "(Int)")
                          names(raneff) <- re_names
                          return(c(maineff, raneff))
                        })
      
      stat.tot <- apply(coe.tot, 1, mean)
      
      var.w <- apply(0.05 * sapply(lapply(1:10, function(pv) (coe.rep[[pv]] - coe.tot[, pv])^2), function(e) apply(e, 1, sum)), 1, mean)
      
      var.b <- (1/(length(pvnames)-1)) * apply(sapply(1:config$parameters$PVreps, 
                                                      function(pv) (coe.tot[, pv] - stat.tot)^2), 1, sum)
      
      stat.se <- (var.w + (1 + 1/config$parameters$PVreps) * var.b)^(1/2)
      
      stat.t <- stat.tot/stat.se
      
      
      reg.tab <- data.frame(Estimate = stat.tot, `Std. Error` = stat.se, 
                            `t value` = stat.t, check.names = F)
      
      n_coef <- NROW(reg.pv[[1]]$coefficients)
      n_raneff <- NROW(as.data.frame(reg.pv[[1]]$varcor))
      
      raneff.tab <- reg.tab[(n_coef+1):NROW(reg.tab),]
      names(raneff.tab)[c(1,2)] <- paste("Var", names(raneff.tab)[c(1,2)])
      reg.tab <- reg.tab[-((n_coef+1):NROW(reg.tab)),]
      
      results <- list(replicates = lapply(coe.rep, t), 
                      residuals = NA, var.w = var.w, var.b = var.b, 
                      reg = reg.tab, raneff = raneff.tab)
      return(results)
    }
    if (config$parameters$weights == "JK") {
      stop("Not implemented yet")
    }
    if (config$parameters$weights == "mixed_piaac") {
      stop("Not implemented yet")
    }
  }
  if (missing(by)) {
    output <- reg.pv.input(x = x, pvlabel = pvlabel, data = data, 
                           std = std, config = config, 
                           parallel = parallel, ncores = ncores)
  }
  else {
    output <- lapply(split(data, droplevels(data[by])), 
                     function(i) reg.pv.input(x = x, pvlabel = pvlabel, 
                                              data = i, std = std, 
                                              config = config,
                                              parallel = parallel,
                                              ncores = ncores))
  }
  if (export) {
    write.csv(output, file = file.path(folder, paste(name, ".csv", sep = "")))
  }
  class(output) <- "intsvy.reg"
  return(output)
}