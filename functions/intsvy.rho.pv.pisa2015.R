intsvy.rho.pv.pisa2015 <- function(variable, pvlabels, by, data, export = FALSE, name = "output", 
                                   folder = getwd(), config=intsvy:::pisa2015_conf){

  rho.pv.input <- function(variable, pvlabels, data, config) {
    
    if (config$parameters$weights == "BRR") {
      if (length(pvlabels) == 2 & missing(variable)){
        stop("Not implemented yet")
      } else {
        pvnames  <- paste0("PV", 1:config$parameters$PVreps, pvlabels)
        
        data <- na.omit(data[c(variable, pvnames, config$variables$weightFinal, paste0(config$variables$weightBRR, 1:config$parameters$BRRreps))])
        
        rhopvrp <- lapply(1:config$parameters$BRRreps, function(i) cov.wt(x = data[c(variable, pvnames[1])], cor = T, wt = data[[paste0(config$variables$weightBRR,i)]])[[5]])
        
        rhopvtot <- lapply(pvnames, function(i) cov.wt(x = data[c(variable, i)], cor = TRUE, wt = data[[config$variables$weightFinal]])[[5]])
        
        varw <- (1/20)*Reduce("+", lapply(rhopvrp, function(x) (x - rhopvtot[[1]])^2)) 
        
        rhopvtot.mean <- apply(simplify2array(rhopvtot), c(1,2), FUN = mean)
        
        varb <- (1/(config$parameters$PVreps-1))*apply(simplify2array(lapply(1:10, function(i) (rhopvtot[[i]]-rhopvtot.mean)^2)), c(1,2), sum)
        
        rhose <- (varw + (1 + 1/config$parameters$PVreps)*varb)^(1/2)
        
        rhotot <- Reduce("+", rhopvtot)/length(rhopvtot)
        
        rhomat <- round(do.call(cbind, lapply(1:ncol(rhotot), function(x) t(rbind(rhotot[, x], rhose[, x])))), 5)
        
        colnames(rhomat) <- unlist(lapply(1:2, function(i) c(paste(c(variable, pvlabels), "Rho", sep = " ")[i], paste(c(variable, pvlabels), "SE", sep = " ")[i])))
        
        return(round(rhomat, 3))
      }
    }
  }
  
  if (missing(by)) {
    output <- rho.pv.input(variable = variable, pvlabels = pvlabels, data = data, config = config)
  } else {
    output <- lapply(split(data, factor(data[[by]])), 
                     function(x) rho.pv.input(variable = variable, pvlabels = pvlabels, data = x, config = config))
  }
  if (export) {
    write.csv(output, file = file.path(folder, paste(name, ".csv", sep = "")))
  }
  class(output) <- c("intsvy.rho", class(output))
  return(output)
  
}    

