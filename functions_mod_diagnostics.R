# ====== Functions for model Bayesian diagnostics ====

# Trace plot
trace_plot <- function(fit, par, n_chains) {
  
  d <- as_mcmc.list(fit)
  
  d <- lapply(d, FUN = 
                function(x) {
                  x <- as.data.frame(unclass(as.matrix(x)))
                  x$iter <- 1:nrow(x)
                  x
                })
  
  for (i in 1:n_chains) d[[i]]$chain <- i
  
  plot(d[[1]][, 'iter', drop = T], d[[1]][, par, drop = T], 
       type = 'l', col = 1, main = '', ylab = par, 
       xlab = 'Iteration')
  for (i in 2:n_chains) {
    lines(d[[i]][, 'iter', drop = T], d[[i]][, par, drop = T], 
          col = i, main = '', ylab = par, 
          xlab = 'Iteration')
  }
  
}


# Pareto K, ess, and Rhat
mod_diagnostics <- function(model, output, pareto_k = F) {
  
  par(mar = c(4, 4, 1, 1))
  
  if (pareto_k) {
    
    l <- model$loo()
    
    pk <- l$diagnostics$pareto_k
    
    pareto <- tibble(x = 1:length(pk), 
                     pareto_k = pk)
    
    diags <- na.omit(tibble(x = 1:nrow(output), 
                            rhat = output$rhat, 
                            ess_bulk = output$ess_bulk, 
                            ess_tail = output$ess_tail))
    
    
    diags %$% plot(rhat ~ x, pch = 16, col = 'tomato3', xlab = 'Parameters', 
                   ylab = 'Rhat')
    abline(h = 1.05, lty = 3, col = 'red')
    diags %$% plot(sort(ess_bulk) ~ x, pch = 16, col = 'cyan4', xlab = 'Parameters', 
                   ylab = 'ESS')
    diags %$% points(sort(ess_tail) ~ x, pch = 16, col = 'tan1')
    abline(h = 500, lty = 3, col = 'red')
    pareto %$% plot(pareto_k ~ x, pch = 16, col = 'purple', xlab = 'Observations', 
                    ylab = 'Pareto-k', ylim = c(-0.5, 1.2))
    abline(h = c(0.5, 0.7, 1), lty = 3, col = 'red')
    text(x = rep(nrow(pareto)/2, 4), y = c(0.25, 0.6, 0.8, 1.2), 
         labels = c('Perfect', 'ok', 'high', 'too hight'))
    par(mfrow = c(1, 1))
    
  } else {
    diags <- na.omit(tibble(x = 1:nrow(output), 
                            rhat = output$rhat, 
                            ess_bulk = output$ess_bulk, 
                            ess_tail = output$ess_tail))
    
    
    diags %$% plot(rhat ~ ess_bulk, cex = 1, col = 'tomato3', xlab = 'ESS parameters (tail and bulk)', 
                   ylab = 'Rhat', xlim = c(0, max(ess_bulk) + 200))
    abline(h = 1.05, v = 500, lty = 3, col = 'red')
    diags %$% points(rhat ~ ess_tail, cex = 1, col = 'cyan4')
    #par(mfrow = c(1, 1))
    
  }
  
}
