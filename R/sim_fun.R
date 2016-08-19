#~~~~~~~~~~~~~~#
# sim function #
#~~~~~~~~~~~~~~#
# factors to manipulate: signal to noise (coef of data generating model to sig_y ratio)
# extra temrs in DG model (BA suggest that global model be smaller than the data generating model)
#    - need to think more about justification for this
# number of terms in data generating model (simple vs. tapering effects)
# correlation among x variables - start with 2 to keep simple (iid, .2, .4, .6)
# forcing collinear variables above a certain threshold into the model to create model 
# set....ask megan about this - idea for another factor to manipulate - gets at 
# bias rather than variance, so maybe not good idea for this study. 

sim_d <- function(nsims, n = 60, p = 5, betas = c(0,0,0,1,1.2), 
                  sig_y = 2.5, sig_x = rep(1,5), cor_x = 0, 
                  cor_vars = c(3,5), tol = 0.05, seed_num = NULL, case = 1, 
                  ...){
  d <- matrix(NA, nrow = nsims, ncol = p)
  dat <- as.list(1:nsims)
  PIP <- matrix(NA, nrow = nsims, ncol = p) 
  
  for(i in 1:nsims){
    dat[[i]] <- data_sim(n = n, p = p, betas = betas, 
                         sig_y = sig_y, sig_x = sig_x, cor_x = cor_x, 
                         cor_vars = cor_vars, tol = tol)
  }
  if (case == 1) {
    sum_dat <- lapply(dat, FUN = function(x){
      dat_bms <- BMS::bms(x, mprior = "uniform", user.int = F, g = "UIP")
      vars <- var_betaMA(dat_bms)
      var_ma <- vars[,1]
      var_cts <- vars[,2]
      d <- vars[,1]/vars[,2]
      names(d) <- dimnames(vars)[[1]]
      PIP <- coef(dat_bms)[order(coef(dat_bms)[,5]), ][,1]
      return(c(d, PIP))
    }
    ) 
    sum_dat <- do.call(rbind, sum_dat)
    out <- as.list(1:3)
    out[[1]] <- dat
    out[[2]] <- tidyr::gather(data.frame(sum_dat[,1:p]), "X1:X5")
    out[[3]] <- tidyr::gather(data.frame(sum_dat[,(p+1):(2*p)]), "X1:X5")
    names(out) <- c("dat", "d", "PIP")
    names(out$d) <- c("X", "d")
    names(out$PIP) <- c("X", "PIP")
  } else {
    sum_dat <- lapply(dat, FUN = function(x){
      dat_bms <- BMS::bms(x[,c(1,2:5)], mprior = "uniform", user.int = F, g = "UIP")
      vars <- var_betaMA(dat_bms)
      d <- vars[,1]/vars[,2]
      names(d) <- dimnames(vars)[[1]]
      PIP <- coef(dat_bms)[order(coef(dat_bms)[,5]), ][,1]
      return(c(d, PIP))
    }
    )
    sum_dat <- do.call(rbind, sum_dat)
    out <- as.list(1:3)
    out[[1]] <- dat
    out[[2]] <- tidyr::gather(data.frame(sum_dat[,1:(p-1)]), "X1:X4")
    out[[3]] <- tidyr::gather(data.frame(sum_dat[,p:(2*(p-1))]), "X1:X4")
    names(out) <- c("dat", "d", "PIP")
    names(out$d) <- c("X", "d")
    names(out$PIP) <- c("X", "PIP")
  }

  return(out)
}


sim_d2 <- function(nsims, n = 60, p = 5, betas = c(0,0,0,1,1.2), 
                  sig_y = 2.5, sig_x = rep(1,5), cor_x = 0, 
                  cor_vars = c(3,5), tol = 0.05, seed_num = NULL, case = 1,
                  truth4 = F,
                  ...){
  
  dat <- as.list(1:nsims)
  
  for(i in 1:nsims){
    dat[[i]] <- data_sim(n = n, p = p, betas = betas, 
                         sig_y = sig_y, sig_x = sig_x, cor_x = cor_x, 
                         cor_vars = cor_vars, tol = tol, truth4 = F)
  }
  if (case == 1) {
    sum_dat <- lapply(dat, FUN = function(x){
      dat_bms <- BMS::bms(x, mprior = "uniform", user.int = F, g = "UIP")
      var_top <- dat_bms$topmod$betas2()[,1] - (dat_bms$topmod$betas()[,1])^2
      vars <- var_betaMA(dat_bms)
      var_ma <- vars[,1]
      var_cts <- vars[,2]
      names(var_top) <- dimnames(vars)[[1]]
      names(var_cts) <- dimnames(vars)[[1]]
      names(var_ma) <- dimnames(vars)[[1]]
      d <- vars[,1]/vars[,2]
      names(d) <- dimnames(vars)[[1]]
      PIP <- coef(dat_bms)[order(coef(dat_bms)[,5]), ][,1]
      return(c(d, PIP, var_ma, var_cts, var_top))
    }
    ) 
    sum_dat <- do.call(rbind, sum_dat)
    out <- as.list(1:2)
    out[[1]] <- dat
    d <- tidyr::gather(data.frame(sum_dat[,1:p]), "X1:X5")
    names(d) <- c("X", "d")
    d$PIP <- tidyr::gather(data.frame(sum_dat[,(p+1):(2*p)]), "X1:X5")[,2]
    d$var_ma <- tidyr::gather(data.frame(sum_dat[,(2*p+1):(3*p)]), "X1:X5")[,2]
    d$var_cts <- tidyr::gather(data.frame(sum_dat[,(3*p+1):(4*p)]), "X1:X5")[,2]
    d$var_top <- tidyr::gather(data.frame(sum_dat[,(4*p+1):(5*p)]), "X1:X5")[,2]
    d$var_top[d$var_top == 0] <- NA
    d$d_topMA <- d$var_ma/d$var_top
    d$d_topCts <- d$var_cts/d$var_top
    d$diff_MAt <- sqrt(d$var_ma) - sqrt(d$var_top)
    d$diff_ct <- sqrt(d$var_cts) - sqrt(d$var_top)
    
    out[[2]] <- d
    names(out) <- c("dat", "summary")
  } else {
      sum_dat <- lapply(dat, FUN = function(x){
      dat_bms <- BMS::bms(x[,c(1,2:5)], mprior = "uniform", user.int = F, g = "UIP")
      var_top <- dat_bms$topmod$betas2()[,1] - (dat_bms$topmod$betas()[,1])^2
      vars <- var_betaMA(dat_bms)
      var_ma <- vars[,1]
      var_cts <- vars[,2]
      names(var_top) <- dimnames(vars)[[1]]
      names(var_cts) <- dimnames(vars)[[1]]
      names(var_ma) <- dimnames(vars)[[1]]
      d <- vars[,1]/vars[,2]
      names(d) <- dimnames(vars)[[1]]
      PIP <- coef(dat_bms)[order(coef(dat_bms)[,5]), ][,1]
      return(c(d, PIP, var_ma, var_cts, var_top))
    }
    ) 
    sum_dat <- do.call(rbind, sum_dat)
    out <- as.list(1:2)
    out[[1]] <- dat
    p <- p-1
    d <- tidyr::gather(data.frame(sum_dat[,1:p]), "X1:X4")
    names(d) <- c("X", "d")
    d$PIP <- tidyr::gather(data.frame(sum_dat[,(p+1):(2*p)]), "X1:X4")[,2]
    d$var_ma <- tidyr::gather(data.frame(sum_dat[,(2*p+1):(3*p)]), "X1:X4")[,2]
    d$var_cts <- tidyr::gather(data.frame(sum_dat[,(3*p+1):(4*p)]), "X1:X4")[,2]
    d$var_top <- tidyr::gather(data.frame(sum_dat[,(4*p+1):(5*p)]), "X1:X4")[,2]
    d$var_top[d$var_top == 0] <- NA
    d$d_topMA <- d$var_ma/d$var_top
    d$d_topCts <- d$var_cts/d$var_top
    d$diff_MAt <- sqrt(d$var_ma) - sqrt(d$var_top)
    d$diff_ct <- sqrt(d$var_cts) - sqrt(d$var_top)
    out[[2]] <- d
    names(out) <- c("dat", "summary")
  }
  
  return(out)
}


