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

# simulation wrapper function to genreate data frames under different data generating 
# models, and then fit BMS to those data under different assumptions 
# about the model set w.r.t. the true data generating model. 
varMActs_sim <- function(nsims, n = 60, p = 5, betas = c(0,0,0,1,1.2), 
                      sig_y = 2.5, sig_x = rep(1,5), cor_x = 0, 
                      cor_vars = c(3,5), tol = 0.05, seed_num = NULL, case = 1,
                      truth4 = F,
                      ...){
  
  dat <- as.list(1:nsims)
  
  for(i in 1:nsims){
    dat[[i]] <- data_sim(n = n, p = p, betas = betas, 
                             sig_y = sig_y, sig_x = sig_x, 
                             cor_x = cor_x, 
                             cor_vars = cor_vars, 
                             tol = tol, truth4 = truth4)
  }
  out <- as.list(1:2)
  out[[1]] <- dat
  if (case == 1) {
    sum_dat <- lapply(dat, FUN = function(x){
      dat_bms <- BMS::bms(x, mprior = "uniform", 
                          user.int = F, g = "UIP")
      var_top <- dat_bms$topmod$betas2()[,1] - 
        (dat_bms$topmod$betas()[,1])^2
      vars <- var_betaMA(dat_bms)
      var_ma <- vars[,1]
      var_cts <- vars[,2]
      names(var_top) <- dimnames(vars)[[1]]
      names(var_cts) <- dimnames(vars)[[1]]
      names(var_ma) <- dimnames(vars)[[1]]
      d <- vars[,1]/vars[,2]
      names(d) <- dimnames(vars)[[1]]
      PIP <- coef(dat_bms)[order(coef(dat_bms)[,5]), ][,1]
      return(data.frame(X = names(x[2:6]), cor_x, d, PIP, 
                        var_ma, var_cts, var_top))
    })
    d <- data.frame()
    for(i in 1:length(sum_dat)){
      df_summ <- sum_dat[[i]]
      df_summ$iter <- i
      d <- rbind(d, df_summ)
    }
    # create more summary measures
    d$var_top[d$var_top == 0] <- NA
    d$d_topMA <- d$var_ma/d$var_top
    d$d_topCts <- d$var_cts/d$var_top
    d$diff_MAt <- sqrt(d$var_ma) - sqrt(d$var_top)
    d$diff_ct <- sqrt(d$var_cts) - sqrt(d$var_top)
    out[[2]] <- d
    names(out) <- c("dat", "summary")
  } else {
    sum_dat <- lapply(dat, FUN = function(x){
      dat_bms <- BMS::bms(x[,c(1,2:5)], mprior = "uniform", 
                          user.int = F, g = "UIP")
      var_top <- dat_bms$topmod$betas2()[,1] - 
        (dat_bms$topmod$betas()[,1])^2
      vars <- var_betaMA(dat_bms)
      var_ma <- vars[,1]
      var_cts <- vars[,2]
      names(var_top) <- dimnames(vars)[[1]]
      names(var_cts) <- dimnames(vars)[[1]]
      names(var_ma) <- dimnames(vars)[[1]]
      d <- vars[,1]/vars[,2]
      names(d) <- dimnames(vars)[[1]]
      PIP <- coef(dat_bms)[order(coef(dat_bms)[,5]), ][,1]
      return(data.frame(X = names(x[2:5]), cor_x, 
             d, PIP, var_ma, var_cts, var_top))
    }
    ) 
    
    d <- data.frame()
    for(i in 1:length(sum_dat)){
      df_summ <- sum_dat[[i]]
      df_summ$iter <- i
      d <- rbind(d, df_summ)
    }
    # create more summary measures
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

# data simulation function, generates data under specific data 
# generating model - e.g., different signal to noise ratio, 
# levels of correlation between X3 and X5, tapering effect of X4. 
# true model: truth4 = F, Y = X4 + 1.2X5 + epsilon; epsilon ~ N(0,sigy)
#             truth4 = T, Y = X4 + 0.5 X^2 + 0.25X^3 + 1.2X5
# 
data_sim <- function(n = 60, p = 5, betas = c(0,0,0,1,1.2),
                         sig_y = 2.5, sig_x = rep(1, 5), cor_x = 0,
                         cor_vars = c(3,5), tol = 0.05, truth4 = F, 
                         b42 = 0.5, b43 = 0.25, ...){
  # for now we are just going to assume that two of the variables are 
  # correlated X3 and X5, can change this with cor_vars
  try(if(length(betas) != p)  
    stop("dimensions of betas and number of parameters (p) must agree"))
  
  # draw from mv normal with desired covariance structure to 
  # get X1, X2, ..., X5
  
  var_x <- sig_x^2
  cov <- diag(var_x)
  cov[cor_vars[1], cor_vars[2]] <- cor_x*sig_x[cor_vars[1]]*sig_x[cor_vars[2]]
  cov[cor_vars[2], cor_vars[1]] <- cov[cor_vars[1], cor_vars[2]]
  
  Xmat <- LearnBayes::rmnorm(n, mean = rep(0,5), varcov = cov)
  # calculate difference between 
  c <- abs(cor_x - cor(Xmat[,cor_vars[1]], Xmat[, cor_vars[2]]))
  
  while(c >= tol) {
    Xmat <- LearnBayes::rmnorm(n, mean = rep(0,5), varcov = cov)
    c <- abs(cor_x - cor(Xmat[,cor_vars[1]], Xmat[, cor_vars[2]]))
  }
  
  dimnames(Xmat)[[2]] <- paste0("X", 1:p)
  # generate y 
  epsilon <- rnorm(n, 0, sig_y)
  # check for truth 4 scenario
  if(truth4 == F){
    Y <- Xmat%*%matrix(betas) + epsilon
  } else {
    X4sq <- Xmat[,4]^2
    X4cu <- Xmat[,4]^3
    Xmat <- cbind(Xmat, X4sq, X4cu)
    betas <- c(betas, b42, b43)
    Y <- Xmat%*%matrix(betas) + epsilon
    Xmat <- Xmat[,1:5]
  }
  # make the data frame 
  dat <-  data.frame(Y = Y, Xmat)
  return(dat)
}
# sim wrapper function to test that data sim is generating data the way we 
# want it to. 
sim_test <- function(nsim = 1000, n = 60, p = 5, betas = c(0,0,0,1,1.2),
                     sig_y = 2.5, sig_x = rep(1, 5), cor_x = 0,
                     cor_vars = c(3,5), tol = 0.05, truth4 = F, 
                     b42 = 0.5, b43 = 0.25, ...){
  summ_df <- data.frame()
  for(i in 1:nsim){
    dat <- data_sim(n = n, p = p, betas = betas, 
                        sig_y = sig_y, sig_x = sig_x, cor_x = cor_x, 
                        cor_vars = cor_vars, tol = tol, truth4 = truth4)
    fit <- lm(Y ~ X1 + X2 + X3 + X4 + X5, data = dat)
    bias <- coef(fit) - c(0,betas)
    cap <- (confint(fit)[,1] <= c(0, betas)) & ((confint(fit)[,2] >= c(0,betas)))
    out <- data.frame(coef(fit), confint(fit), summary(fit)$coef[,2],
                      row.names(confint(fit)), bias, as.numeric(cap), c(0,betas))
    names(out) <- c("est", "lower95", "upper95", "se", "prc", "bias", "cap", "truth")
    out$iter <- i
    summ_df <- rbind(summ_df,out)
  }
  df_results <- summ_df %>% dplyr::group_by(prc) %>% 
    dplyr::summarise(avg_est = mean(est), 
                     avg_se = mean(se),
                     avg_bias = mean(bias), 
                     avg_cap = mean(cap), 
                     avg_ciL = mean(lower95), 
                     avg_ciU = mean(upper95),
                     truth = unique(truth))
  sim_sum <- as.list(1:2)
  sim_sum[[1]] <- summ_df
  sim_sum[[2]] <- df_results
  return(sim_sum)
}


