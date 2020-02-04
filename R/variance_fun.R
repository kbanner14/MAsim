var_betaMA <- function(x){
  # function to take a bma object created using bms and return the variance of 
  # the posterior distribution for the MA coefficient according to the formula 
  # provided in Hoeting et. al 1999
  
  # calculate posterior means for each individual model delta_hat_k in Hoeting
  mean_betas <- x$topmod$betas()
  
  # I think we want the intercept only model.
  # # forget the intercept only model
  # idx <- which(apply(mean_betas, 2, sum) != 0)
  mean_betas <- t(mean_betas)
  
  inmat <- mean_betas
  inmat[which(inmat != 0)] <- 1
  
  # calculate the variances of the coefficients in each individual model
  # (Var[delta|D,M_k])
  var_betas <- x$topmod$betas2() - x$topmod$betas()^2
  var_betas <- t(var_betas)
  
  # calculate posterior model weights for the top models
  weights <- matrix(sort(BMS::pmp.bma(x)[,1], decreasing = T))
  
  # Calculate the model averaged posterior estiamtes squared.
  Ebeta_all2 <- (t(weights)%*%mean_betas)^2
  Ebeta_indiv2 <- mean_betas^2
  term1 <- t(weights)%*%(var_betas  + Ebeta_indiv2)
  var_est <- term1 - Ebeta_all2
  dimnames(var_est)[[2]] <- dimnames(coef(x)[order(coef(x)[,"Idx"]), ])[[1]]
  dimnames(var_est)[[1]] <- "var_MA"
  var_est <- t(var_est)
  var_est <- data.frame(var_est)
  var_est$sd_MA <- sqrt(var_est$var_MA)
  
  # # Calculate the cts. component variance.
  # # have to do this in a for loop because the 
  # var_cts <- numeric(dim(mean_betas)[2])
  # for(i in 1:length(var_cts)){
  #   idx <- which(inmat[,i] == 1)
  #   # posteiror means from the individual models that include variable 
  #   mb <- mean_betas[idx,i]
  #   # PMP's for models that include variable
  #   wt <- weights[idx]
  #   # normalize those weights
  #   wt_re <- wt/sum(wt)
  #   # posterior variances
  #   vb <- var_betas[idx,i]
  #   # individual means squared for first term (var(beta|m_k, y) + E(beta|mk, y)^2)
  #   Eb_ind2 <- mb^2
  #   # 
  #   Eb_cts2 <- (t(wt)%*%mb)^2
  #   t1 <- t(wt)%*%(vb  + Eb_ind2)
  #   var_cts[i] <- t1 - Eb_cts2
  # }
  
  var_cts_norm <- numeric(dim(mean_betas)[2])
  for(i in 1:length(var_cts_norm)){
    idx <- which(inmat[,i] == 1)
    # posteiror means from the individual models that include variable 
    mb <- mean_betas[idx,i]
    # PMP's for models that include variable
    wt <- weights[idx]
    # normalize those weights
    wt_re <- wt/sum(wt)
    # posterior variances
    vb <- var_betas[idx,i]
    # individual means squared for first term (var(beta|m_k, y) + E(beta|mk, y)^2)
    Eb_ind2 <- mb^2
    # 
    Eb_cts2 <- (t(wt_re)%*%mb)^2
    t1 <- t(wt_re)%*%(vb  + Eb_ind2)
    var_cts_norm[i] <- t1 - Eb_cts2
  }
  
  
  # var_est$var_cts <- var_cts
  # var_est$sd_cts <- sqrt(var_cts)
  var_est$var_cts_norm <- var_cts_norm
  var_est$sd_cts_norm <- sqrt(var_cts_norm)
  var_est <- var_est[,c(1,3,2,4)]
  
  return(var_est)
}


# function to obtain the approximate MA posterior means and sd's, and also 
# the number of draws taken from each model considered. 

sum_stat <- function(dat, num_sims = 1000, mod_names = NULL,...){
  
  # function to track useful summary statistics for evaluating 
  # MCMC errror in our simulation process for the UIP g-prior formulation of 
  # BMA. Input a data frame and return mean_MA, sd_cts, sd_MA, and total samples. 
  # This version uses the multinomial distribution to sample from individual 
  # posteriors
  
  # genreate BMA object and save important info 
  x <- bms(dat, mprior = "uniform", g = "UIP", user.int = F)
  
  # create input matrix for models visisted
  post_means <- x$topmod$betas()
  inmat <- post_means
  inmat[inmat != 0] <- 1
  
  # save data and dimensionality
  Yvec <- x$X.data[,1]
  Xmat <- x$X.data[,-1]
  K <- dim(inmat)[1]
  
  # find posterior model probabilities
  weights <- matrix(sort(pmp.bma(x)[,1], decreasing = T))
  
  # generate individual and MA posterior distributions for coefficients
  # Save the number of samples per model going into the MA posterior 
  MA_posts <- MApp::MApost_bms_sim(x, num_draws = num_sims, mod_names = mod_names)
  posts <- MA_posts[[1]]
  mod_samps <- MA_posts[[2]]
  
  # track informative summary statistics 
  MA_Qsum <- apply(posts, 2, function(x){
    SD_MA <- sd(x)
    mean_MA <- mean(x)
    x <-x[x != 0]
    cts_SD <- sd(x)
    return(c(mean_MA, cts_SD, SD_MA))
  })
  
  # sort the info to match original sampler (not multinomial)
  MA_Qsum <- data.frame(t(MA_Qsum))
  names(MA_Qsum) <- c("mean_MA","SD_cts", "SD_MA")
  MA_Qsum$tot_samp <- sum(mod_samps)
  
  # return data frame and samps per model 
  return(list(MA_Qsum[,c(4,1,2,3)], mod_samps))
  
}






