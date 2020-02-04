# must load MAPP funtions load MAPP functions 

# more general function for simulation study
data_sim <- function(n = 60, p = 5, betas = c(0,0,0,1,1.2), 
                    sig_y = 2.5, sig_x = rep(1, 5), cor_x = 0,
                    cor_vars = c(3,5), tol = 0.05, truth4 = F, 
                    b42 = 0.5, b43 = 0.25, ...){
  # for now we are just going to assume that two of the variables are 
  # correlated X3 and X5, can change this with cor_vars
  try(if(length(betas) != p)  
    stop("dimensions of betas and number of parameters (p) must agree"))
  
    # draw iid normal x's, then use cor matrix (R) and Cholesky 
    # factorization to get desired correlation structure in x's
    
    var_x <- sig_x^2
    cov <- diag(var_x)
    R <- diag(p)
    R[cor_vars[1], cor_vars[2]] <- cor_x
    R[cor_vars[2], cor_vars[1]] <- cor_x
    U <- t(chol(R))
    
    
    # simulate the x matrix 
    Xmat <- LearnBayes::rmnorm(n, mean = betas, varcov = cov)
    Xmat <- t(U %*% t(Xmat))
    c <- abs(cor_x - cor(Xmat[,cor_vars[1]], Xmat[, cor_vars[2]]))
    
    while(c >= tol) {
      Xmat <- LearnBayes::rmnorm(n, mean = betas, varcov = cov)
      Xmat <- t(U %*% t(Xmat))
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
    # make the dat frame
    dat <- scale(data.frame(Y,Xmat), center = T, scale = T)
    dat <- data.frame(dat)
    return(dat)
    
}

