# make a plotting function that takes a random sample 
# from the data frame and looks at d and pep with lines connecting 
# observations from the same realization of data

prof_plot <- function(sim_df, n_samp = 10, cor_x, y = "d"){
  
  # create data frame
  df <- sim_df$d
  pip <- sim_df$PIP
  df$pep <- 1 - pip[,2]
  nx <- length(unique(df$X))
  nsim <- dim(df)[1]/nx
  df$rz <- rep(1:nsim, nx)
  
  # num realizations 
  num_rel <- length(sim_df[[1]])
  # get the sample
  s <- sample(1:num_rel, n_samp, replace = F)
  df <- subset(df, rz %in% s)
  
  top <- bquote(rho["3,6"]~"="~.(cor_x))
  
  if(y == "d"){
    ggplot2::ggplot(df, aes(y = d, x = X, col = rz)) +  
      ggplot2::geom_line(aes(group = factor(rz)), lwd = 1) + 
      ggplot2::geom_point() + 
      ggplot2::theme_bw() + 
      ggplot2::theme(legend.position = "none") + 
      ggplot2::labs(title = top)
  } else {
    ggplot2::ggplot(df, aes(y = pep, x = X, col = factor(rz))) +  
      ggplot2::geom_line(aes(group = factor(rz)), lwd = 1) + 
      ggplot2::geom_point() + 
      ggplot2::theme_bw() + 
      ggplot2::theme(legend.position = "none") + 
      ggplot2::labs(title = top)
  }
  
}

panel_prof <- function(df_vec, cor_vec = c(0.0, 0.3, 0.6, 0.7, 0.8, 0.9), 
                       y = "d", n_samp = 5){
  plots <- list()
  m <- length(cor_vec)
  for(i in 1:m){
    p1 <- prof_plot(df_list[[i]], y = y, cor_x = cor_vec[i], n_samp = n_samp)
    plots[[i]] <- p1
  }
  print(Rmisc::multiplot(plotlist = plots, cols = 3))
}

# raw data plotting functions
plot_raw_dat <- function(df_vec, y,
                         cor_vec = c(0, 0.3, 0.6, 0.7, 0.8, 0.9), 
                         plot_wind = c(2,3)){
  par(mfrow = plot_wind)
  xs <- unique(df_vec[[2]][,1])
  nsims <- dim(df_vec[[2]])[1]/length(xs)
  colors <- rep(c("#878787","#878787","#878787", "#4d4d4d","#4d4d4d"), each = nsims)
  idx <- which(attributes(df_vec)$names == y )
  yt <- ifelse(y == "d", latex2exp::TeX("$Var_{MA}$ / $Var_{cts}$"), "PIP")
  yl <- ifelse(y == "d", 3.2, 1)
  hline <- ifelse(y == "d", 1, NA)
  j <- 1
  for(i in idx){
    plot(df_vec[[i]][,2] ~ jitter(as.numeric(factor(df_vec[[i]][,1]))), 
         pch = "+",
         main = bquote("cor("~X[3]~","~X[5]~") = "~.(cor_vec[j])), ylim = c(0,yl), 
         ylab = yt, type = "n", xaxt = "n")
    axis(1, at = 1:5, labels =  c(latex2exp::TeX("$X_1$"), 
                                  latex2exp::TeX("$X_2$"), 
                                  latex2exp::TeX("$X_3$"), 
                                  latex2exp::TeX("$X_4$"),
                                  latex2exp::TeX("$X_5$"))
    )
    abline(h = hline, lwd = 2, lty = 2, col = "#b2182b")
    points(df_vec[[i]][,2] ~ jitter(as.numeric(factor(df_vec[[i]][,1]))), 
           pch = "+", type = "p", col = colors)
    j <- j + 1
  }
}

#~~~~~~~~~~~~~~~~~~~~~# 
# some plotting funs  # 
#~~~~~~~~~~~~~~~~~~~~~# 

# plot the raw data as '+' symbols see how d and PIP change 
# as correlations between x and y changes

plot_raw_dat <- function(df_vec, y,
                         cor_vec = c(0, 0.3, 0.6, 0.7, 0.8, 0.9), 
                         plot_wind = c(2,3)){
  par(mfrow = plot_wind)
  xs <- unique(df_vec[[2]][,1])
  nsims <- dim(df_vec[[2]])[1]/length(xs)
  colors <- rep(c("#878787","#878787","#878787", "#4d4d4d","#4d4d4d"), each = nsims)
  idx <- which(attributes(df_vec)$names == y )
  yt <- ifelse(y == "d", latex2exp::TeX("$Var_{MA}$ / $Var_{cts}$"), "PIP")
  yl <- ifelse(y == "d", 3.2, 1)
  hline <- ifelse(y == "d", 1, NA)
  j <- 1
  for(i in idx){
    plot(df_vec[[i]][,2] ~ jitter(as.numeric(factor(df_vec[[i]][,1]))), 
         pch = "+",
         main = bquote("cor("~X[3]~","~X[5]~") = "~.(cor_vec[j])), ylim = c(0,yl), 
         ylab = yt, type = "n", xaxt = "n")
    axis(1, at = 1:5, labels =  c(latex2exp::TeX("$X_1$"), 
                                  latex2exp::TeX("$X_2$"), 
                                  latex2exp::TeX("$X_3$"), 
                                  latex2exp::TeX("$X_4$"),
                                  latex2exp::TeX("$X_5$"))
    )
    abline(h = hline, lwd = 2, lty = 2, col = "#b2182b")
    points(df_vec[[i]][,2] ~ jitter(as.numeric(factor(df_vec[[i]][,1]))), 
           pch = "+", type = "p", col = colors)
    j <- j + 1
  }
}

bean_raw_dat <- function(df_vec, y, 
                         cor_vec = c(0, 0.3, 0.6, 0.7, 0.8, 0.9), 
                         plot_wind = c(2,3)){
  par(mfrow = plot_wind)
  xs <- unique(df_vec[[2]][,1])
  nsims <- dim(df_vec[[2]])[1]/length(xs)
  idx <- which(attributes(df_vec)$names == y )
  yt <- ifelse(y == "d", latex2exp::TeX("$Var_{MA}$ / $Var_{cts}$"), "PIP")
  yl <- ifelse(y == "d", 3.2, 1)
  hline <- ifelse(y == "d", 1, NA)
  j <- 1
  cols <- c("#878787","#878787","#878787", "#4d4d4d","#4d4d4d")
  for(i in idx){
    beanplot::beanplot(df_vec[[i]][,2] ~ df_vec[[i]][,1], 
                       main = bquote("cor("~X[3]~","~X[5]~") = "~.(cor_vec[j])), 
                       ylim = c(0.00001, yl), 
                       ylab = yt, log = "", xaxt = "n", 
                       col = list("#878787","#878787","#878787", "#4d4d4d","#4d4d4d"))
    axis(1, at = 1:5, labels =  c(latex2exp::TeX("$X_1$"), 
                                  latex2exp::TeX("$X_2$"), 
                                  latex2exp::TeX("$X_3$"), 
                                  latex2exp::TeX("$X_4$"),
                                  latex2exp::TeX("$X_5$")),
    )
    abline(h = hline, lwd = 2, lty = 2, col = "#b2182b")
    j <- j + 1
  }
}

# each variable on it's own, d ~ PIP for one correlation

plot_indiv <- function(df, cor_x, p = 5, plot_wind = c(2,3), 
                       color = c("#878787","#878787","#878787", "#4d4d4d","#4d4d4d"), 
                       PEP = F, legend = TRUE){
  par(mfrow = plot_wind)
  dat <- df[[2]]
  dat$PIP <- df[[3]][,2]
  dat$pep <- 1-df[[3]][,2]
  xs <- unique(dat$X)
  if(PEP == F){
    for(i in 1:p){
      plot(d ~ PIP, data = subset(dat, X == xs[i]), 
           xlab = "PIP", xlim = c(0,1), 
           ylim = c(0,3.5),
           ylab = latex2exp::TeX("$Var_{MA}$ / $Var_{cts}$"), 
           main = bquote(X[.(i)]), 
           type = "n")
      abline(h = 1, lwd = 2, lty = 2, col = "#b2182b")
      points(d ~ PIP, data = subset(dat, X == xs[i]), 
             pch = "+", xlab = "PIP", xlim = c(0,1), 
             ylim = c(0,3.5),
             ylab = latex2exp::TeX("$Var_{MA}$ / $Var_{cts}$"), 
             main = xs[i], col = color[i])
    }
  } else {
    for(i in 1:p){
      plot(d ~ pep, data = subset(dat, X == xs[i]), 
           xlab = "PEP", 
           xlim = c(0,1), 
           ylim = c(0, 3.5),
           ylab = latex2exp::TeX("$Var_{MA}$ / $Var_{cts}$"), 
           main = bquote(X[.(i)]), 
           type = "n")
      abline(h = 1, lwd = 2, lty = 2, col = "#b2182b")
      points(d ~ pep, data = subset(dat, X == xs[i]),  
             pch = "+",  
             col = color[i])
    }
  }
  if(legend == T){
    plot(c(1,1), xaxt = "n", yaxt = "n", xlim = c(0,1), ylim = c(0,3.2), 
         type = "n", xlab = "", ylab = "")
    text(x = 0.5, y = 1.5, bquote("cor("~X[3]~","~X[5]~") = "~.(cor_x)))    
  } 
  
}

plot_indiv_compare <- function(df_vec, cor_x, 
                               color1  = c("#878787","#878787","#878787","#4d4d4d","#4d4d4d"), 
                               color2 = c("#b2abd2","#b2abd2","#b2abd2", "#8073ac","#8073ac"), 
                               p = 5, labs = NULL, plot_wind = c(2,3), PEP = F){
  par(mfrow = plot_wind)
  dat <- df_vec[[2]]
  dat$PIP <- df_vec[[3]][,2]
  dat$pep <- 1- df_vec[[3]][,2]
  dat2 <- df_vec[[5]]
  dat2$PIP <- df_vec[[6]][,2]
  dat2$pep <- 1- df_vec[[6]][,2]
  xs <- unique(dat$X)
  xs2 <- unique(dat2$X)
  num_x <- dim(dat)[1]/length(xs)
  if(length(xs) < 5){
    xs <- c(xs, NA)
    m <- data.frame(rep(paste0("X", which(is.na(xs))), num_x), 
                    rep(NA, num_x), rep(NA, num_x), rep(NA, num_x))
    names(m) <- names(dat)
    dat <- rbind(dat,m)
  }
  if(length(xs2) < 5){
    xs2 <- c(xs2, NA)
    m <- data.frame(rep(paste0("X", which(is.na(xs2))), num_x), 
                    rep(NA, num_x), rep(NA, num_x), rep(NA, num_x))
    names(m) <- names(dat2)
    dat2 <- rbind(dat2,m)
  }
  if(PEP == F){
    for(i in 1:p){
      plot(d ~ PIP, data = subset(dat, X == xs[i]), 
           pch = "+", xlab = "PIP", xlim = c(0,1), 
           ylim = c(0,3.2),
           ylab = latex2exp::TeX("$Var_{MA}$ / $Var_{cts}$"), 
           main = bquote(X[.(i)]),  type = "n")
      abline(h = 1, lwd = 1, lty = 2, col = "#b2182b")
      abline(v = 0.5, lwd = 1, lty = 2, col = "#b2182b")
      points(d ~ PIP, data = subset(dat, X == xs[i]), 
             pch = "+", col = color1[i], cex = 1.5)
      points(d ~ PIP, data = subset(dat, X == xs[i]), 
             pch = 20, col = color1[i])
      points(d ~ PIP, data = subset(dat2, X == xs2[i]), 
             pch = "o", col = color2[i])
    }
  } else {
    for(i in 1:p){
      plot(d ~ pep, data = subset(dat, X == xs[i]), 
           pch = "+", xlab = "PEP", xlim = c(0,1), 
           ylim = c(0,3.2),
           ylab = latex2exp::TeX("$Var_{MA}$ / $Var_{cts}$"), 
           main = bquote(X[.(i)]),  type = "n")
      abline(h = 1, lwd = 1, lty = 2, col = "#b2182b")
      abline(v = 0.5, lwd = 1, lty = 2, col = "#b2182b")
      points(d ~ pep, data = subset(dat, X == xs[i]), 
             pch = "+", col = color1[i], cex = 1.5)
      points(d ~ pep, data = subset(dat, X == xs[i]), 
             pch = 20, col = color1[i])
      points(d ~ pep, data = subset(dat2, X == xs2[i]), 
             pch = "o", col = color2[i])
    }
  }
  plot(c(1,1), xaxt = "n", yaxt = "n", xlim = c(0,1), ylim = c(0,3.2), 
       type = "n", xlab = "", ylab = "")
  text(x = 0.5, y = 1.5, bquote("cor("~X[3]~","~X[5]~") = "~.(cor_x)))
  if (!is.null(labs)) {
    labels <- c(bquote(sigma[y]~"="~.(labs[1])), bquote(sigma[y]~"="~.(labs[2])))
    legend("topleft", 
           legend = c(as.expression(labels[1]), as.expression(labels[2])),
           pch = c("+", "o"), 
           col = c("#4d4d4d", "#8073ac"), bty = "n")
  }
}


# d ~ PIP all variables together for all correlations

plot_all <- function(df_vec, cor_vec = c(0, 0.3, 0.6, 0.7, 0.8, 0.9), 
                     plot_wind = c(2,3), PEP = F){
  par(mfrow = plot_wind)
  xs <- unique(df_vec[[2]][,1])
  nsims <- dim(df_vec[[2]])[1]/length(xs)
  colors <- rep(c("#66c2a5", "#fc8d62", "#8da0cb","#e78ac3","#a6d854"), each = nsims)
  idx_d <- which(attributes(df_vec)$names == "d")
  max_y <- max(sapply(df_vec[idx_d], function(x){
    return(max(x[,2]))
  })) + sd(sapply(df_vec[idx_d], function(x){
    return(max(x[,2]))
  }))/2
  j <- 1
  if (PEP == F) {
    for(i in idx_d){
      plot(df_vec[[i]][,2] ~ df_vec[[(i+ 1)]][,2], col = colors, 
           pch = "+", xlab = "PIP", ylab = latex2exp::TeX("$Var_{MA}$ / $Var_{cts}$"), 
           main = bquote("cor("~X[3]~","~X[5]~") = "~.(cor_vec[j])), 
           type = "n", ylim = c(0,max_y), xlim = c(0,1))
      abline(h = 1, lwd = 2, lty = 2, col = "#4d4d4d")
      points(df_vec[[i]][,2] ~ df_vec[[(i+ 1)]][,2], 
             col = colors, pch = "+")
      j <- j + 1
    }
    
    legend("topleft", legend = c("X1", "X2", "X3", "X4", "X5"), pch = rep("+", 5), 
           fill = c("#66c2a5", "#fc8d62", "#8da0cb","#e78ac3","#a6d854"), 
           cex = 1, bty = "n")
  } else {
    for(i in idx_d){
      pep <- 1 - df_vec[[(i + 1)]][,2]
      plot(df_vec[[i]][,2] ~ pep, col = colors, 
           pch = "+", xlab = "PEP", ylab = latex2exp::TeX("$Var_{MA}$ / $Var_{cts}$"), 
           main = bquote("cor("~X[3]~","~X[5]~") = "~.(cor_vec[j])), 
           type = "n", ylim = c(0,max_y), xlim = c(0,1))
      abline(h = 1, lwd = 2, lty = 2, col = "#4d4d4d")
      points(df_vec[[i]][,2] ~ pep, 
             col = colors, pch = "+")
      j <- j + 1
    }
    legend("topright", legend = c("X1", "X2", "X3", "X4", "X5"), pch = rep("+", 5), 
           fill = c("#66c2a5", "#fc8d62", "#8da0cb","#e78ac3","#a6d854"), 
           cex = 1, bty = "n")
  }
}


plot_all_compare <- function(df_vec, cor_vec = c(0.0,0.3,0.6,0.7,0.8,0.9), 
                             plot_wind = c(2,3), PEP = F){
  par(mfrow = plot_wind)
  xs <- unique(df_vec[[2]][,1])
  nsims <- dim(df_vec[[2]])[1]/length(xs)
  colors <- rep(c("#66c2a5", "#fc8d62", "#8da0cb","#e78ac3","#a6d854"), each = nsims)
  idx_d <- which(attributes(df_vec)$names == "d")
  idx_d <- idx_d[1:6]
  j <- 1
  for(i in idx_d){
    plot(df_vec[[i]][,2] ~ df_vec[[(i+ 1)]][,2], col = colors, 
         pch = "+", xlab = "PIP", ylab = latex2exp::TeX("$Var_{MA}$ / $Var_{cts}$"), 
         main = bquote("cor("~X[3]~","~X[5]~") = "~.(cor_vec[j])), 
         type = "n", ylim = c(0,3.2))
    abline(h = 1, lwd = 2, lty = 2, col = "#4d4d4d")
    points(df_vec[[i]][,2] ~ df_vec[[(i+ 1)]][,2], 
           col = colors, pch = "+")
    points(df_vec[[(i+18)]][,2] ~ df_vec[[(i+19)]][,2], 
           col = colors, pch = "o") 
    j <- j + 1
  }
  
  legend("topleft", legend = c("X1", "X2", "X3", "X4", "X5"), 
         fill = c("#66c2a5", "#fc8d62", "#8da0cb","#e78ac3","#a6d854"), 
         cex = 1, bty = "n")
}


prof_plot <- function(sim_df, n_samp = 10, cor_x, y = "d"){
  
  # create data frame
  df <- sim_df$d
  pip <- sim_df$PIP
  df$pep <- 1 - pip[,2]
  nx <- length(unique(df$X))
  nsim <- dim(df)[1]/nx
  df$rz <- rep(1:nsim, nx)
  
  # num realizations 
  num_rel <- length(sim_df[[1]])
  # get the sample
  s <- sample(1:num_rel, n_samp, replace = F)
  df <- subset(df, rz %in% s)
  
  top <- bquote(rho["3,6"]~"="~.(cor_x))
  
  if(y == "d"){
    ggplot2::ggplot(df, aes(y = d, x = X, col = factor(rz))) +  
      ggplot2::geom_line(aes(group = factor(rz)), lwd = 1) + 
      ggplot2::geom_point() + 
      ggplot2::theme_bw() + 
      ggplot2::theme(legend.position = "none") + 
      ggplot2::labs(title = top)
  } else {
    ggplot2::ggplot(df, aes(y = pep, x = X, col = factor(rz))) +  
      ggplot2::geom_line(aes(group = factor(rz)), lwd = 1) + 
      ggplot2::geom_point() + 
      ggplot2::theme_bw() + 
      ggplot2::theme(legend.position = "none") + 
      ggplot2::labs(title = top)
  }
  
}


panel_prof <- function(df_vec, cor_vec = c(0.0, 0.3, 0.6, 0.7, 0.8, 0.9), 
                       y = "d", n_samp = 5){
  plots <- list()
  m <- length(cor_vec)
  for(i in 1:m){
    p1 <- prof_plot(df_list[[i]], y = y, cor_x = cor_vec[i], n_samp = n_samp)
    plots[[i]] <- p1
  }
  print(Rmisc::multiplot(plotlist = plots, cols = 3))
}


