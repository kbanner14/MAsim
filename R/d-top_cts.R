######################################################
# Functions for comparing d_topMA to d_topCTS, etc.  #
######################################################

bean_d_dat <- function(df_vec, y, 
                          cor_vec = c(0, 0.3, 0.6, 0.7, 0.8, 0.9), 
                          plot_wind = c(2,3)){
  par(mfrow = plot_wind)
  xs <- unique(df_vec[[2]][,1])
  nsims <- dim(df_vec[[2]])[1]/length(xs)
  idx <- which(names(df_vec) == "summary")
  yt <- ifelse(y == "d", latex2exp::TeX("$Var_{MA}$ / $Var_{cts}$"),
               ifelse(y == "d_topCts", latex2exp::TeX("$Var_{cts}$ / $Var_{top}$"),
                      ifelse(y == "d_topMA", 
                             latex2exp::TeX("$Var_{MA}$ / $Var_{top}$"), 
                             ifelse(y == "diff_ct", latex2exp::TeX("$SD_{cts} - SD_{top}$"), 
                                    ifelse(y == "diff_MAt", 
                                           latex2exp::TeX("$SD_{MA} - SD_{top}$"), "PIP")
                      ))))
  yl <- ifelse(y == "PIP", 1, 
               ifelse(y == "d_topCts", 1.25, 3.25))
  hline <- ifelse(y == "PIP", NA, 
                  ifelse(y == "diff_ct" | y == "diff_MAt", 0, 1))
  j <- 1
  cols <- c("#878787","#878787","#878787", "#4d4d4d","#4d4d4d")
  for(i in idx){
    beanplot::beanplot(df_vec[[i]][,y] ~ df_vec[[i]][,1], 
                       main = bquote("cor("~X[3]~","~X[5]~") = "~.(cor_vec[j])), 
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

# d ~ PIP all variables together for all correlations

plot_all_d <- function(df_vec, y, cor_vec = c(0, 0.3, 0.6, 0.7, 0.8, 0.9), 
                     plot_wind = c(2,3), PEP = F){
  par(mfrow = plot_wind)
  xs <- unique(df_vec[[2]][,1])
  nsims <- dim(df_vec[[2]])[1]/length(xs)
  colors <- rep(c("#66c2a5", "#fc8d62", "#8da0cb","#e78ac3","#a6d854"), each = nsims)
  idx_d <- which(attributes(df_vec)$names == "summary")
  max_y <- max(sapply(df_vec[idx_d], function(x){
    return(max(x[,y], na.rm = T))
  })) + sd(sapply(df_vec[idx_d], function(x){
    return(max(x[,y], na.rm = T))
  }))/2
  j <- 1
  ylabel <- ifelse(y == "d", latex2exp::TeX("$Var_{MA}$ / $Var_{cts}$"),
                         ifelse(y == "d_topCts", latex2exp::TeX("$Var_{cts}$ / $Var_{top}$"),
                                ifelse(y == "d_topMA", latex2exp::TeX("$Var_{MA}$ / $Var_{top}$"), 
                                       ifelse(y == "diff_ct", latex2exp::TeX("$SD_{cts} - SD_{top}$"), 
                                               latex2exp::TeX("$SD_{MA} - SD_{top}$")))))
  if (PEP == F) {
    for(i in idx_d){
      plot(df_vec[[i]][,y] ~ df_vec[[i]][,"PIP"], col = colors, 
           pch = "+", xlab = "PIP", ylab = ylabel, 
           main = bquote("cor("~X[3]~","~X[5]~") = "~.(cor_vec[j])), 
           type = "n", ylim = c(0,max_y), xlim = c(0,1))
      abline(h = 1, lwd = 1.5, lty = 2, col = "#4d4d4d")
      abline(v = .5, lwd = 1.5, lty = 2, col = "#4d4d4d")
      points(df_vec[[i]][,y] ~ df_vec[[i]][,"PIP"], 
             col = colors, pch = "+")
      j <- j + 1
    }
    
    legend("topleft", legend = c("X1", "X2", "X3", "X4", "X5"), pch = rep("+", 5), 
           fill = c("#66c2a5", "#fc8d62", "#8da0cb","#e78ac3","#a6d854"), 
           cex = 1, bty = "n")
  } else {
    for(i in idx_d){
      pep <- 1 - df_vec[[i]][,"PIP"]
      plot(df_vec[[i]][,y] ~ pep, col = colors, 
           pch = "+", xlab = "PEP", ylab = ylabel, 
           main = bquote("cor("~X[3]~","~X[5]~") = "~.(cor_vec[j])), 
           type = "n", ylim = c(0,max_y), xlim = c(0,1))
      abline(h = 1, lwd = 1.5, lty = 2, col = "#4d4d4d")
      abline(v = .5, lwd = 1.5, lty = 2, col = "#4d4d4d")
      points(df_vec[[i]][,y] ~ pep, 
             col = colors, pch = "+")
      j <- j + 1
    }
    legend("topright", legend = c("X1", "X2", "X3", "X4", "X5"), pch = rep("+", 5), 
           fill = c("#66c2a5", "#fc8d62", "#8da0cb","#e78ac3","#a6d854"), 
           cex = 1, bty = "n")
  }
}

plot_indiv_d <- function(df, y, cor_x, p = 5, plot_wind = c(2,3), 
                       color = c("#878787","#878787","#878787", "#4d4d4d","#4d4d4d"), 
                       PEP = F, legend = TRUE){
  par(mfrow = plot_wind)
  dat <- df[[2]]
  pep <- 1- dat$PIP
  xs <- unique(dat$X)
  max_y <- max(dat[,y], na.rm = T) + sd(dat[,y], na.rm = T)/2
  ylabel <- ifelse(y == "d", latex2exp::TeX("$Var_{MA}$ / $Var_{cts}$"),
                   ifelse(y == "d_topCts", latex2exp::TeX("$Var_{cts}$ / $Var_{top}$"),
                          latex2exp::TeX("$Var_{MA}$ / $Var_{top}$")))
  if(PEP == F){
    for(i in 1:p){
      pdat <- subset(dat, X == xs[i])
      plot(pdat[,y] ~ pdat[,"PIP"], 
           xlab = "PIP", xlim = c(0,1), 
           ylim = c(0,max_y),
           ylab = ylabel, 
           main = bquote(X[.(i)]), 
           type = "n")
      abline(h = 1, lwd = 1.5, lty = 2, col = "#b2182b")
      abline(v = 0.5, lwd = 1.5, lty = 2, col = "#b2182b")
      points(pdat[,y] ~ pdat[,"PIP"], 
             pch = "+", 
             col = color[i])
    }
  } else {
    for(i in 1:p){
      pdat <- subset(dat, X == xs[i])
      pep <- 1 - pdat[,"PIP"]
      plot(pdat[,y]  ~ pep, 
           xlab = "PEP", 
           xlim = c(0,1), 
           ylim = c(0, max_y),
           ylab = ylabel, 
           main = bquote(X[.(i)]), 
           type = "n")
      abline(h = 1, lwd = 1.5, lty = 2, col = "#b2182b")
      abline(v = 0.5, lwd = 1.5, lty = 2, col = "#b2182b")
      points(pdat[,y]  ~ pep,
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



plot_all_compare_d <- function(df_vec, y, 
                               cor_vec = c(0.0,0.3,0.6,0.7,0.8,0.9), 
                               plot_wind = c(2,3), 
                               PEP = F){
  par(mfrow = plot_wind)
  xs <- unique(df_vec[[2]][,"X"])
  nsims <- dim(df_vec[[2]])[1]/length(xs)
  colors <- rep(c("#66c2a5", "#fc8d62", 
                  "#8da0cb","#e78ac3",
                  "#a6d854"), each = nsims)
  ylabel <- ifelse(y == "d", latex2exp::TeX("$Var_{MA}$ / $Var_{cts}$"),
                   ifelse(y == "d_topCts", 
                          latex2exp::TeX("$Var_{cts}$ / $Var_{top}$"),
                          ifelse(y == "d_topMA", 
                                 latex2exp::TeX("$Var_{MA}$ / $Var_{top}$"), 
                                 ifelse(y == "diff_ct", 
                                        latex2exp::TeX("$SD_{cts} - SD_{top}$"), 
                                        latex2exp::TeX("$SD_{MA} - SD_{top}$")))))
  idx_d <- which(attributes(df_vec)$names == "summary")
  max_y <- max(sapply(df_vec[idx_d], function(x){
    return(max(x[,y], na.rm = T))
  })) + sd(sapply(df_vec[idx_d], function(x){
    return(max(x[,y], na.rm = T))
  }))/2
  idx_d <- idx_d[1:6]
  j <- 1
  if ( PEP == F ){
    for(i in idx_d){
      plot(df_vec[[i]][,y] ~ df_vec[[i]][,"PIP"], col = colors, 
           pch = "+", xlab = "PIP", ylab = ylabel, 
           main = bquote("cor("~X[3]~","~X[5]~") = "~.(cor_vec[j])), 
           type = "n", ylim = c(0,max_y))
      abline(h = 1, lwd = 2, lty = 2, col = "#4d4d4d")
      points(df_vec[[i]][,y] ~ df_vec[[(i)]][,"PIP"], 
             col = colors, pch = "+")
      points(df_vec[[(i+12)]][,y] ~ df_vec[[(i+12)]][,"PIP"], 
             col = colors, pch = "o") 
      j <- j + 1
    }
  } else {
    for(i in idx_d){
      pep <- 1 - df_vec[[i]][,"PIP"]
      pep2 <- 1 - df_vec[[(i+12)]][,"PIP"]
      plot(df_vec[[i]][,y] ~ pep, col = colors, 
           pch = "+", xlab = "PEP", ylab = ylabel, 
           main = bquote("cor("~X[3]~","~X[5]~") = "~.(cor_vec[j])), 
           type = "n", ylim = c(0,max_y))
      abline(h = 1, lwd = 2, lty = 2, col = "#4d4d4d")
      points(df_vec[[i]][,y] ~ pep, 
             col = colors, pch = "+")
      points(df_vec[[(i+12)]][,y] ~ pep2, 
             col = colors, pch = "o") 
      j <- j + 1
    }
  }
  
  legend("topleft", legend = c("X1", "X2", "X3", "X4", "X5"), 
         fill = c("#66c2a5", "#fc8d62", "#8da0cb","#e78ac3","#a6d854"), 
         cex = 1, bty = "n")
}


plot_y_compare <- function(df, y_vec, cor_x, 
                               color1  = c("#878787","#878787",
                                           "#878787","#4d4d4d",
                                           "#4d4d4d"), 
                               color2 = c("#b2abd2","#b2abd2",
                                          "#b2abd2", "#8073ac",
                                          "#8073ac"), 
                               p = 5, labs = NULL, 
                           plot_wind = c(2,3), PEP = F){
  par(mfrow = plot_wind)
  dat <- df[[2]]
  dat$pep <- 1- dat$PIP
  xs <- unique(dat$X)
  num_x <- dim(dat)[1]/length(xs)
  if(length(xs) < 5){
    xs <- c(xs, NA)
    m <- data.frame(rep(paste0("X", which(is.na(xs))), num_x), 
                    rep(NA, num_x), rep(NA, num_x), rep(NA, num_x))
    names(m) <- names(dat)
    dat <- rbind(dat,m)
  }
  max_y <- max(apply(dat[,y_vec], 2, function(x){
    my <- max(x, na.rm = T)
    sy <- sd(x, na.rm = T)/2
    return(my + sy)
  }))

  if(PEP == F){
    for(i in 1:p){
      pdat <- subset(dat, X == xs[i])
      plot(pdat[,y_vec[1]] ~ pdat[,"PIP"],  
           pch = "+", xlab = "PIP", xlim = c(0,1), 
           ylim = c(0,max_y),
           ylab = "d",
           main = bquote(X[.(i)]),  type = "n")
      abline(h = 1, lwd = 1, lty = 2, col = "#b2182b")
      abline(v = 0.5, lwd = 1, lty = 2, col = "#b2182b")
      points(pdat[,y_vec[1]] ~  pdat[,"PIP"], 
             pch = "+", col = color1[i], cex = 1.5)
      points(pdat[,y_vec[1]] ~ pdat[,"PIP"], 
             pch = 20, col = color1[i])
      points(pdat[,y_vec[2]] ~  pdat[,"PIP"],
             pch = "o", col = color2[i])
    }
  } else {
    for(i in 1:p){
      pdat <- subset(dat, X == xs[i])
      pep <-  1 - pdat[, "PIP"]
      plot(pdat[, y_vec[1]] ~ pep,
           pch = "+", xlab = "PEP", xlim = c(0,1), 
           ylim = c(0,max_y),
           ylab = "d",
           main = bquote(X[.(i)]),  type = "n")
      abline(h = 1, lwd = 1, lty = 2, col = "#b2182b")
      abline(v = 0.5, lwd = 1, lty = 2, col = "#b2182b")
      points(pdat[, y_vec[1]]  ~ pep, 
             pch = "+", col = color1[i], cex = 1.5)
      points(pdat[, y_vec[1]] ~ pep, 
             pch = 20, col = color1[i])
      points(pdat[, y_vec[2]] ~ pep, 
             pch = "o", col = color2[i])
    }
  }
  plot(c(1,1), xaxt = "n", yaxt = "n", xlim = c(0,1), ylim = c(0,3.2), 
       type = "n", xlab = "", ylab = "")
  text(x = 0.5, y = 1.5, bquote("cor("~X[3]~","~X[5]~") = "~.(cor_x)))
  if (!is.null(labs)) {
    legend("topleft", 
           legend = labs,
           pch = c("+", "o"), 
           col = c("#4d4d4d", "#8073ac"), bty = "n")
  }
}



plot_trt_compare <- function(df_vec, y, cor_x, 
                           color1  = c("#878787","#878787",
                                       "#878787","#4d4d4d",
                                       "#4d4d4d"), 
                           color2 = c("#b2abd2","#b2abd2",
                                      "#b2abd2", "#8073ac",
                                      "#8073ac"), 
                           p = 5, labs = NULL, 
                           plot_wind = c(2,3), PEP = F){
    par(mfrow = plot_wind)
    dat <- df_vec[[2]]
    dat2 <- df_vec[[4]]
    xs <- unique(dat$X)
    xs2 <- unique(dat2$X)
    num_x <- dim(dat)[1]/length(xs)
    num_vars <- dim(dat)[2]
    
    ylabel <- ifelse(y == "d", latex2exp::TeX("$Var_{MA}$ / $Var_{cts}$"),
                     ifelse(y == "d_topCts", 
                            latex2exp::TeX("$Var_{cts}$ / $Var_{top}$"),
                            ifelse(y == "d_topMA", 
                                   latex2exp::TeX("$Var_{MA}$ / $Var_{top}$"), 
                                   ifelse(y == "diff_ct", 
                                          latex2exp::TeX("$SD_{cts} - SD_{top}$"), 
                                          latex2exp::TeX("$SD_{MA} - SD_{top}$")))))
    
    if(length(xs) < 5){
      xs <- c(xs, NA)
      m <- data.frame(rep(paste0("X", which(is.na(xs2))), num_x), 
                      matrix(NA, ncol = num_vars-1, nrow = num_x))
      names(m) <- names(dat2)
      dat <- rbind(dat,m)
    }
    if(length(xs2) < 5){
      xs2 <- c(xs2, NA)
      m <- data.frame(rep(paste0("X", which(is.na(xs2))), num_x), 
                      matrix(NA, ncol = num_vars-1, nrow = num_x))
      names(m) <- names(dat2)
      dat2 <- rbind(dat2,m)
    }
    idx_d <- which(attributes(df_vec)$names == "summary")
    max_y <- max(sapply(df_vec[idx_d], function(x){
      return(max(x[,y], na.rm = T))
    })) + sd(sapply(df_vec[idx_d], function(x){
      return(max(x[,y], na.rm = T))
    }))/2
    if(PEP == F){
      for(i in 1:p){
        pdat <- subset(dat, X == xs[i])
        pdat2 <- subset(dat2, X == xs[i])
        plot(dat[,y] ~ dat[,"PIP"],
             pch = "+", xlab = "PIP",
             xlim = c(0,1), 
             ylim = c(0, max_y),
             ylab = ylabel, 
             main = bquote(X[.(i)]),  type = "n")
        abline(h = 1, lwd = 1, lty = 2, col = "#b2182b")
        abline(v = 0.5, lwd = 1, lty = 2, col = "#b2182b")
        points(pdat[,y] ~ pdat[,"PIP"],  
               pch = "+", col = color1[i], cex = 1.5)
        points(pdat[,y] ~ pdat[,"PIP"], 
               pch = 20, col = color1[i])
        points(pdat2[,y] ~ pdat2[,"PIP"], 
               pch = "o", col = color2[i])
      }
    } else {
      for(i in 1:p){
        pdat <- subset(dat, X == xs[i])
        pep <- 1 - pdat[,"PIP"]
        pdat2 <- subset(dat2, X == xs[i])
        pep2 <- 1 - pdat2[,"PIP"]
        plot(pdat[,y] ~ pep,
             pch = "+", xlab = "PEP",
             xlim = c(0,1), 
             ylim = c(0, max_y),
             ylab = ylabel, 
             main = bquote(X[.(i)]),  type = "n")
        abline(h = 1, lwd = 1, lty = 2, col = "#b2182b")
        abline(v = 0.5, lwd = 1, lty = 2, col = "#b2182b")
        points(pdat[,y] ~ pep,  
               pch = "+", col = color1[i], cex = 1.5)
        points(pdat[,y] ~ pep, 
               pch = 20, col = color1[i])
        points(pdat2[,y] ~ pep2, 
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
