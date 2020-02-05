######################################################
# Functions for comparing d_topMA to d_topCTS, etc.  #
######################################################
# beanplot comparison of quantities of interest
count_fun <- function(x){
  out <- data.frame(y = .95*x$max_y, 
                    label = paste("n = ", length(!is.na(x[,y]))))
  return(out)
}
compare_varMActs <- function(df_vec, y){
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
  hline <- ifelse(y == "PIP", 0.5, 
                  ifelse(y == "diff_ct" | y == "diff_MAt", 0, 1))
  df_plot <- ggprocess(df_vec, y = y)
  idx_in <- which(names(df_plot) %in% c("X", "cor_x", y, "max_y"))
  df_sub <- df_plot[,idx_in]
  df_sub <- df_sub[complete.cases(df_sub),]
  df_summ <- df_sub %>% dplyr::group_by(X, cor_x) %>% 
    dplyr::summarise(n_sim = dplyr::n(), y = max(max_y))
  ggplot2::ggplot(df_plot, ggplot2::aes(x = X, y = df_plot[,y], col = X)) + 
    ggplot2::geom_label(data = df_summ, aes(x = X, y = max(df_summ$y), 
                                            label = n_sim), 
                        col = "black", size = 4) +
    ggplot2::geom_violin(na.rm = TRUE) + 
    ggplot2::geom_point(size = 0.75, alpha = 0.2, na.rm = TRUE) +
    ggplot2::stat_summary(fun.y = median, geom = "point", 
                          pch = "-", size=10, na.rm = T) + 
    ggplot2::ylab(yt) + 
    ggplot2::facet_wrap(~cor_x) +
    ggplot2::scale_color_manual(values = c("#878787","#878787",
                                           "#878787", "#4d4d4d","#4d4d4d")) + 
    ggplot2::geom_hline(yintercept = hline, lty = 2, col = "#b2182b") + 
    # ggplot2::stat_summary(fun.data = count_fun, 
                           # geom = "text") + 
    ggplot2::theme_bw(base_size = 14) + 
    ggplot2::theme(legend.position = "n") 
}

# plot_all_d, and plot_all_compare_d depreciated - use many_sim_viz
# plot_indiv_d depreciated - use indiv_x_viz

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


#########################################
##     Revised plotting functions      ##
#########################################

process_fun <- function(df_vec, y){
  xs <- unique(df_vec[[2]][,1])
  nsims <- dim(df_vec[[2]])[1]/length(xs)
  colors <- rep(c('#c7e9b4', '#7fcdbb', 
                  '#41b6c4', '#2c7fb8',
                  '#253494')[1:length(xs)], 
                each = nsims)
  idx_d <- which(attributes(df_vec)$names == "summary")
  max_y <- sapply(df_vec[idx_d], function(x){
    return(max(x[,y], na.rm = T))
  })
  max_y <- max(max_y) + sd(max_y)/2
  for(i in idx_d){
    pep <- 1 - df_vec[[i]][,"PIP"]
    df_vec[[i]]$pep <- pep
    df_vec[[i]]$colors <- colors
    df_vec[[i]]$max_y <- max_y
  }
  return(df_vec) 
}

process_trt <- function (df_comp, y) {
  
  dat <- df_comp[[2]]
  dat2 <- df_comp[[4]]
  xs <- unique(dat$X)
  xs2 <- unique(dat2$X)
  num_x <- dim(dat)[1]/length(xs)
  num_vars <- dim(dat)[2]
  
  if(length(xs) < 5){
    xs <- c(xs, NA)
    m <- data.frame(rep(paste0("X", 
                               which(is.na(xs2))), num_x), 
                    matrix(NA, ncol = num_vars-1, nrow = num_x))
    names(m) <- names(dat2)
    dat <- rbind(dat,m)
  }
  if(length(xs2) < 5){
    xs2 <- c(xs2, NA)
    m <- data.frame(rep(paste0("X", 
                               which(is.na(xs2))), num_x), 
                    matrix(NA, ncol = num_vars-1, nrow = num_x))
    names(m) <- names(dat2)
    dat2 <- rbind(dat2,m)
  }
  df_comp <- list(dat, dat2)
  nsims <- dim(df_comp[[2]])[1]/length(xs)
  colors1  = rep(c("#d0d1e6","#d0d1e6", 
                   "#d0d1e6","#a6bddb", 
                   "#a6bddb"), 
                 each = nsims)
  colors2 = rep(c("#0570b0","#0570b0",
                  "#0570b0", "#045a8d",
                  "#045a8d"), 
                each = nsims)
  max_y <- sapply(df_comp, function(x){
    return(max(x[,y], na.rm = T))
  })
  max_y <- max(max_y) + sd(max_y)/2
  
  for(i in 1:2){
    pep <- 1 - df_comp[[i]][,"PIP"]
    df_comp[[i]]$pep <- pep
    if(i == 1){
      df_comp[[i]]$colors <- colors1
    } else {
      df_comp[[i]]$colors <- colors2
    }
    df_comp[[i]]$max_y <- max_y
  }
  return(df_comp) 
}

process_y <- function (df, y_vec) {
  
  dat <- df[[2]]
  xs <- unique(dat$X)
  nsims <- dim(dat)[1]/length(xs)
  colors1  = rep(c("#d0d1e6","#d0d1e6", 
                   "#d0d1e6","#a6bddb", 
                   "#a6bddb"), each = nsims)
  colors2 = rep(c("#0570b0","#0570b0",
                  "#0570b0", "#045a8d",
                  "#045a8d"), each = nsims)
  max_y <- apply(dat[,y_vec], 2, max, na.rm = T)
  max_y <- max(max_y) + sd(max_y)/2
  for(i in 1:2){
    pep <- 1 - dat[,"PIP"]
    dat$pep <- pep
    dat$colors1 <- colors1
    dat$colors2 <- colors2
    dat$max_y <- max_y
  }
  return(dat) 
}

pres_d_plot <- function(df_vec, y = "d", 
                        mar_list = list(c(.25,4,4,0), c(.25,2,4, 2), 
                                        c(.25,0,4,4), c(4,4,.25,0), 
                                        c(4,2,.25, 2), c(4,0,.25,4)),
                        m = rbind(c(1,2,3), c(4,5,6)),
                        cor_vec = c(0.0,0.3,0.6,0.7,0.8,0.9),
                        n_var = 5, 
                        plot_order = "forward", 
                        lab = "top", y_max = NULL){
  df_vec <- process_fun(df_vec, y = y)
  idx_d <- which(attributes(df_vec)$names == "summary")
  ylabel <- ifelse(y == "d", latex2exp::TeX("$Var_{MA}$ / $Var_{cts}$"),
                   ifelse(y == "d_topCts", 
                          latex2exp::TeX("$Var_{cts}$ / $Var_{top}$"),
                          ifelse(y == "d_topMA", 
                                 latex2exp::TeX("$Var_{MA}$ / $Var_{top}$"),
                                 ifelse(y == "diff_ct", 
                                        latex2exp::TeX("$SD_{cts} - SD_{top}$"),
                                        latex2exp::TeX("$SD_{MA} - SD_{top}$"))
                          )))
  layout(m)							
  xs <- unique(df_vec[[2]][,1])
  j <- 1
  # cex_x <- c(5,4.5,4,3.5,3) 
  alpha_x <- 255*rep(0.4,5)
  for(i in idx_d){
    
    # par(mar = mar_list[[j]], bg = "#f0f0f0")
    par(mar = mar_list[[j]], bg = "#ffffff")
    dat <- df_vec[[i]]
    y_max <- ifelse(is.null(y_max), dat$max_y[1], y_max)
    plot(dat[,y] ~ dat$pep, 
         pch = 20,  
         type = "n", ylim = c(0,y_max), 
         xlim = c(0,1), 
         xaxt = "n",
         yaxt = "n", xlab = "", ylab = "")
    abline(h = 1, lwd = 1.5, lty = 2, col = "#4d4d4d")
    abline(v = .5, lwd = 1.5, lty = 2, col = "#4d4d4d")
    if (lab == "top") {
      text(x = 0.2, y = y_max, 
           paste("Cor = ", cor_vec[j]), pos = 1, cex = 2)
    } else {
      text(x = 0.2, y = 0.2, 
           paste("Cor = ", cor_vec[j]), pos = 1, cex = 2)
    }
    # add axes 
    if ( j %in% c(1,4)) {
      axis(side = 2, 
           labels = as.character(round(seq(0, y_max, length = 11), 2)), 
           at = seq(0, y_max, length = 11), tick = T, cex.axis = 1.75)
      mtext(ylabel, 2, line = 2.5)
    }
    if (j %in% c(4,5,6)){
      axis(side = 1, at = seq(0, 1, length = 11), tick = T, 
           cex.axis = 1.75)
      mtext("PEP", 1, line = 2.5)
    }
    if( plot_order == "forward") {
      for(v in 1:n_var) {
        dp <- subset(dat, dat$X == xs[v])
        p_col <- col2rgb(unique(dp$colors))
        p_col <- rgb(red = p_col[1,], green = p_col[2,], 
                     blue = p_col[3,], alpha = alpha_x[v],
                     maxColorValue = 255)
        points(dp[,y] ~ dp$pep, 
               col = p_col, 
               pch = 20, cex = 3.5)
      }      	
    } else {
      for(v in n_var:1) {
        dp <- subset(dat, dat$X == xs[v])
        p_col <- col2rgb(unique(dp$colors))
        p_col <- rgb(red = p_col[1,], green = p_col[2,], 
                     blue = p_col[3,], alpha = alpha_x[v], 
                     maxColorValue = 255)
        points(dp[,y] ~ dp$pep, 
               col = p_col, 
               pch = 20, cex = 3.5)
      }      
      
    }
    j <- j + 1	
  }
  leg <- c(expression(X[1]),expression(X[2]),
           expression(X[3]), expression(X[4]), expression(X[5]))
  fill <- c('#c7e9b4', '#7fcdbb', '#41b6c4', '#2c7fb8','#253494')
  legend("topright", legend = leg[1:n_var], pch = rep("+", n_var), 
         fill = fill[1:n_var], 
         cex = 1.75, bty = "n")														
}



pres_trt_comp <- function(df_comp, y, cor_x, 
                          mar_list = list(c(.25,4,4,0), c(.25,2,4, 2), 
                                          c(.25,0,4,4), c(4,4,.25,0), c(4,2,.25, 2), 
                                          c(4,0,.25,4)),
                          m = rbind(c(1,2,3), c(4,5,6)),
                          n_var = 5, 
                          p = 5, labs = NULL, y_max = NULL){
  
  ylabel <- ifelse(y == "d", latex2exp::TeX("$Var_{MA}$ / $Var_{cts}$"),
                   ifelse(y == "d_topCts", 
                          latex2exp::TeX("$Var_{cts}$ / $Var_{top}$"),
                          ifelse(y == "d_topMA", 
                                 latex2exp::TeX("$Var_{MA}$ / $Var_{top}$"),
                                 ifelse(y == "diff_ct", 
                                        latex2exp::TeX("$SD_{cts} - SD_{top}$"),
                                        latex2exp::TeX("$SD_{MA} - SD_{top}$"))
                          )))
  layout(m)
  df_comp <- process_trt(df_comp, y = y)					
  dat <- df_comp[[1]]
  dat2 <- df_comp[[2]]
  xs <- unique(df_comp[[2]][,1])
  alpha_x <- 255*rep(0.4,5)	
  
  j <- 1
  leg <- c(expression(X[1]),expression(X[2]),
           expression(X[3]), expression(X[4]), expression(X[5]))
  for(i in 1:p){
    par(mar = mar_list[[j]], bg = "#ffffff")
    pdat <- subset(dat, X == xs[i])
    pdat2 <- subset(dat2, X == xs[i])
    y_max <- ifelse(is.null(y_max), pdat$max_y[1], y_max)
    plot(pdat[,y] ~ pdat$pep, 
         pch = 20,  
         type = "n", ylim = c(0,y_max), 
         xlim = c(0,1), 
         xaxt = "n",
         yaxt = "n", xlab = "", ylab = "")
    abline(h = 1, lwd = 1.5, lty = 2, col = "#4d4d4d")
    abline(v = .5, lwd = 1.5, lty = 2, col = "#4d4d4d")
    text(x = 0.8, y = pdat$max_y[1], leg[i], pos = 1, cex = 2)
    # add axes 
    if ( j %in% c(1,4)) {
      axis(side = 2, 
           labels = as.character(round(seq(0, y_max, length = 11), 2)), 
           at = seq(0, y_max, length = 11), tick = T, cex.axis = 1.75)
      mtext(ylabel, 2, line = 2.5)
    }
    if (j %in% c(4,5,6)){
      axis(side = 1, at = seq(0, 1, length = 11), tick = T, cex.axis = 1.75)
      mtext("PEP", 1, line = 2.5)
    }
    
    p_col <- col2rgb(unique(pdat$colors))
    p_col <- rgb(red = p_col[1,], green = p_col[2,], 
                 blue = p_col[3,], alpha = alpha_x[i], 
                 maxColorValue = 255)    
    p_col2 <- col2rgb(unique(pdat2$colors))
    p_col2 <- rgb(red = p_col2[1,], green = p_col2[2,], 
                  blue = p_col2[3,], alpha = alpha_x[i], 
                  maxColorValue = 255) 
    points(pdat[,y] ~ pdat$pep, 
           col = p_col, 
           pch = 20, cex = 3.5)
    points(pdat2[,y] ~ pdat2$pep, 
           col = p_col2, 
           pch = 18, cex = 3.5)
    j <- j + 1
  }      	
  par(mar = mar_list[[j]], bg = "#ffffff")     				    
  plot(c(1,1), xaxt = "n", yaxt = "n", xlim = c(0,1), ylim = c(0,3.2), 
       type = "n", xlab = "", ylab = "")
  text(x = 0.5, y = 1.5, 
       bquote("cor("~X[3]~","~X[5]~") = "~.(cor_x)), cex = 2)
  col1 <- unique(dat$colors)[2]
  col2 <- unique(dat2$colors)[2]
  col1 <- rgb(red = col2rgb(col1)[1,], 
              green = col2rgb(col1)[2,],
              blue = col2rgb(col1)[3,], 
              alpha = alpha_x[5], maxColorValue = 255)
  col2 <- rgb(red = col2rgb(col2)[1,], 
              green = col2rgb(col2)[2,],
              blue = col2rgb(col2)[3,], 
              alpha = alpha_x[5], maxColorValue = 255)
  if (!is.null(labs)) {
    labels <- c(labs[1], labs[2])
    legend("top", 
           legend = labels,
           pch = c(20, 18), 
           col = c(col1,col2), bty = "n", cex = 2)
  }
}


pres_y_comp <- function(df, y_vec, cor_x, 
                        mar_list = list(c(.25,4,4,0), c(.25,2,4, 2), 
                                        c(.25,0,4,4), c(4,4,.25,0), c(4,2,.25, 2), 
                                        c(4,0,.25,4)),
                        m = rbind(c(1,2,3), c(4,5,6)),
                        n_var = 5, 
                        p = 5, labs = NULL,
                        y_max = NULL){
  
  layout(m)
  df <- process_y(df, y_vec = y_vec)					
  dat <- df
  xs <- unique(df$X)
  alpha_x <- 255*rep(0.4,5)	
  leg <- c(expression(X[1]),expression(X[2]),
           expression(X[3]), expression(X[4]), expression(X[5]))	
  j <- 1
  
  for(i in 1:p){
    par(mar = mar_list[[j]], bg = "#ffffff")
    pdat <- subset(dat, X == xs[i])
    y_max <- ifelse(is.null(y_max), pdat$max_y[1], y_max)
    plot(pdat[,y_vec[1]] ~ pdat$pep, 
         pch = 20,  
         type = "n", ylim = c(0,y_max), 
         xlim = c(0,1), 
         xaxt = "n",
         yaxt = "n", xlab = "", ylab = "")
    abline(h = 1, lwd = 1.5, lty = 2, col = "#4d4d4d")
    abline(v = .5, lwd = 1.5, lty = 2, col = "#4d4d4d")
    text(x = 0.8, y = y_max, leg[i], pos = 1, cex = 2)
    # add axes 
    if ( j %in% c(1,4)) {
      axis(side = 2, 
           labels = as.character(round(seq(0, y_max, length = 11), 2)), 
           at = seq(0, y_max, length = 11), tick = T, cex.axis = 1.75)
    }
    if (j %in% c(4,5,6)){
      axis(side = 1, at = seq(0, 1, length = 11), tick = T, cex.axis = 1.75)
      mtext("PEP", 1, line = 2.5)
    }
    
    p_col <- col2rgb(unique(pdat$colors1))
    p_col <- rgb(red = p_col[1,], green = p_col[2,], 
                 blue = p_col[3,], alpha = alpha_x[i], 
                 maxColorValue = 255)    
    p_col2 <- col2rgb(unique(pdat$colors2))
    p_col2 <- rgb(red = p_col2[1,], green = p_col2[2,], 
                  blue = p_col2[3,], alpha = alpha_x[i], 
                  maxColorValue = 255) 
    points(pdat[,y_vec[1]] ~ pdat$pep, 
           col = p_col, 
           pch = 20, cex = 3.5)
    points(pdat[,y_vec[2]] ~ pdat$pep, 
           col = p_col2, 
           pch = 18, cex = 3.5)
    j <- j + 1
  }      	
  par(mar = mar_list[[j]], bg = "#ffffff")     				    
  plot(c(1,1), xaxt = "n", yaxt = "n", xlim = c(0,1), ylim = c(0,3.2), 
       type = "n", xlab = "", ylab = "")
  text(x = 0.5, y = 1.5, bquote("cor("~X[3]~","~X[5]~") = "~.(cor_x)), 
       cex = 2.5, font = 2, col = "black")
  col1 <- unique(dat$colors1)[2]
  col2 <- unique(dat$colors2)[2]
  col1 <- rgb(red = col2rgb(col1)[1,], 
              green = col2rgb(col1)[2,],
              blue = col2rgb(col1)[3,], 
              alpha = alpha_x[5], maxColorValue = 255)
  col2 <- rgb(red = col2rgb(col2)[1,], 
              green = col2rgb(col2)[2,],
              blue = col2rgb(col2)[3,], 
              alpha = alpha_x[5], maxColorValue = 255)
  if (!is.null(labs)) {
    labels <- c(labs[1], labs[2])
    legend("top", 
           legend = labels,
           pch = c(20, 18), 
           col = c(col1,col2), bty = "n", cex = 2)
  }
}



process_y_indiv <- function (df, y) {
  
  dat <- df[[2]]
  xs <- unique(dat$X)
  nsims <- dim(dat)[1]/length(xs)
  colors2 = rep(c("#0570b0","#0570b0",
                  "#0570b0", "#045a8d",
                  "#045a8d"), each = nsims)
  max_y <- max(dat[,y], na.rm = T)
  max_y <- max(max_y) + 0.25
  for(i in 1:2){
    pep <- 1 - dat[,"PIP"]
    dat$pep <- pep
    dat$colors <- colors2
    dat$max_y <- max_y
  }
  return(dat) 
}

pres_y <- function(df, y, cor_x, 
                   mar_list = list(c(.25,4,4,0), c(.25,2,4, 2),
                                   c(.25,0,4,4), c(4,4,.25,0), 
                                   c(4,2,.25, 2), c(4,0,.25,4)),
                   m = rbind(c(1,2,3), c(4,5,6)),
                   n_var = 5, 
                   p = 5, y_max = NULL){
  
  layout(m)
  df <- process_y_indiv(df, y = y)					
  dat <- df
  xs <- unique(df$X)
  alpha_x <- 255*rep(0.4,5)	
  leg <- c(expression(X[1]),expression(X[2]),
           expression(X[3]), expression(X[4]), expression(X[5]))
  ylabel <- ifelse(y == "d", latex2exp::TeX("$Var_{MA}$ / $Var_{cts}$"),
                   ifelse(y == "d_topCts", 
                          latex2exp::TeX("$Var_{cts}$ / $Var_{top}$"),
                          ifelse(y == "d_topMA", 
                                 latex2exp::TeX("$Var_{MA}$ / $Var_{top}$"),
                                 ifelse(y == "diff_ct", 
                                        latex2exp::TeX("$SD_{cts} - SD_{top}$"),
                                        latex2exp::TeX("$SD_{MA} - SD_{top}$"))
                          )))
  j <- 1
  
  for(i in 1:p){
    par(mar = mar_list[[j]], bg = "#ffffff")
    pdat <- subset(dat, X == xs[i])
    y_max <- ifelse(is.null(y_max), pdat$max_y[1], y_max)
    plot(pdat[,y] ~ pdat$pep, 
         pch = 20,  
         type = "n", ylim = c(0,y_max), 
         xlim = c(0,1), 
         xaxt = "n",
         yaxt = "n", xlab = "", ylab = "")
    abline(h = 1, lwd = 1.5, lty = 2, col = "#4d4d4d")
    abline(v = .5, lwd = 1.5, lty = 2, col = "#4d4d4d")
    text(x = 0.8, y = y_max, leg[i], pos = 1, cex = 2)
    # add axes 
    if ( j %in% c(1,4)) {
      axis(side = 2, 
           labels = as.character(round(seq(0, y_max, length = 11), 2)), 
           at = seq(0, y_max, length = 11), tick = T, cex.axis = 1.75)
      mtext(ylabel, 2, line = 2.5)
    }
    if (j %in% c(4,5,6)){
      axis(side = 1, at = seq(0, 1, length = 11), tick = T, cex.axis = 1.75)
      mtext("PEP", 1, line = 2.5)
    }
    
    p_col <- col2rgb(unique(pdat$colors))
    p_col <- rgb(red = p_col[1,], green = p_col[2,], 
                 blue = p_col[3,], alpha = alpha_x[i], 
                 maxColorValue = 255)    
    points(pdat[,y] ~ pdat$pep, 
           col = p_col, 
           pch = 20, cex = 3.5)
    j <- j + 1
  }      	
  par(mar = mar_list[[j]], bg = "#ffffff")     				    
  plot(c(1,1), xaxt = "n", yaxt = "n", xlim = c(0,1), ylim = c(0,3.2), 
       type = "n", xlab = "", ylab = "")
  text(x = 0.5, y = 1.5, bquote("cor("~X[3]~","~X[5]~") = "~.(cor_x)), 
       cex = 2.5, font = 2, col = "black")
}

final_bean_d <- function(df_vec, y, 
                         cor_vec = c(0, 0.3, 0.6, 0.7, 0.8, 0.9), 
                         mar_list = list(c(.25,4,4,0), c(.25,2,4, 2), 
                                         c(.25,0,4,4), c(4,4,.25,0), 
                                         c(4,2,.25, 2), c(4,0,.25,4)), 
                         m = rbind(c(1,2,3), c(4,5,6))){
  layout(m)
  idx_d <- which(attributes(df_vec)$names == "summary")
  max_y <- sapply(df_vec[idx_d], function(x){
    return(max(x[,y], na.rm = T))})
  min_y <- sapply(df_vec[idx_d], function(x){
    return(min(x[,y], na.rm = T))})
  num_obs <- lapply(df_vec[idx_d], function(x){
    xs <- unique(x$X)
    obs <- rep(NA, 5)
    nas <- rep(NA, 5)
    for(i in 1:5){
      nas[i] <- sum(is.na(subset(x, X == xs[i])[,y]))
      obs[i] <- 300-nas[i]
    }
    return(c(obs))
  })
  min_y <- min(min_y) - 30*sd(min_y)
  xs <- unique(df_vec[[2]][,1])
  nsims <- dim(df_vec[[2]])[1]/length(xs)
  yt <- ifelse(y == "d", latex2exp::TeX("$Var_{MA}$ / $Var_{cts}$"),
               ifelse(y == "d_topCts", 
                      latex2exp::TeX("$Var_{cts}$ / $Var_{top}$"),
                      ifelse(y == "d_topMA", 
                             latex2exp::TeX("$Var_{MA}$ / $Var_{top}$"), 
                             ifelse(y == "diff_ct", 
                                    latex2exp::TeX("$SD_{cts} - SD_{top}$"), 
                                    ifelse(y == "diff_MAt", 
                                           latex2exp::TeX("$SD_{MA} - SD_{top}$"), 
                                           "PIP")
                             ))))
  yl <- ifelse(y == "PIP", 1, 
               ifelse(y == "d_topCts", 1.25, 3.25))
  hline <- ifelse(y == "PIP", NA, 
                  ifelse(y == "diff_ct" | y == "diff_MAt", 0, 1))
  j <- 1
  
  df_vec <- process_fun(df_vec, y = y)
  
  for(i in idx_d){
    par(mar = mar_list[[j]], bg = "#ffffff")
    if(j %in% c(1,2,3)){
      beanplot::beanplot(df_vec[[i]][,y] ~ df_vec[[i]][,1], 
                         yaxt = "n", log = "", xaxt = "n", 
                         ylim = c(min_y, max_y[3] + sd(max_y[1:3])/4))
      text(1:5, min_y, as.character(unlist(num_obs[j])), 
           cex = 1.5, font = 2)
      text(1.75, max_y[3], 
           bquote("Cor("~X[3]~","~X[5]~") = "~.(cor_vec[j])), 
           cex = 1.5, font = 2)
    } else {
      beanplot::beanplot(df_vec[[i]][,y] ~ df_vec[[i]][,1], 
                         yaxt = "n", log = "", xaxt = "n", 
                         ylim = c(min_y, max(max_y)+sd(max_y)/2))
      axis(1, at = 1:5, labels =  c(latex2exp::TeX("$X_1$"), 
                                    latex2exp::TeX("$X_2$"), 
                                    latex2exp::TeX("$X_3$"), 
                                    latex2exp::TeX("$X_4$"),
                                    latex2exp::TeX("$X_5$")), cex.axis = 1.75)
      text(1:5, min_y, as.character(unlist(num_obs[j])), cex = 1.5, font = 2)
      text(1.75, max(max_y), 
           bquote("Cor("~X[3]~","~X[5]~") = "~.(cor_vec[j])), 
           cex = 1.5, font = 2)
    }
    if (j == 1) {
      axis(side = 2, 
           labels = as.character(round(seq(0, max_y[3] + sd(max_y[1:3])/4, 
                                           length = 11), 2)), 
           at = seq(0, max_y[3] + sd(max_y[1:3])/4, length = 11), 
           tick = T, cex.axis = 1.5)
      mtext(yt, 2, line = 2.5)
    }
    if (j == 4) {
      axis(side = 2, 
           labels = as.character(round(seq(0, max(max_y) + sd(max_y)/4, 
                                           length = 11), 2)), 
           at = seq(0,max(max_y) + sd(max_y)/2, length = 11), 
           tick = T, cex.axis = 1.5)
      mtext(yt, 2, line = 2.5)
    }
    abline(h = hline, lwd = 2, lty = 2, col = "#b2182b")
    j <- j + 1
  }
}

final_bean_diff <- function(df_vec, y, 
                            cor_vec = c(0, 0.3, 0.6, 0.7, 0.8, 0.9), 
                            mar_list = list(c(.25,4,4,0), c(.25,2,4, 2), 
                                            c(.25,0,4,4), c(4,4,.25,0), 
                                            c(4,2,.25, 2), c(4,0,.25,4)), 
                            m = rbind(c(1,2,3), c(4,5,6))){
  layout(m)
  idx_d <- which(attributes(df_vec)$names == "summary")
  max_y <- sapply(df_vec[idx_d], function(x){
    return(max(x[,y], na.rm = T))})
  min_y <- sapply(df_vec[idx_d], function(x){
    return(min(x[,y], na.rm = T))})
  num_obs <- lapply(df_vec[idx_d], function(x){
    xs <- unique(x$X)
    obs <- rep(NA, 5)
    nas <- rep(NA, 5)
    for(i in 1:5){
      nas[i] <- sum(is.na(subset(x, X == xs[i])[,y]))
      obs[i] <- 300-nas[i]
    }
    return(c(obs))
  })
  min_y <- min(min_y) - 30*sd(min_y)
  xs <- unique(df_vec[[2]][,1])
  nsims <- dim(df_vec[[2]])[1]/length(xs)
  yt <- ifelse(y == "d", latex2exp::TeX("$Var_{MA}$ / $Var_{cts}$"),
               ifelse(y == "d_topCts", 
                      latex2exp::TeX("$Var_{cts}$ / $Var_{top}$"),
                      ifelse(y == "d_topMA", 
                             latex2exp::TeX("$Var_{MA}$ / $Var_{top}$"), 
                             ifelse(y == "diff_ct", 
                                    latex2exp::TeX("$SD_{cts} - SD_{top}$"), 
                                    ifelse(y == "diff_MAt", 
                                           latex2exp::TeX("$SD_{MA} - SD_{top}$"), 
                                           "PIP")
                             ))))
  yl <- ifelse(y == "PIP", 1, 
               ifelse(y == "d_topCts", 1.25, 3.25))
  hline <- ifelse(y == "PIP", NA, 
                  ifelse(y == "diff_ct" | y == "diff_MAt", 0, 1))
  j <- 1
  
  df_vec <- process_fun(df_vec, y = y)
  
  for(i in idx_d){
    par(mar = mar_list[[j]], bg = "#ffffff")
    if(j %in% c(1,2,3)){
      beanplot::beanplot(df_vec[[i]][,y] ~ df_vec[[i]][,1], 
                         yaxt = "n", log = "", xaxt = "n", 
                         ylim = c(min_y,max(max_y)+sd(max_y)/2))
      text(1:5, min_y, as.character(unlist(num_obs[j])), cex = 1.5, font = 2)
      text(1.75, max(max_y), 
           bquote("Cor("~X[3]~","~X[5]~") = "~.(cor_vec[j])), 
           cex = 1.5, font = 2)
    } else {
      beanplot::beanplot(df_vec[[i]][,y] ~ df_vec[[i]][,1], 
                         yaxt = "n", log = "", xaxt = "n", 
                         ylim = c(min_y, max(max_y)+sd(max_y)/2))
      axis(1, at = 1:5, labels =  c(latex2exp::TeX("$X_1$"), 
                                    latex2exp::TeX("$X_2$"), 
                                    latex2exp::TeX("$X_3$"), 
                                    latex2exp::TeX("$X_4$"),
                                    latex2exp::TeX("$X_5$")), cex.axis = 1.75)
      text(1:5, min_y, as.character(unlist(num_obs[j])), cex = 1.5, font = 2)
      text(1.75, max(max_y), 
           bquote("Cor("~X[3]~","~X[5]~") = "~.(cor_vec[j])), 
           cex = 1.5, font = 2)
    }
    if (j %in% c(1,4)) {
      axis(side = 2, 
           labels = as.character(round(seq(0, max(max_y) + sd(max_y)/4, 
                                           length = 11), 2)), 
           at = seq(0,max(max_y) + sd(max_y)/2, length = 11), 
           tick = T, cex.axis = 1.5)
      mtext(yt, 2, line = 2.5)
    }
    abline(h = hline, lwd = 2, lty = 2, col = "#b2182b")
    j <- j + 1
  }
}

# processing fun
ggprocess <- function(df_vec, cor_vec = c(0.0,0.3,0.6,0.7,0.8,0.9), y = "d", 
                      cols = c('#c7e9b4', '#7fcdbb', '#41b6c4', '#2c7fb8','#253494')){
  idx <- which(names(df_vec) == "summary")
  df_out <- data.frame()
  for(i in 1:length(idx)){
    dat <- df_vec[[idx[i]]]
    dat$max_y <- max(dat[,y], na.rm = T) + sd(dat[,y], na.rm = T)/4
    df_out <- rbind(df_out, dat)
  }
  df_out$col <- ifelse(df_out$X == "X1", cols[1], 
                       ifelse(df_out$X == "X2", cols[2], 
                              ifelse(df_out$X == "X3", cols[3], 
                                     ifelse(df_out$X == "X4", cols[4], cols[5]))))
  df_out$cor_x <- factor(df_out$cor_x) 
  levels(df_out$cor_x) <- paste("Cor(X3,X5) =", cor_vec)
  df_out$iter <- factor(df_out$iter)
  df_out$PEP <- 1-df_out$PIP
  df_out$id <- interaction(df_out$cor_x, df_out$iter)
  return(df_out)
}

# individual iterations on cleaner plot without all simulation results 
many_iter_viz <- function(df_vec, n_iter = 10, y = "d"){
  df_plot <- ggprocess(df_vec)
  n_sim <- max(as.numeric(df_plot$iter))
  idx <- sample(x = 1:n_sim, size = n_iter, rep = F)
  ylabel <- ifelse(y == "d", latex2exp::TeX("$Var_{MA}$ / $Var_{cts}$"),
                   ifelse(y == "d_topCts", 
                          latex2exp::TeX("$Var_{cts}$ / $Var_{top}$"),
                          ifelse(y == "d_topMA", 
                                 latex2exp::TeX("$Var_{MA}$ / $Var_{top}$"), 
                                 ifelse(y == "diff_ct", 
                                        latex2exp::TeX("$SD_{cts} - SD_{top}$"), 
                                        latex2exp::TeX("$SD_{MA} - SD_{top}$")))))
  df_sub <- subset(df_plot, as.numeric(df_plot$iter) %in% idx)
  ggplot2::ggplot(data = df_sub, ggplot2::aes(x = PEP, y = df_sub[,y], 
                                              col = X)) + 
    annotate("rect", xmin = 0, ymin = 1, xmax = 1, 
             ymax = max(df_plot[,y], na.rm = T), alpha = 0.2, 
             fill = "red") + 
    annotate("rect", xmin = 0, ymin = 0, xmax = 0.5, 
             ymax = max(df_plot[,y], na.rm = T), alpha = 0.2, 
             fill = "blue") +
    geom_point(size = 3) + 
    geom_text(label = as.character(df_sub$X), 
              nudge_y = 0.5, nudge_x = 0.1) + 
    facet_grid(iter ~ cor_x) + 
    scale_color_manual(values = c("#fc8d62", "#fc8d62", "#999999",
                                  "#4d4d4d","#4d4d4d")) +
    # scale_shape_manual(values = c(16,17,8,15,18)) + 
    ylab(ylabel) + 
    theme_bw() + 
    theme(legend.position = "n") 
}

# create a plot with the iterations that are good for 
many_iter_viz2 <- function(df_vec, n_iter = 10, y = "d"){
  df_plot <- ggprocess(df_vec)
  n_sim <- max(as.numeric(df_plot$iter))
  idx <- sample(x = 1:n_sim, size = n_iter, rep = F)
  ylabel <- ifelse(y == "d", latex2exp::TeX("$Var_{MA}$ / $Var_{cts}$"),
                   ifelse(y == "d_topCts", 
                          latex2exp::TeX("$Var_{cts}$ / $Var_{top}$"),
                          ifelse(y == "d_topMA", 
                                 latex2exp::TeX("$Var_{MA}$ / $Var_{top}$"), 
                                 ifelse(y == "diff_ct", 
                                        latex2exp::TeX("$SD_{cts} - SD_{top}$"), 
                                        latex2exp::TeX("$SD_{MA} - SD_{top}$")))))
  df_sub <- subset(df_plot, as.numeric(df_plot$iter) %in% idx)
  ggplot2::ggplot(data = df_sub, ggplot2::aes(x = PEP, y = df_sub[,y], 
                                              shape = X, col = as.numeric(iter))) + 
    annotate("rect", xmin = 0, ymin = 1, xmax = 1, 
             ymax = max(df_plot[,y], na.rm = T), alpha = 0.4, 
             fill = "gray") + 
    annotate("rect", xmin = 0, ymin = 0, xmax = 0.5, 
             ymax = max(df_plot[,y], na.rm = T), alpha = 0.4, 
             fill = "gray") +
    geom_point(size = 3) + 
    # geom_line(lty = 2, lwd = 0.5) +
    # geom_text(label = as.character(df_sub$X), 
    #           nudge_y = 0.5, nudge_x = 0.1) + 
    facet_wrap(~cor_x, ncol = 6) + 
    scale_shape_manual(values = c(16,17,8,15,18)) + 
    ylab(ylabel) + 
    theme_bw() 
}

# similar to original plotting function with each cor 
# in a panel and all vars in all plots, can use subset 
# of df to look at just one cor, compare some, or all. 
# option to highlight one iteration of simulation to see 
# where the individual PRCs fall under treatment levels
many_sims_viz <- function(df_vec, y = "d", alpha = 0.4, 
                          highlight = FALSE, sim_i = NULL, 
                          plot_order = "backward"){
  if(highlight == TRUE & is.null(sim_i)) {
    message("Must specify an iteration to highlight")
  }
  df_plot <- ggprocess(df_vec = df_vec)
  if(plot_order == "backward"){
    df_plot <- df_plot[order(df_plot[,"X"], decreasing = T), ]  
  } else {
    df_plot <- df_plot[order(df_plot[,"X"]), ]
  }
  ylabel <- ifelse(y == "d", latex2exp::TeX("$Var_{MA}$ / $Var_{cts}$"),
                   ifelse(y == "d_topCts", 
                          latex2exp::TeX("$Var_{cts}$ / $Var_{top}$"),
                          ifelse(y == "d_topMA", 
                                 latex2exp::TeX("$Var_{MA}$ / $Var_{top}$"), 
                                 ifelse(y == "diff_ct", 
                                        latex2exp::TeX("$SD_{cts} - SD_{top}$"), 
                                        latex2exp::TeX("$SD_{MA} - SD_{top}$")))))
  if(highlight == FALSE){
    ggplot2::ggplot(data = df_plot, ggplot2::aes(x = PEP, y = df_plot[,y], col = X)) + 
      annotate("rect", xmin = 0, ymin = 1, xmax = 1, 
               ymax = max(df_plot[,y], na.rm = T), alpha = 0.4, 
               fill = "gray") + 
      annotate("rect", xmin = 0, ymin = 0, xmax = 0.5, 
               ymax = max(df_plot[,y], na.rm = T), alpha = 0.4, 
               fill = "gray") +
      geom_point(size = 3, alpha = alpha, na.rm = T) + 
      facet_wrap(~cor_x, ncol = 6) + 
      ylab(ylabel) + 
      scale_color_manual(values = c('#c7e9b4', '#7fcdbb', '#41b6c4', '#2c7fb8','#253494')) + 
      theme_bw()   + 
      theme(legend.position = "bottom") 
  } else {
    df_plot$iter_num <- as.numeric(df_plot$iter)
    df_highlight <- subset(df_plot, iter_num == sim_i)
    ggplot2::ggplot(data = df_plot, ggplot2::aes(x = PEP, 
                                                 y = df_plot[,y], col = X)) + 
      annotate("rect", xmin = 0, ymin = 1, xmax = 1, 
               ymax = max(df_plot[,y], na.rm = T), alpha = 0.4, 
               fill = "gray") + 
      annotate("rect", xmin = 0, ymin = 0, xmax = 0.5, 
               ymax = max(df_plot[,y], na.rm = T), alpha = 0.4, 
               fill = "gray") +
      geom_point(size = 3, alpha = alpha, na.rm = T) + 
      geom_point(data = df_highlight, 
                 aes(x = PEP, y = df_highlight[,y]), 
                 size = 4, col = "#f0027f", shape = 18) +
      geom_label(data = df_highlight, label = as.character(df_highlight$X), 
                 aes(x = PEP, y = df_highlight[,y]), 
                 nudge_x = 0.1, nudge_y = 0.1) + 
      geom_point(data = df_highlight, 
                 aes(x = PEP, y = df_highlight[,y], col = X), 
                 size = 1, shape = 18) +
      facet_wrap(~cor_x, n_col = 6) + 
      ylab(ylabel) + 
      scale_color_manual(values = c('#c7e9b4', '#7fcdbb', '#41b6c4', '#2c7fb8','#253494')) + 
      theme_bw(base_size = 14) + 
      theme(legend.position = "none")
  }
}

# facet grid instead of facet wrap to put all Cor and Xs in separate plot
# for one treatment
many_sims_full <- function(df_vec, y = "d", alpha = 0.4, 
                          highlight = FALSE, iter_idx = NULL, n_iter = 5){
  df_plot <- ggprocess(df_vec = df_vec)
  if(highlight == FALSE){
    ylabel <- ifelse(y == "d", latex2exp::TeX("$Var_{MA}$ / $Var_{cts}$"),
                     ifelse(y == "d_topCts", 
                            latex2exp::TeX("$Var_{cts}$ / $Var_{top}$"),
                            ifelse(y == "d_topMA", 
                                   latex2exp::TeX("$Var_{MA}$ / $Var_{top}$"), 
                                   ifelse(y == "diff_ct", 
                                          latex2exp::TeX("$SD_{cts} - SD_{top}$"), 
                                          latex2exp::TeX("$SD_{MA} - SD_{top}$")))))
    ggplot2::ggplot(data = df_plot, ggplot2::aes(x = PEP, y = df_plot[,y])) + 
      annotate("rect", xmin = 0, ymin = 1, xmax = 1, 
               ymax = max(df_plot[,y], na.rm = T), alpha = 0.4, 
               fill = "gray") + 
      annotate("rect", xmin = 0, ymin = 0, xmax = 0.5, 
               ymax = max(df_plot[,y], na.rm = T), alpha = 0.4, 
               fill = "gray") +
      geom_point(size = 3, alpha = alpha, na.rm = T) + 
      facet_grid(X~cor_x) + 
      ylab(ylabel) +
      theme_bw()   
  } else {
    df_plot$iter_num <- as.numeric(df_plot$iter)
    if(is.null(iter_idx)) 
      iter_idx <- sample(1:max(df_plot$iter_num), size = n_iter, rep = F)
    df_highlight <- subset(df_plot, iter_num %in% iter_idx)
    ylabel <- ifelse(y == "d", latex2exp::TeX("$Var_{MA}$ / $Var_{cts}$"),
                     ifelse(y == "d_topCts", 
                            latex2exp::TeX("$Var_{cts}$ / $Var_{top}$"),
                            ifelse(y == "d_topMA", 
                                   latex2exp::TeX("$Var_{MA}$ / $Var_{top}$"), 
                                   ifelse(y == "diff_ct", 
                                          latex2exp::TeX("$SD_{cts} - SD_{top}$"), 
                                          latex2exp::TeX("$SD_{MA} - SD_{top}$")))))
    ggplot2::ggplot(data = df_plot, ggplot2::aes(x = PEP, 
                                                 y = df_plot[,y])) + 
      annotate("rect", xmin = 0, ymin = 1, xmax = 1, 
               ymax = max(df_plot[,y], na.rm = T), alpha = 0.4, 
               fill = "gray") + 
      annotate("rect", xmin = 0, ymin = 0, xmax = 0.5, 
               ymax = max(df_plot[,y], na.rm = T), alpha = 0.4, 
               fill = "gray") +
      geom_point(size = 3, alpha = alpha, na.rm = T) + 
      facet_grid(cor_x ~ X) + 
      geom_point(data = df_highlight, 
                 aes(x = PEP, y = df_highlight[,y], col = iter_num), 
                 size = 4, shape = 18, alpha = .9) +
      ylab(ylabel) + 
      scale_color_distiller() +
      theme_bw(base_size = 14) + 
      theme(legend.position = "none")
  }
}

# facet by X and color by correlation for one treatment
many_sims_corviz <- function(df_vec, y = "d", alpha = 0.4, 
                          highlight = FALSE, sim_i = NULL, plot_order = "backward"){
  if(highlight == TRUE & is.null(sim_i)) {
    message("Must specify an iteration to highlight")
  }
  df_plot <- ggprocess(df_vec = df_vec)
  if(plot_order == "backward"){
    df_plot <- df_plot[order(df_plot[,"cor_x"], decreasing = T), ]  
  } else {
    df_plot <- df_plot[order(df_plot[,"cor_x"]), ]
  }
  ylabel <- ifelse(y == "d", latex2exp::TeX("$Var_{MA}$ / $Var_{cts}$"),
                   ifelse(y == "d_topCts", 
                          latex2exp::TeX("$Var_{cts}$ / $Var_{top}$"),
                          ifelse(y == "d_topMA", 
                                 latex2exp::TeX("$Var_{MA}$ / $Var_{top}$"), 
                                 ifelse(y == "diff_ct", 
                                        latex2exp::TeX("$SD_{cts} - SD_{top}$"), 
                                        latex2exp::TeX("$SD_{MA} - SD_{top}$")))))
  if(highlight == FALSE){
    ggplot2::ggplot(data = df_plot, ggplot2::aes(x = PEP, y = df_plot[,y], col = cor_x)) + 
      annotate("rect", xmin = 0, ymin = 1, xmax = 1, 
               ymax = max(df_plot[,y], na.rm = T), alpha = 0.4, 
               fill = "gray") + 
      annotate("rect", xmin = 0, ymin = 0, xmax = 0.5, 
               ymax = max(df_plot[,y], na.rm = T), alpha = 0.4, 
               fill = "gray") +
      geom_point(size = 4, alpha = alpha, na.rm = T) + 
      facet_wrap(~X, ncol = 5) + 
      ylab(ylabel) + 
      # scale_color_manual(values = c("#ffffcc",'#c7e9b4', '#7fcdbb', 
                                    # '#41b6c4', '#2c7fb8','#253494')) + 
      # scale_color_manual(values = c("#d4b9da", "#c994c7", "#df65b0", 
      #                               "#e7298a", "#ce1256", "#980043")) +
      scale_color_manual(values = c("#fed976", "#feb24c", "#fd8d3c",
                                    "#fc4e2a","#e31a1c","#bd0026")) +
      theme_bw() + 
      theme(legend.position = "bottom",  
            # legend.background = element_rect(fill = "darkgray"), 
            legend.title = element_blank())
  } else {
    df_plot$iter_num <- as.numeric(df_plot$iter)
    df_highlight <- subset(df_plot, iter_num == sim_i)
    ggplot2::ggplot(data = df_plot, ggplot2::aes(x = PEP, 
                                                 y = df_plot[,y], col = cor_x)) + 
      annotate("rect", xmin = 0, ymin = 1, xmax = 1, 
               ymax = max(df_plot[,y], na.rm = T), alpha = 0.4, 
               fill = "gray") + 
      annotate("rect", xmin = 0, ymin = 0, xmax = 0.5, 
               ymax = max(df_plot[,y], na.rm = T), alpha = 0.4, 
               fill = "gray") +
      geom_point(size = 3, alpha = alpha, na.rm = T) + 
      geom_point(data = df_highlight, 
                 aes(x = PEP, y = df_highlight[,y]), 
                 size = 4, col = "black", shape = 18) +
      geom_label(data = df_highlight, )
      geom_point(data = df_highlight, 
                 aes(x = PEP, y = df_highlight[,y], col = cor_x), 
                 size = 3, shape = 18) +
      facet_wrap(~X, ncol = 5) + 
      ylab(ylabel) + 
      scale_color_manual(values = c("#fed976", "#feb24c", "#fd8d3c",
                                    "#fc4e2a","#e31a1c","#bd0026")) +
      theme_bw(base_size = 14) + 
      theme(legend.position = "bottom", 
            legend.title = element_blank())
    
  }
}

# Compare treatments for all correlations on one plot
# or just save this info in the df_summ created by sim_d2new...
df_list <- list(df_vec, df_vec_s1)

sims_trt_compare <- function(df_list, trt_labels, y = "d", alpha = 0.4, 
                                               highlight = FALSE, 
                             iter_idx = NULL, n_iter = 5){
  df_plot <- data.frame()
  for(i in 1:length(df_list)){
    df <- ggprocess(df_vec = df_list[[i]])
    df$trt <- trt_labels[i]
    df_plot <- rbind(df_plot, df)
  }
  ylabel <- ifelse(y == "d", latex2exp::TeX("$Var_{MA}$ / $Var_{cts}$"),
                   ifelse(y == "d_topCts", 
                          latex2exp::TeX("$Var_{cts}$ / $Var_{top}$"),
                          ifelse(y == "d_topMA", 
                                 latex2exp::TeX("$Var_{MA}$ / $Var_{top}$"), 
                                 ifelse(y == "diff_ct", 
                                        latex2exp::TeX("$SD_{cts} - SD_{top}$"), 
                                        latex2exp::TeX("$SD_{MA} - SD_{top}$")))))
  if(highlight == FALSE){
    ggplot2::ggplot(data = df_plot, ggplot2::aes(x = PEP, y = df_plot[,y], col = trt)) + 
      annotate("rect", xmin = 0, ymin = 1, xmax = 1, 
               ymax = max(df_plot[,y], na.rm = T), alpha = 0.4, 
               fill = "gray") + 
      annotate("rect", xmin = 0, ymin = 0, xmax = 0.5, 
               ymax = max(df_plot[,y], na.rm = T), alpha = 0.4, 
               fill = "gray") +
      geom_point(size = 3, alpha = alpha, na.rm = T, shape = 18) + 
      facet_grid(cor_x~X) + 
      ylab(ylabel) +
      theme_bw()  
  } else {
    df_plot$iter_num <- as.numeric(df_plot$iter)
    if(is.null(iter_idx)) 
      iter_idx <- sample(1:max(df_plot$iter_num), size = n_iter, rep = F)
    df_highlight <- subset(df_plot, iter_num %in% iter_idx)
    ggplot2::ggplot(data = df_plot, ggplot2::aes(x = PEP, 
                                                 y = df_plot[,y], col = trt)) + 
      annotate("rect", xmin = 0, ymin = 1, xmax = 1, 
               ymax = max(df_plot[,y], na.rm = T), alpha = 0.4, 
               fill = "gray") + 
      annotate("rect", xmin = 0, ymin = 0, xmax = 0.5, 
               ymax = max(df_plot[,y], na.rm = T), alpha = 0.4, 
               fill = "gray") +
      geom_point(size = 3, alpha = alpha, na.rm = T, shape = 18) + 
      facet_grid(cor_x ~ X) + 
      geom_point(data = df_highlight, 
                 aes(x = PEP, y = df_highlight[,y], 
                     shape = iter), 
                 size = 4, col = "black", alpha = .9) +
      ylab(ylabel) + 
      theme_bw(base_size = 14) 
  }
}


