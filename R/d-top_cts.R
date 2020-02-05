######################################################
# Plotting functions for investigating sim results   #
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

# processing fun to take summary data frame from many scenarios 
# and create df for plotting/summarizing results

ggprocess <- function(df_vec, y = "d"){
  idx <- which(names(df_vec) == "summary")
  df_out <- data.frame()
  for(i in 1:length(idx)){
    dat <- df_vec[[idx[i]]]
    dat$max_y <- max(dat[,y], na.rm = T) + sd(dat[,y], na.rm = T)/4
    df_out <- rbind(df_out, dat)
  }
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
      facet_wrap(~cor_x, ncol = 6) + 
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
sims_trt_compare <- function(df_list, trt_labels, y = "d", alpha = 0.4, 
                             cols = c("#ffffb2","#f1a340", 
                                      "#c7e9b4", "#bcbddc", "#f6e8c3","#fa9fb5" )){
  df_plot <- data.frame()
  for(i in 1:length(df_list)){
    df <- ggprocess(df_vec = df_list[[i]], y = y)
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
  n_out <- 6 - length(df_list)
  col_plot <- cols[(n_out+1):6]
    ggplot2::ggplot(data = df_plot, ggplot2::aes(x = PEP, y = df_plot[,y], col = trt)) + 
      annotate("rect", xmin = 0, ymin = 1, xmax = 1, 
               ymax = max(df_plot[,y], na.rm = T), alpha = 0.4, 
               fill = "black") + 
      annotate("rect", xmin = 0, ymin = 0, xmax = 0.5, 
               ymax = max(df_plot[,y], na.rm = T), alpha = 0.4, 
               fill = "black") +
      geom_point(size = 3, alpha = alpha, na.rm = T, shape = 18) + 
      facet_grid(cor_x~X) + 
      ylab(ylabel) +
      # theme_bw()  + 
      theme_dark(base_size = 14) +
      scale_color_manual(values = col_plot) 
}
# all x_s on one plot for each COR, but facet grid by trt label. 
many_trt_viz <- function(df_list, trt_labels, y = "d", alpha = 0.4, 
                          highlight = FALSE, sim_i = NULL, 
                          plot_order = "backward"){
  if(highlight == TRUE & is.null(sim_i)) {
    message("Must specify an iteration to highlight")
  }
  df_plot <- data.frame()
  for(i in 1:length(df_list)){
    df <- ggprocess(df_vec = df_list[[i]], y = y)
    df$trt <- trt_labels[i]
    df_plot <- rbind(df_plot, df)
  }
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
      facet_grid(trt~cor_x) + 
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
      facet_grid(trt~cor_x) + 
      ylab(ylabel) + 
      scale_color_manual(values = c('#c7e9b4', '#7fcdbb', '#41b6c4', '#2c7fb8','#253494')) + 
      theme_bw(base_size = 14) + 
      theme(legend.position = "none")
  }
}


