source('PlantSim_sim.R', echo=TRUE)
# paras = c(
#1   input$num_plot_tab3,
#2   as.numeric(input$num_spe_tab3),
#3   input$sim_time_tab3,
#4   input$growth_rate_tab3,
#5   input$st_portion_tab3,
#6   input$surv_rate_tab3,
#7   input$obs_err_tab3,
#8   input$killrate,
#9   input$con_com_tab3,
#10  input$hetero_com_tab3
# )

biasobs_plot <- function(paras_biasobs) {
  sim_result_list <- list()
  list_count = 1
  num_sims <- 8
  paras <- paras_biasobs
  obs_err_step <- paras_biasobs[7]
  if (obs_err_step == 0) {
    obserror_group <- rep(0, num_sims)
    obserror_group_names <- paste("Obs err ", obserror_group, "rep ", c(1:num_sims))

  } else {
    obserror_group <- seq(0, 1, paras_biasobs[7])
    obserror_group_names <- paste("Obs err ", obserror_group)
  }

  true.paras <- c(paras_biasobs[6],
                  paras_biasobs[4],
                  paras_biasobs[9],
                  paras_biasobs[10])
  surv_rate <- true.paras[1]
  growth_rate <- true.paras[2]
  for (obserr in obserror_group) {
    paras[7] <- obserr
    sim_result_list[[list_count]] <- PlantSim_sim(paras)$all
    list_count <- list_count + 1
  }

  time_snaps <- list()
  tend <- paras[3]
  for (i in c(1:(tend - 1))) {
    time_snaps[[i]] <- c(i, i + 1)
  }


  coefs <- NULL
  corr <- NULL
  nspe <- paras_biasobs[2]
  if ( nspe == 1) {
    for (sim_result in sim_result_list) {
      # remove the data with 0 plants
      row_sub = which(apply(sim_result, 1, function(row) all(row > 0 )))
      ##Subset as usual
      sim_result <- sim_result[row_sub,,, drop = FALSE]
      for (time_snap in time_snaps) {
        data_x <- sim_result[, 1, time_snap[1]]
        data_y <- sim_result[, 1, time_snap[2]]

        lmodel <- lm(log(data_y / data_x) ~ data_x)
        coefs <- rbind(coefs, c(coef(lmodel)[1], coef(lmodel)[2]))
      }
    }
    coefs[,2] <- -coefs[, 2]

  } else {
    for (sim_result in sim_result_list) {
      # remove the data with 0 plants
      row_sub = which(apply(sim_result, 1, function(row) all(row > 0 )))
      ##Subset as usual
      sim_result <- sim_result[row_sub,,, drop = FALSE]
      # estimates for each time snap
      for (time_snap in time_snaps) {
        data_spe1_x <- sim_result[, 1, time_snap[1]]
        data_spe1_y <- sim_result[, 1, time_snap[2]]

        data_spe2_x <- sim_result[, 2, time_snap[1]]
        data_spe2_y <- sim_result[, 2, time_snap[2]]

        lmodel <- lm(log(data_spe1_y / data_spe1_x) ~ data_spe1_x + data_spe2_x)
        coefs <- rbind(coefs, c(coef(lmodel)[1], coef(lmodel)[2], coef(lmodel)[3]))
        corr <- c(corr, cor(data_spe1_x, data_spe2_x))
      }
    }
    coefs[,2] <- -coefs[, 2]
    coefs[,3] <- -coefs[, 3]

  }

  # correct the estimated growth rate
  coefs[, 1] <- exp(coefs[, 1] - 1)/surv_rate
  coefs <- cbind(c(1:(tend - 1)), coefs, corr)

  if (nspe == 1) {
    colnames(coefs) <- c("Times", "Growth rate", "a")
  } else {
    colnames(coefs) <- c("Times", "Growth rate", "a", "b", "corr")
  }

  coef_dataframe <- data.frame(coefs)
  coef_dataframe$group <- rep(obserror_group_names, each = length(time_snaps))
  coef_dataframe$group <- factor(coef_dataframe$group,
                     levels = obserror_group_names)

  fig1 <- plot_ly(coef_dataframe, x = ~Times,
                  y = ~Growth.rate,
                  split = ~group,
                  legendgroup= ~group)
  fig1 <- fig1 %>% add_lines(name = ~group,
                             color = ~group,
                             colors = "PiYG",
                             showlegend = F)
  fig1 <- fig1 %>% add_lines(y = growth_rate,
                             line = list(color = "red", dash = "dash"),
                             showlegend = F)
  fig1 <- fig1 %>%
    layout(annotations = list(x = 0.2 , y = 1.05, text = "Growth rate", showarrow = F,
                              xref='paper', yref='paper'))

  if (nspe == 1) {
    fig2 <- plot_ly(coef_dataframe, x = ~Times, y = ~a, split = ~group,
                    legendgroup= ~group)
    fig2 <- fig2 %>% add_lines(name = ~group,
                               color = ~group,
                               colors = "PiYG")
    fig2 <- fig2 %>% add_lines(y = true.paras[3],
                               line = list(color = "red", dash = "dash"),
                               showlegend = F)
    fig2 <- fig2 %>%
      layout(annotations = list(x = 0.2 , y = 1.05, text = "a", showarrow = F,
                                xref='paper', yref='paper'))
    fig_inf1 <- subplot(fig1, fig2)

  } else {
    fig2 <- plot_ly(coef_dataframe, x = ~Times, y = ~a, split = ~group,
                    legendgroup= ~group)
    fig2 <- fig2 %>% add_lines(name = ~group,
                               color = ~group,
                               colors = "PiYG",
                               showlegend = F)
    fig2 <- fig2 %>% add_lines(y = true.paras[3],
                               line = list(color = "red", dash = "dash"),
                               showlegend = F)
    fig2 <- fig2 %>%
      layout(annotations = list(x = 0.2 , y = 1.05, text = "a", showarrow = F,
                                xref='paper', yref='paper'))
    fig3 <- plot_ly(coef_dataframe, x = ~Times, y = ~b, split = ~group,
                    legendgroup= ~group)
    fig3 <- fig3 %>% add_lines(name = ~group,
                               color = ~group,
                               colors = "PiYG")
    fig3 <- fig3 %>% add_lines(y = true.paras[4],
                               line = list(color = "red", dash = "dash"),
                               showlegend = F)
    fig3 <- fig3 %>%
      layout(annotations = list(x = 0.2 , y = 1.05, text = "b", showarrow = F,
                                xref='paper', yref='paper'))
    fig_inf1 <- subplot(fig1, fig2, fig3)

  }

  fig_inf1 <- fig_inf1 %>% layout(
    xaxis = list(title = "Years"),
    yaxis = list (title = "Estimates"),
    legend = list(x = 0.4, y = -0.2, orientation = 'h'))


  # Plot 2, fixing growth rate

  # estimates for each time snap
  coefs_fix <- NULL

  if ( nspe == 1) {
    for (sim_result in sim_result_list) {
      # remove the data with 0 plants
      row_sub = which(apply(sim_result, 1, function(row) all(row > 0 )))
      ##Subset as usual
      sim_result <- sim_result[row_sub,,, drop = FALSE]
      for (time_snap in time_snaps) {
        data_x <- sim_result[, 1, time_snap[1]]
        data_y <- sim_result[, 1, time_snap[2]]

        lmodel <- lm(log(data_y / data_x) - 1 - log(growth_rate) ~ 0 + data_x)
        coefs_fix <- rbind(coefs_fix, c(coef(lmodel)[1]))
      }
    }
    coefs_fix[,1] <- -coefs_fix[, 1]
  } else {
    for (sim_result in sim_result_list) {
      # remove the data with 0 plants
      row_sub = which(apply(sim_result, 1, function(row) all(row > 0 )))
      ##Subset as usual
      sim_result <- sim_result[row_sub,,, drop = FALSE]
      # estimates for each time snap
      for (time_snap in time_snaps) {
        data_spe1_x <- sim_result[, 1, time_snap[1]]
        data_spe1_y <- sim_result[, 1, time_snap[2]]

        data_spe2_x <- sim_result[, 2, time_snap[1]]
        data_spe2_y <- sim_result[, 2, time_snap[2]]

        lmodel <- lm(log(data_spe1_y / data_spe1_x) - log(growth_rate) - 1 ~ 0 + data_spe1_x + data_spe2_x)
        coefs_fix <- rbind(coefs_fix, c(coef(lmodel)[1], coef(lmodel)[2]))
      }
    }
    coefs_fix[,1] <- -coefs_fix[, 1]
    coefs_fix[,2] <- -coefs_fix[, 2]
  }

  # correct the estimated growth rate
  coefs_fix <- cbind(c(1:(tend - 1)), coefs_fix)

  if (nspe == 1) {
    colnames(coefs_fix) <- c("Times",  "a")
  } else {
    colnames(coefs_fix) <- c("Times", "a", "b")
  }


  coef_fix_dataframe <- data.frame(coefs_fix)
  coef_fix_dataframe$group <- rep(obserror_group_names, each = length(time_snaps))
  coef_fix_dataframe$group <- factor(coef_fix_dataframe$group,
                                 levels = obserror_group_names)


  if (nspe == 1) {
    fig1_fix <- plot_ly(coef_fix_dataframe, x = ~Times, y = ~a, split = ~group)
    fig1_fix <- fig1_fix %>% add_lines(name = ~group,
                                       color = ~group,
                                       colors = "PiYG")
    fig1_fix <- fig1_fix %>% add_lines(y = true.paras[3],
                                       line = list(color = "red",
                                                   dash = "dash"),
                                       showlegend = F)
    fig1_fix <- fig1_fix %>%
      layout(annotations = list(x = 0.2 , y = 1.05, text = "a", showarrow = F,
                                xref='paper', yref='paper'))
    fig_inf2 <- fig1_fix
  } else {
    fig1_fix <- plot_ly(coef_fix_dataframe, x = ~Times, y = ~a, split = ~group,
                        legendgroup= ~group)
    fig1_fix <- fig1_fix %>% add_lines(name = ~group,
                                       color = ~group,
                                       colors = "PiYG",
                                       showlegend = F)
    fig1_fix <- fig1_fix %>% add_lines(y = true.paras[3],
                                       line = list(color = "red",
                                                   dash = "dash"),
                                       showlegend = F)
    fig1_fix <- fig1_fix %>%
      layout(annotations = list(x = 0.2 , y = 1.05, text = "a", showarrow = F,
                                xref='paper', yref='paper'))

    fig2_fix <- plot_ly(coef_fix_dataframe, x = ~Times, y = ~b, split = ~group,
                        legendgroup= ~group)
    fig2_fix <- fig2_fix %>% add_lines(name = ~group,
                                       color = ~group,
                                       colors = "PiYG",
                                       showlegend = F)
    fig2_fix <- fig2_fix %>% add_lines(y = true.paras[4], line = list(color = "red", dash = "dash"), showlegend = F)
    fig2_fix <- fig2_fix %>%
      layout(annotations = list(x = 0.2 , y = 1.05, text = "b", showarrow = F,
                                xref='paper', yref='paper'))

    fig_corr <- plot_ly(coef_dataframe, x = ~Times, y = ~corr, split = ~group,
                        legendgroup= ~group)
    fig_corr <- fig_corr %>% add_lines(name = ~group,
                                       color = ~group,
                                       colors = "PiYG")
    fig_corr <- fig_corr %>% add_lines(y = 0, line = list(color = "red", dash = "dot"), showlegend = F)
    fig_corr <- fig_corr %>%
      layout(annotations = list(x = 0.2 , y = 1.05, text = "Corr", showarrow = F,
                                xref='paper', yref='paper'))
    fig_inf2 <- subplot(fig1_fix, fig2_fix, fig_corr)
  }

  fig_inf2 <- fig_inf2 %>% layout(
    xaxis = list(title = "Years"),
    yaxis = list (title = "Estimates"),
    legend = list(x = 0.4, y = -0.2, orientation = 'h'))



  return(list(fig_inf1, fig_inf2))

}
