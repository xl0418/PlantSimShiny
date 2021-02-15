PlantSim_infplot <- function(sim_result, true.paras) {
  nplot <- dim(sim_result)[1]
  nspe <- dim(sim_result)[2]
  tend <- dim(sim_result)[3]


  # Inference plots
  time_snaps <- list()
  for (i in c(1:(tend - 1))) {
  time_snaps[[i]] <- c(i, i + 1)
  }


  # remove the data with 0 plants
  row_sub = which(apply(sim_result, 1, function(row) all(row > 0 )))
  ##Subset as usual
  sim_result <- sim_result[row_sub,,, drop = FALSE]

  # estimates for each time snap
  coefs <- NULL
  corr <- NULL

  if ( nspe == 1) {
    for (time_snap in time_snaps) {
      data_x <- sim_result[, 1, time_snap[1]]
      data_y <- sim_result[, 1, time_snap[2]]

      lmodel <- lm(log(data_y / data_x) ~ data_x)
      coefs <- rbind(coefs, c(coef(lmodel)[1], coef(lmodel)[2]))
    }
    coefs[,2] <- -coefs[, 2]
  } else {
    # estimates for each time snap
    for (time_snap in time_snaps) {
      data_spe1_x <- sim_result[, 1, time_snap[1]] + 1
      data_spe1_y <- sim_result[, 1, time_snap[2]] + 1

      data_spe2_x <- sim_result[, 2, time_snap[1]] + 1
      data_spe2_y <- sim_result[, 2, time_snap[2]] + 1

      lmodel <- lm(log(data_spe1_y / data_spe1_x) ~ data_spe1_x + data_spe2_x)
      coefs <- rbind(coefs, c(coef(lmodel)[1], coef(lmodel)[2], coef(lmodel)[3]))
      corr <- c(corr, cor(data_spe1_x, data_spe2_x))
    }

    coefs[,2] <- -coefs[, 2]
    coefs[,3] <- -coefs[, 3]
  }

  # correct the estimated growth rate
  coefs[, 1] <- exp(coefs[, 1] - 1)/surv_rate
  coefs <- cbind(c(1:(tend - 1)), coefs)

  if (nspe == 1) {
    colnames(coefs) <- c("Times", "Growth rate", "a")
    # true parameters
    true_para <- data.frame(values = true.paras[1:2],
                            names = c("growth rate", "a"))
  } else {
    colnames(coefs) <- c("Times", "Growth rate", "a", "b")
    # true parameters
    true_para <- data.frame(values = true.paras,
                            names = c("growth rate", "a", "b"))
  }


  coef_dataframe <- data.frame(coefs)

  fig1 <- plot_ly(coef_dataframe, x = ~Times, y = ~Growth.rate)
  fig1 <- fig1 %>% add_lines(name = ~"Est. growth rate")
  fig1 <- fig1 %>% add_lines(y = true.paras[1], line = list(color = "red", dash = "dash"), name = "True growth rate")
  fig2 <- plot_ly(coef_dataframe, x = ~Times, y = ~a)
  fig2 <- fig2 %>% add_lines(name = ~"Est. a")
  fig2 <- fig2 %>% add_lines(y = true.paras[2], line = list(color = "red", dash = "dash"), name = "True a")

  if (nspe == 1) {
    fig_inf1 <- subplot(fig1, fig2)
  } else {
    fig3 <- plot_ly(coef_dataframe, x = ~Times, y = ~b)
    fig3 <- fig3 %>% add_lines(name = ~"Est. b")
    fig3 <- fig3 %>% add_lines(y = true.paras[3], line = list(color = "red", dash = "dash"), name = "True b")

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
    for (time_snap in time_snaps) {
      data_x <- sim_result[, 1, time_snap[1]]
      data_y <- sim_result[, 1, time_snap[2]]

      lmodel <- lm(log(data_y / data_x) - 1 - log(growth_rate) ~ 0 + data_x)
      coefs_fix <- rbind(coefs_fix, c(coef(lmodel)[1]))
    }
    coefs_fix[,1] <- -coefs_fix[, 1]
  } else {
    # estimates for each time snap
    for (time_snap in time_snaps) {
      data_spe1_x <- sim_result[, 1, time_snap[1]] + 1
      data_spe1_y <- sim_result[, 1, time_snap[2]] + 1

      data_spe2_x <- sim_result[, 2, time_snap[1]] + 1
      data_spe2_y <- sim_result[, 2, time_snap[2]] + 1

      lmodel <- lm(log(data_spe1_y / data_spe1_x) - log(growth_rate) - 1 ~ 0 + data_spe1_x + data_spe2_x)
      coefs_fix <- rbind(coefs_fix, c(coef(lmodel)[1], coef(lmodel)[2]))
    }

    coefs_fix[,1] <- -coefs_fix[, 1]
    coefs_fix[,2] <- -coefs_fix[, 2]
  }

  # correct the estimated growth rate
  coefs_fix <- cbind(c(1:(tend - 1)), coefs_fix)

  if (nspe == 1) {
    colnames(coefs_fix) <- c("Times",  "a")
    # true parameters
    true_para_fix <- data.frame(values = true.paras[2],
                            names = c("a"))
  } else {
    colnames(coefs_fix) <- c("Times", "a", "b")
    # true parameters
    true_para_fix <- data.frame(values = true.paras[2:3],
                            names = c( "a", "b"))
  }


  coef_fix_dataframe <- data.frame(coefs_fix)

  fig1_fix <- plot_ly(coef_fix_dataframe, x = ~Times, y = ~a)
  fig1_fix <- fig1_fix %>% add_lines(name = ~"Est. a", line = list(color = "green"))
  fig1_fix <- fig1_fix %>% add_lines(y = true.paras[2], line = list(color = "red", dash = "dash"), name = "True a")

  if (nspe == 1) {
    fig_inf2 <- fig1_fix
  } else {
    fig2_fix <- plot_ly(coef_fix_dataframe, x = ~Times, y = ~b)
    fig2_fix <- fig2_fix %>% add_lines(name = ~"Est. b", line = list(color = "purple"))
    fig2_fix <- fig2_fix %>% add_lines(y = true.paras[3], line = list(color = "red", dash = "dash"), name = "True b")

    fig_inf2 <- subplot(fig1_fix, fig2_fix)

  }

  fig_inf2 <- fig_inf2 %>% layout(
    xaxis = list(title = "Years"),
    yaxis = list (title = "Estimates"),
    legend = list(x = 0.4, y = -0.2, orientation = 'h'))



  return(list(fig_inf1, fig_inf2))
}