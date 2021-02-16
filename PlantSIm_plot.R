PlantSim_plot <- function(sim_result) {
  nplot <- dim(sim_result)[1]
  nspe <- dim(sim_result)[2]
  tend <- dim(sim_result)[3]
  # process the data in a dataframe format
  # remove the data with 0 plants
  row_sub = which(apply(sim_result, 1, function(row) all(row > 0 )))
  if (length(row_sub) < 3) {
    text = paste("\n  No plots show positive number of plants. \n",
                 "\n  Please try other parameters.")
    fig_warning <- ggplot() +
      annotate("text", x = 4, y = 25, size=8, label = text) +
      theme_void()
    return(list(fig_warning, fig_warning))
    } else {
    plots <- sort(sample(row_sub, size = 3))
    plants_data <- NULL
    colname_data <- NULL
    for (spe_ind in c(1:nspe)) {
      for (ii in c(1:length(plots))) {
        i = plots[ii]
        plants_data <- cbind(plants_data, sim_result[i, spe_ind, ])
        colname_data <- c(colname_data, paste0("spe", spe_ind, "plot", ii))
      }
    }

    plants_data <- cbind(c(1:tend), plants_data)
    colnames(plants_data) <- c('years',
                               colname_data
    )
    plants_dataframe <-
      data.frame(plants_data)

    # local plots
    fig <- plot_ly(plants_dataframe, x = ~years, y = ~spe1plot1, name = paste("Spe 1 in plot", plots[1]), type = 'scatter', mode = 'lines',
                   line = list(color = 'rgb(205, 12, 24)', width = 4))
    fig <- fig %>% add_trace(y = ~spe1plot2, name = paste("Spe 1 in plot", plots[2]), line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'))
    fig <- fig %>% add_trace(y = ~spe1plot3, name = paste("Spe 1 in plot", plots[3]), line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dot'))
    fig <- fig %>% layout(
      xaxis = list(title = "Year"),
      yaxis = list (title = "Plants"),
      legend = list(x = 0.1, y = 0.9))

    if (nspe == 2) {
      fig <- fig %>% add_trace(y = ~spe2plot1, name = paste("Spe 2 in plot", plots[1]), line = list(color = 'rgb(22, 96, 167)', width = 4))
      fig <- fig %>% add_trace(y = ~spe2plot2, name = paste("Spe 2 in plot", plots[2]), line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dash'))
      fig <- fig %>% add_trace(y = ~spe2plot3, name = paste("Spe 2 in plot", plots[3]), line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dot'))

    }

    # global plots
    overall_data <- cbind(c(1:tend), t(apply(sim_result, c(2, 3), sum)))
    if (nspe == 1) {
      colnames(overall_data) <- c("years", "spe1")
    } else {
      colnames(overall_data) <- c("years", "spe1", "spe2")
    }

    overall_df <- data.frame(overall_data)

    fig_globa <- plot_ly(overall_df, x = ~years, y = ~spe1, name = "Spe 1", type = 'scatter', mode = 'lines',
                         line = list(color = 'rgb(205, 12, 24)', width = 4))
    fig_globa <- fig_globa %>% layout(title = "",
                                      xaxis = list(title = "Years"),
                                      yaxis = list(title = "Plants"),
                                      legend = list(x = 0.1, y = 0.9))

    if (nspe == 2) {
      fig_globa <- fig_globa %>% add_trace(y = ~spe2, name = "Spe 2", line = list(color = 'rgb(22, 96, 167)', width = 4))
    }



    return(list(fig, fig_globa))
  }

}
