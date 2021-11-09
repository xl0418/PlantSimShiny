PlantSim_sim <- function(paras) {

  # Specify parameters
  nplot <- paras[1] # the number of the plots
  nspe <- paras[2] # the number of the species
  tend <- paras[3] # the total time steps of simulation
  ini_abundance <- matrix(runif(nplot * nspe, 10, 20), nplot, nspe)
  growth_rate <- paras[4]

  st_portion <- paras[5]
  surv_rate <- paras[6]
  obs_err_rate <- paras[7]
  kill_rate <- paras[8]

  interaction_matrix <- matrix(0, nspe, nspe)
  if (nspe == 1) {
    interaction_matrix[1, 1] <- paras[9]
  } else {
    interaction_matrix[1, 1] <- paras[9]
    interaction_matrix[1, 2] <- paras[10]
    interaction_matrix[2, 1] <- paras[10]
    interaction_matrix[2, 2] <- paras[9]
  }


  sim_result <- plantsim(
    nplot = nplot,
    nspe = nspe,
    t = tend,
    ini_abundance = ini_abundance,
    growth_rate = growth_rate,
    interaction_matrix = interaction_matrix,
    st_portion = st_portion
  )

  if (obs_err_rate != 0) {
    sim_result$all <-
      round(rtnorm(1,
             mu = sim_result$all,
             sd = pmax(sim_result$all * obs_err_rate, 1),
             lb = sim_result$all * (1 - obs_err_rate),
             ub = sim_result$all * (1 + obs_err_rate) ))
    dim(sim_result$all) <- c(nplot, nspe, tend)
  }


  return(sim_result)
}
