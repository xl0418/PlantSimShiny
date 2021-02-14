PlantSim_sim <- function(paras) {

  # Specify parameters
  nplot <- paras[1] # the number of the plots
  nspe <- paras[2] # the number of the species
  tend <- paras[3] # the total time steps of simulation
  ini_abundance <- matrix(runif(nplot * nspe, 1, 5), nplot, nspe)
  growth_rate <- paras[4]
  interaction_matrix <- matrix(0, nspe, nspe)
  if (nspe == 1) {
    interaction_matrix[1, 1] <- paras[8]
  } else {
    interaction_matrix[1, 1] <- paras[8]
    interaction_matrix[1, 2] <- paras[9]
    interaction_matrix[2, 1] <- paras[9]
    interaction_matrix[2, 2] <- paras[8]
  }


  st_portion <- paras[5]
  surv_rate <- paras[6]

  sim_result <- plantsim(
    nplot = nplot,
    nspe = nspe,
    t = tend,
    ini_abundance = ini_abundance,
    growth_rate = growth_rate,
    interaction_matrix = interaction_matrix,
    st_portion = st_portion,
    surv_rate = surv_rate
  )

  sim_result <- sim_result + rnorm(length(sim_result), 0, paras[7])
  return(sim_result)
}