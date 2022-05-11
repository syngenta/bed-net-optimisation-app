
make_outputs <- function(output, rv, i){
  
  # ** baseline ####
  
  # input curves
  
  output[[paste0('base_decay_chemical', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_decay_curves(rv[[paste0('sim_results', i)]],
                      optimum_list=list(c(0),matrix(c(3,1,1),1,3),c("Baseline")),
                      curve="chemical")
  })
  
  output[[paste0('base_decay_mortality', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_decay_curves(rv[[paste0('sim_results', i)]],
                      optimum_list=list(c(0),matrix(c(3,1,1),1,3),c("Baseline")),
                      curve="mortality")
  })
  
  output[[paste0('base_decay_physical', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_decay_curves(rv[[paste0('sim_results', i)]],
                      optimum_list=list(c(0),matrix(c(3,1,1),1,3),c("Baseline")),
                      curve="physical")
  })
  
  # output dynamics
  
  output[[paste0('base_optimal_dynamics_popsize', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimal_dynamics(rv[[paste0('sim_results', i)]],curve="Population Size")
  })
  
  output[[paste0('base_optimal_dynamics_allele_freq', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimal_dynamics(rv[[paste0('sim_results', i)]],curve="Allele Frequency")
  })
  
  output[[paste0('base_optimal_dynamics_both', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimal_dynamics(rv[[paste0('sim_results', i)]],curve="Resistance Phenotype")
  })
  
  # vector control
  
  output[[paste0('base_optimality_landscape_vc1', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],fixed_dim=3,z_var="vc")
  })
  
  output[[paste0('base_optimality_landscape_vc2', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],fixed_dim=2,z_var="vc")
  })
  
  output[[paste0('base_optimality_landscape_vc3', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],fixed_dim=1,z_var="vc")
  })
  
  # coverage
  
  output[[paste0('base_optimality_landscape_coverage1', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],fixed_dim=3,z_var="cv")
  })
  
  output[[paste0('base_optimality_landscape_coverage2', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],fixed_dim=2,z_var="cv")
  })
  
  output[[paste0('base_optimality_landscape_coverage3', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],fixed_dim=1,z_var="cv")
  })
  
  # bednet cost
  
  output[[paste0('base_optimality_landscape_bednet_cost1', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],fixed_dim=3,z_var="bc")
  })
  
  output[[paste0('base_optimality_landscape_bednet_cost2', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],fixed_dim=2,z_var="bc")
  })
  
  output[[paste0('base_optimality_landscape_bednet_cost3', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],fixed_dim=1,z_var="bc")
  })
  
  # ** max vector control ####
  
  # input curves
  
  output[[paste0('mvc_decay_chemical', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_decay_curves(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],echo=F),
                      curve="chemical")
  })
  
  output[[paste0('mvc_decay_mortality', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_decay_curves(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],echo=F),
                      curve="mortality")
  })
  
  output[[paste0('mvc_decay_physical', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_decay_curves(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],echo=F),
                      curve="physical")
  })
  
  # output dynamics
  
  output[[paste0('mvc_optimal_dynamics_popsize', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimal_dynamics(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],echo=F),
                          curve="Population Size")
  })
  
  output[[paste0('mvc_optimal_dynamics_allele_freq', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimal_dynamics(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],echo=F),
                          curve="Allele Frequency")
  })
  
  output[[paste0('mvc_optimal_dynamics_both', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimal_dynamics(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],echo=F),
                          curve="Resistance Phenotype")
  })
  
  # vector control
  
  output[[paste0('mvc_optimality_landscape_vc1', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],echo=F),
                              fixed_dim=3,z_var="vc")
  })
  
  output[[paste0('mvc_optimality_landscape_vc2', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],echo=F),
                              fixed_dim=2,z_var="vc")
  })
  
  output[[paste0('mvc_optimality_landscape_vc3', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],echo=F),
                              fixed_dim=1,z_var="vc")
  })
  
  # coverage
  
  output[[paste0('mvc_optimality_landscape_coverage1', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],echo=F),
                              fixed_dim=3,z_var="cv")
  })
  
  output[[paste0('mvc_optimality_landscape_coverage2', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],echo=F),
                              fixed_dim=2,z_var="cv")
  })
  
  output[[paste0('mvc_optimality_landscape_coverage3', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],echo=F),
                              fixed_dim=1,z_var="cv")
  })
  
  # bednet cost
  
  output[[paste0('mvc_optimality_landscape_bednet_cost1', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],echo=F),
                              fixed_dim=3,z_var="bc")
  })
  
  output[[paste0('mvc_optimality_landscape_bednet_cost2', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],echo=F),
                              fixed_dim=2,z_var="bc")
  })
  
  output[[paste0('mvc_optimality_landscape_bednet_cost3', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],echo=F),
                              fixed_dim=1,z_var="bc")
  })
  
  # ** cheapest bednets ####
  
  # input curves
  
  output[[paste0('cheapest_decay_chemical', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_decay_curves(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],opt_name="WHO",
                                                                                   echo=F),
                      curve="chemical")
  })
  
  output[[paste0('cheapest_decay_mortality', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_decay_curves(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],opt_name="WHO",
                                                                                   echo=F),
                      curve="mortality")
  })
  
  output[[paste0('cheapest_decay_physical', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_decay_curves(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],opt_name="WHO",
                                                                                   echo=F),
                      curve="physical")
  })
  
  # output dynamics
  
  output[[paste0('cheapest_optimal_dynamics_popsize', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimal_dynamics(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],opt_name="WHO",
                                                                                       echo=F),
                          curve="Population Size")
  })
  
  output[[paste0('cheapest_optimal_dynamics_allele_freq', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimal_dynamics(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],opt_name="WHO",
                                                                                       echo=F),
                          curve="Allele Frequency")
  })
  
  output[[paste0('cheapest_optimal_dynamics_both', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimal_dynamics(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],opt_name="WHO",
                                                                                       echo=F),
                          curve="Resistance Phenotype")
  })
  
  # vector control
  
  output[[paste0('cheapest_optimality_landscape_vc1', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],
                                                                                           opt_name="WHO",
                                                                                           echo=F),
                              fixed_dim=3,z_var="vc")
  })
  
  output[[paste0('cheapest_optimality_landscape_vc2', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],
                                                                                           opt_name="WHO",
                                                                                           echo=F),
                              fixed_dim=2,z_var="vc")
  })
  
  output[[paste0('cheapest_optimality_landscape_vc3', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],
                                                                                           opt_name="WHO",
                                                                                           echo=F),
                              fixed_dim=1,z_var="vc")
  })
  
  # coverage
  
  output[[paste0('cheapest_optimality_landscape_coverage1', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],
                                                                                           opt_name="WHO",
                                                                                           echo=F),
                              fixed_dim=3,z_var="cv")
  })
  
  output[[paste0('cheapest_optimality_landscape_coverage2', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],
                                                                                           opt_name="WHO",
                                                                                           echo=F),
                              fixed_dim=2,z_var="cv")
  })
  
  output[[paste0('cheapest_optimality_landscape_coverage3', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],
                                                                                           opt_name="WHO",
                                                                                           echo=F),
                              fixed_dim=1,z_var="cv")
  })
  
  # bednet cost
  
  output[[paste0('cheapest_optimality_landscape_bednet_cost1', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],
                                                                                           opt_name="WHO",
                                                                                           echo=F),
                              fixed_dim=3,z_var="bc")
  })
  
  output[[paste0('cheapest_optimality_landscape_bednet_cost2', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],
                                                                                           opt_name="WHO",
                                                                                           echo=F),
                              fixed_dim=2,z_var="bc")
  })
  
  output[[paste0('cheapest_optimality_landscape_bednet_cost3', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],
                                                                                           opt_name="WHO",
                                                                                           echo=F),
                              fixed_dim=1,z_var="bc")
  })
  
  # ** who ####
  
  # input curves
  
  output[[paste0('who_decay_chemical', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_decay_curves(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],opt_name="PQL",
                                                                                   echo=F),
                      curve="chemical")
  })
  
  output[[paste0('who_decay_mortality', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_decay_curves(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],opt_name="PQL",
                                                                                   echo=F),
                      curve="mortality")
  })
  
  output[[paste0('who_decay_physical', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_decay_curves(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],opt_name="PQL",
                                                                                   echo=F),
                      curve="physical")
  })
  
  # output dynamics
  
  output[[paste0('who_optimal_dynamics_popsize', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimal_dynamics(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],opt_name="PQL",
                                                                                       echo=F),
                          curve="Population Size")
  })
  
  output[[paste0('who_optimal_dynamics_allele_freq', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimal_dynamics(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],opt_name="PQL",
                                                                                       echo=F),
                          curve="Allele Frequency")
  })
  
  output[[paste0('who_optimal_dynamics_both', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimal_dynamics(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],opt_name="PQL",
                                                                                       echo=F),
                          curve="Resistance Phenotype")
  })
  
  # vector control
  
  output[[paste0('who_optimality_landscape_vc1', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],
                                                                                           opt_name="PQL",
                                                                                           echo=F),
                              fixed_dim=3,z_var="vc")
  })
  
  output[[paste0('who_optimality_landscape_vc2', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],
                                                                                           opt_name="PQL",
                                                                                           echo=F),
                              fixed_dim=2,z_var="vc")
  })
  
  output[[paste0('who_optimality_landscape_vc3', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],
                                                                                           opt_name="PQL",
                                                                                           echo=F),
                              fixed_dim=1,z_var="vc")
  })
  
  # coverage
  
  output[[paste0('who_optimality_landscape_coverage1', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],
                                                                                           opt_name="PQL",
                                                                                           echo=F),
                              fixed_dim=3,z_var="cv")
  })
  
  output[[paste0('who_optimality_landscape_coverage2', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],
                                                                                           opt_name="PQL",
                                                                                           echo=F),
                              fixed_dim=2,z_var="cv")
  })
  
  output[[paste0('who_optimality_landscape_coverage3', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],
                                                                                           opt_name="PQL",
                                                                                           echo=F),
                              fixed_dim=1,z_var="cv")
  })
  
  # bednet cost
  
  output[[paste0('who_optimality_landscape_bednet_cost1', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],
                                                                                           opt_name="PQL",
                                                                                           echo=F),
                              fixed_dim=3,z_var="bc")
  })
  
  output[[paste0('who_optimality_landscape_bednet_cost2', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],
                                                                                           opt_name="PQL",
                                                                                           echo=F),
                              fixed_dim=2,z_var="bc")
  })
  
  output[[paste0('who_optimality_landscape_bednet_cost3', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],
                                                                                           opt_name="PQL",
                                                                                           echo=F),
                              fixed_dim=1,z_var="bc")
  })
  
  # ** universal ####
  
  # input curves
  
  output[[paste0('universal_decay_chemical', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_decay_curves(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],opt_name="UC",
                                                                                   echo=F),
                      curve="chemical")
  })
  
  output[[paste0('universal_decay_mortality', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_decay_curves(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],opt_name="UC",
                                                                                   echo=F),
                      curve="mortality")
  })
  
  output[[paste0('universal_decay_physical', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_decay_curves(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],opt_name="UC",
                                                                                   echo=F),
                      curve="physical")
  })
  
  # output dynamics
  
  output[[paste0('universal_optimal_dynamics_popsize', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimal_dynamics(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],opt_name="UC",
                                                                                       echo=F),
                          curve="Population Size")
  })
  
  output[[paste0('universal_optimal_dynamics_allele_freq', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimal_dynamics(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],opt_name="UC",
                                                                                       echo=F),
                          curve="Allele Frequency")
  })
  
  output[[paste0('universal_optimal_dynamics_both', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimal_dynamics(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],opt_name="UC",
                                                                                       echo=F),
                          curve="Resistance Phenotype")
  })
  
  # vector control
  
  output[[paste0('universal_optimality_landscape_vc1', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],
                                                                                           opt_name="UC",
                                                                                           echo=F),
                              fixed_dim=3,z_var="vc")
  })
  
  output[[paste0('universal_optimality_landscape_vc2', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],
                                                                                           opt_name="UC",
                                                                                           echo=F),
                              fixed_dim=2,z_var="vc")
  })
  
  output[[paste0('universal_optimality_landscape_vc3', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],
                                                                                           opt_name="UC",
                                                                                           echo=F),
                              fixed_dim=1,z_var="vc")
  })
  
  # coverage
  
  output[[paste0('universal_optimality_landscape_coverage1', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],
                                                                                           opt_name="UC",
                                                                                           echo=F),
                              fixed_dim=3,z_var="cv")
  })
  
  output[[paste0('universal_optimality_landscape_coverage2', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],
                                                                                           opt_name="UC",
                                                                                           echo=F),
                              fixed_dim=2,z_var="cv")
  })
  
  output[[paste0('universal_optimality_landscape_coverage3', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],
                                                                                           opt_name="UC",
                                                                                           echo=F),
                              fixed_dim=1,z_var="cv")
  })
  
  # bednet cost
  
  output[[paste0('universal_optimality_landscape_bednet_cost1', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],
                                                                                           opt_name="UC",
                                                                                           echo=F),
                              fixed_dim=3,z_var="bc")
  })
  
  output[[paste0('universal_optimality_landscape_bednet_cost2', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],
                                                                                           opt_name="UC",
                                                                                           echo=F),
                              fixed_dim=2,z_var="bc")
  })
  
  output[[paste0('universal_optimality_landscape_bednet_cost3', i)]] <- renderPlot({
    req(rv[[paste0('sim_results', i)]])
    plot_optimality_landscape(rv[[paste0('sim_results', i)]],optimum_list=calculate_optimum(rv[[paste0('sim_results', i)]],
                                                                                           opt_name="UC",
                                                                                           echo=F),
                              fixed_dim=1,z_var="bc")
    
  })
  
}


make_results_row <- function(prefix, ids, simname, x_tab, y_tab){
  tagList(lapply(1:N_SIM_ROWS, function(i){
    div(
      a(
        `data-toggle`="collapse", href=paste0("#collapse",1000,i,x_tab,y_tab), style="text-decoration: none", 
        h2(textOutput(paste0(prefix, simname, i)))
      ),
      div(
        id=paste0("collapse",1000,i,x_tab,y_tab), class="collapse in",
        fluidRow(
          column(width=4, plotOutput(paste0(prefix, '_', ids[1], i))),
          column(width=4, plotOutput(paste0(prefix, '_', ids[2], i))),
          column(width=4, plotOutput(paste0(prefix, '_', ids[3], i)))
        )
      )
    )
  }))
}

make_results_tab <- function(title, prefix, y_tab){
  tabPanel(
    title,
    tabsetPanel(
      tabPanel(
        'Input Curves',
        tags$details(OUTPUT_INFO[['input_curves']]),
        make_results_row(
          prefix,
          ids=c('decay_chemical', 'decay_mortality', 'decay_physical'),
          simname='input_curves',
          1,
          y_tab
        )
      ),
      tabPanel(
        'Output Dynamics',
        tags$details(OUTPUT_INFO[['output_dynamics']]),
        make_results_row(
          prefix,
          ids=c('optimal_dynamics_popsize', 'optimal_dynamics_allele_freq', 'optimal_dynamics_both'),
          simname='output_dynamics',
          2,
          y_tab
        )
      ),
      tabPanel(
        'Vector Control',
        tags$details(OUTPUT_INFO[['vector_control']]),
        make_results_row(
          prefix,
          ids=c('optimality_landscape_vc1', 'optimality_landscape_vc2', 'optimality_landscape_vc3'),
          simname='vector_control',
          3,
          y_tab
        )
      ),
      tabPanel(
        'Coverage',
        tags$details(OUTPUT_INFO[['coverage']]),
        make_results_row(
          prefix,
          ids=c('optimality_landscape_coverage1', 'optimality_landscape_coverage2', 'optimality_landscape_coverage3'),
          simname='coverage',
          4,
          y_tab
        )
      ),
      tabPanel(
        'Bed-Net Cost',
        tags$details(OUTPUT_INFO[['bednet_cost']]),
        make_results_row(
          prefix,
          ids=c('optimality_landscape_bednet_cost1', 'optimality_landscape_bednet_cost2', 'optimality_landscape_bednet_cost3'),
          simname='bednet_cost',
          5,
          y_tab
        )
      )
    )
  )
}

plot_test <- function(input, i, type){
  
  if (i == 1){
    el <- input$el
    mrx <- input$mrx6
    iX <- input$iX6
    iX_abb <- input$iX6_abb
    cd <- input$cd6
    md <- input$md6
    mh <- input$mh6
    xu <- input$xu6
  } else {
    el <- input$el
    mrx <- input$mrx7
    iX <- input$iX7
    iX_abb <- input$iX7_abb
    cd <- input$cd7
    md <- input$md7
    mh <- input$mh7
    xu <- input$xu7
  }
  
  plot_test_newAI((1-el)^mrx,iX,iX_abb,cd,md,mh,xu,curve=type)
}

show_test_modal <- function(){
  showModal(modalDialog(
    fluidRow(
      column(width=6, plotOutput('test_chemical')),
      column(width=6, plotOutput('test_mortality'))
    )
  ))
}

run_sim <- function(input, token=NULL){
  
  # basic  
  os <- input$os # resistance-management strategy: Mixture, Rotation or Mosaic
  xb <- input$xb # public health budget per household for purchasing bednets 
  iA <- input$iA # name of the first insecticide: Pyrethroid, PBO, 
  iB <- input$iB # name of the second insecticide: Pyrethroid, PBO, 
  
  # advanced
  og <- input$og # mosquito generations per year of simulation 
  oy <- input$oy # number of years of simulation; and the maximum deployment lifespan of 
  ol <- input$ol # the maximum chemical loading of an AI onto a bednet in relative units
  oi_b <- input$oi_b # interval in years for the vector of deployment lifespans to run 
  oi_c <- input$oi_c # interval in relative units for the vector of chemical loading to 
  ou <- input$ou #  # resistance mutation rate (which determines the time it takes for 
  er <- input$er # mosquito population growth rate (for a logistic model)
  ek <- as.numeric(input$ek) # mosquito population carrying capacity (for a logistic model)
  el <- input$el # level of resistance confered by a resistance mutation, as a 
  xn <- input$xn # base cost in USD for an untreated bednet 
  mrx1 <- input$mrx1 # pre-existing level of resistance to Pyrethroids in terms of 
  xs1 <- input$xs1 # proportional subsidy of AI cost for the first insecticide
  xs2 <- input$xs2 # proportional subsidy of AI cost for the second insecticide
  pm <- input$pm # proportional usage of provisioned bednets
  p0 <- input$p0 # proportional bednet physical survival on arrival (year 0)
  p3 <- input$p3 # proportional bednet physical survival after 3 years 
  
  mrx6 <- input$mrx6 # pre-existing level of resistance to insecticide
  iX6 <- input$iX6 # name of insecticide
  iX6_abb <- input$iX6_abb # 3-letter abbreviated name of insecticide
  cd6 <- input$cd6 # rate for chemical decay of insecticide on bednets 
  md6 <- input$md6 # rate for mortality decay from insecticide on bednets
  mh6 <- input$mh6 # half-life for mortality decay from insecticide on bednets
  xu6 <- input$xu6 # cost in USD per relative unit of AI for the insecticide 
  mrx7 <- input$mrx7 # pre-existing level of resistance to insecticide
  iX7 <- input$iX7 # name of insecticide
  iX7_abb <- input$iX7_abb # 3-letter abbreviated name of insecticide
  cd7 <- input$cd7 # rate for chemical decay of insecticide on bednets 
  md7 <- input$md7 # rate for mortality decay from insecticide on bednets
  mh7 <- input$mh7 # half-life for mortality decay from insecticide on bednets
  xu7 <- input$xu7 # cost in USD per relative unit of AI for the insecticide 
  
  # expert
  xs = c(xs1,xs2) # proportional subsidy of AI costs for the insecticides
  mr_set = c((1-el)^mrx1, 1, 1, 1, 1, (1-el)^mrx6, (1-el)^mrx7) # pre-existing level of resistance to insecticides
  iX_set = c("Pyrethroid", "PBO", "Chlorfenapyr", "Pyriproxyfen", "VECTA", iX6, iX7) # names of insecticides 
  iX_abb = c("PYR", "PBO", "CFP", "PPF", "VCT", iX6_abb, iX7_abb) # 3-letter abbreviated names of insecticides 
  cd_set = c(0.01857947, 0.01777809, 0.02035838, 0.03076537, 0.02067709, cd6, cd7) # rate for chemical decay of insecticides on bednets 
  md_set = c(-0.02454014, -0.062127873, -0.013320582, -0.20854655, -0.0312781, md6, md7) # rate for mortality decay from insecticides on bednets 
  mh_set = c(2.231073, 0.364367554, -0.092747368, 1.2101277, 2.512306, mh6, mh7) # half-life for mortality decay from insecticides on bednets
  xu_set = c(0.20, 0.50, 1.30, 1.90, 2.50, xu6, xu7) # cost in USD per relative unit of AI for each insecticide 
  
  run_simulation(os=os,xb=xb,iA=iA,iB=iB, # from basic settings 
                 og=og,oy=oy,ol=ol,oi_b=oi_b,oi_c=oi_c,ou=ou,
                 er=er,ek=ek,el=el,xn=xn,pm=pm,p0=p0,p3=p3,
                 xs=xs,iX_set=iX_set,iX_abb=iX_abb,mr_set=mr_set,
                 cd_set=cd_set,md_set=md_set,mh_set=mh_set,xu_set=xu_set, token)
}

validate_positive_numbers <- function(input){
  x <- c('xb', 'og', 'oy', 'ol')
  for (i in x){
    value <- input[[i]]
    if (is.na(value) || value < 0){
      showModal(modalDialog(
        title='Invalid input',
        paste0('Input "', PARAMETER_MAP[[i]], '" needs to be a positive number')
      ))
      return(FALSE)
    }
  }
  return(TRUE)
}

validate_0_to_1 <- function(input){
  x <- c('el', 'xs1', 'xs2')
  for (i in x){
    value <- input[[i]]
    if (is.na(value) || value < 0 || value > 1){
      showModal(modalDialog(
        title='Invalid input',
        paste0('Input "', PARAMETER_MAP[[i]], '" needs to be in range [0, 1]')
      ))
      return(FALSE)
    }
  }
  return(TRUE)
}

validate_other <- function(input){
  return(TRUE)
}

validate_settings <- function(input){
  
  v <- validate_positive_numbers(input)
  if (!v) return(FALSE)
  
  v <- validate_0_to_1(input)
  if (!v) return(FALSE)
  
  v <- validate_other(input)
  if (!v) return(FALSE)
  
  return(TRUE)  
}

with_info <- function(html){
  id <- sub('-label', '', get_id(html))
  div(
    class='info',
    title=INFO[[id]],
    html
  )
}

get_id <- function(x){
  if (hasName(x, 'attribs') && hasName(x$attribs, 'id')) return(x$attribs$id)
  if (!hasName(x, 'children')) return()
  for (child in x$children){
    id <- get_id(child)
    if (is.character(id) && length(id) == 1L) return(id)
  }
}
