spirit_exit_simulation <- function(model_in = "03.Output/random_coeff_nested_logit_results.pickle",
                                       data_in = "02.Intermediate/Product_Data.rds",
                                       data_out = "03.Output/Exit_Sim_Data.rds",
                                       linear = pyblp$Formulation('0 + prices + NonStop + MktMilesFlown + I(MktMilesFlown**2) + Origin_Firm_Service_Ratio + Extra_Miles + Extra_Miles_Sq + Tourism + C(Year_Quarter_Effect) + C(Carrier)'),
                                       nonlinear = pyblp$Formulation("0 + prices + NonStop + MktMilesFlown"),
                                       mode = "rcl"){
  model <- py_load_object(model_in)
  data <- readRDS(data_in)
  
  # Use the Estimated Product Level Unobservables
  data[, unobserved := model$xi]
  
  # Estimate Marginal Costs
  data[, cost := model$compute_costs()]
  
  # Save original Shares, Prices
  data[, Original_Shares := shares]
  data[, Original_Prices := prices]
  
  # Now, remove Spirit Products
  data <- data[Carrier != "Spirit Air Lines"]
  
  # Need to remove Spirit Beta Coefficient from the Model
  beta_vec <- model$beta
  beta_labels <- model$beta_labels
  beta_vec <- beta_vec[!grepl(pattern = "Spirit", x = beta_labels)]
  rho_est <- model$rho
  
  
  if(mode == "rcl"){
    components <- c(linear, nonlinear)
    sigma_vec <- model$sigma;
  } else {
    components <- linear
  }
  
  remove(model); gc()
  
  # Simulation Without Spirit Products
  simulation <- pyblp$Simulation(product_formulations = components,
                                      product_data = data,
                                      beta = beta_vec,
                                      sigma = sigma_vec,
                                      rho = rho_est,
                                      integration = pyblp$Integration('product', 9L,
                                                                      specification_options = dict("seed" = 97L)),
                                      xi = data$unobserved)
  simulation <- simulation$replace_endogenous(costs = data$cost); gc();
  
  data[, Spirit_Exit_Shares := py_to_r(simulation$product_data$shares)]
  data[, Spirit_Exit_Prices := py_to_r(simulation$product_data$prices)]
  
  saveRDS(data, file = data_out)
}
