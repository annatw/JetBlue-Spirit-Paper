estimate_fare_simulation <- function(model_in = "03.Output/random_coeff_nested_logit_results.pickle",
                                       data_in = "02.Intermediate/Product_Data.rds",
                                       data_out = "03.Output/Fare_Estimate_Robustness.rds",
                                       linear = pyblp$Formulation('0 + prices + NonStop + MktMilesFlown + I(MktMilesFlown**2) + Origin_Firm_Service_Ratio + Extra_Miles + Extra_Miles_Sq + Tourism + C(Year_Quarter_Effect) + C(Carrier)'),
                                       nonlinear = pyblp$Formulation("0 + prices + NonStop + MktMilesFlown"),
                                       mode = "rcl"){
  model <- py_load_object(model_in)
  data <- readRDS(data_in)
  
  data[, unobserved := model$xi]
  
  # Compute Costs
  data[, cost := model$compute_costs()]
  data[, price.original := prices]
  data[, shares.original := shares]

  # Indicate Model Components
  if(mode == "rcl"){
    components <- c(linear, nonlinear)
    sigma_vec <- model$sigma;
  } else {
    components <- linear
  }
  
  beta_vec <- model$beta
  beta_labels <- model$beta_labels
  rho_est <- model$rho
  
  
  #Create Simulation Object
 unchanged_simulation <- pyblp$Simulation(product_formulations = components,
                   product_data = data,
                   beta = beta_vec,
                   sigma = sigma_vec,
                   rho = rho_est,
                   integration = pyblp$Integration('product', 9L,
                                                   specification_options = dict("seed" = 97L)),
                   xi = data$unobserved)
 
  
 simulation_unchanged <- unchanged_simulation$replace_endogenous(costs = data$cost); gc();
 
 # Save New Prices, Shares
 data[, prices := py_to_r(simulation_unchanged$product_data$prices)]
 data[, shares := py_to_r(simulation_unchanged$product_data$shares)]
 
  saveRDS(data, file = data_out)
}

average_fare_simulation_error_graph <- function(data_in = "03.Output/Fare_Estimate_Robustness.rds",
                                                Price_Error_Graph,
                                                Share_Error_Graph,
                                                Price_Error_Percent_Graph,
                                                Share_Error_Percent_Graph,
                                                Spirit_JetBlue_Restrict = FALSE){
  data <- readRDS(data_in)

  data[, Price_Error := prices - price.original]
  data[, Share_Error := shares - shares.original]
  data[, Price_Error_Percent := abs(prices - price.original)/price.original * 100]
  data[, Share_Error_Percent := abs(shares - shares.original)/shares.original * 100]
  
  # INSERT CODE RESTRICTING TO SPIRIT JETBLUE MARKETS IF Spirit_JetBlue_Restrict TRUE
  
  ggplot(data, aes(x = Price_Error)) + 
    geom_histogram(binwidth = 0.1)
  ggplot(data, aes(x = Price_Error_Percent)) + geom_histogram()
  ggplot(data, aes(x = Share_Error_Percent)) + geom_histogram(binwidth = 5)
}
