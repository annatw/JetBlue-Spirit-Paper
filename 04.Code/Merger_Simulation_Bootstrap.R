merger_simulation_advanced_bootstrap <- function(model_in = "03.Output/random_coeff_nested_logit_results.pickle",
                                       data_in = "02.Intermediate/Product_Data.rds",
                                       bootstrap_out = "03.Output/bootstrap_results.pickle",
                                       linear = pyblp$Formulation('0 + prices + NonStop + MktMilesFlown + I(MktMilesFlown**2) + Origin_Firm_Service_Ratio + Extra_Miles + Extra_Miles_Sq + Tourism + C(Year_Quarter_Effect) + C(Carrier)'),
                                       nonlinear = pyblp$Formulation("0 + prices + NonStop + MktMilesFlown"),
                                       mode = "rcl"){
  model <- py_load_object(model_in)
  data <- readRDS(data_in)
  
  data[, merger_carrier := Carrier]
  data[Carrier == "Spirit Air Lines", merger_carrier := "JetBlue Airways"]
  data[, unobserved := model$xi]
  
  test <- model$bootstrap(draws = 20L, seed = 0L)
  
  # Compute Costs
  data[, cost := model$compute_costs()]
  
  # Get Original Markets, Consumer Surplus
  original_problem <- pyblp$Problem(c(linear, nonlinear), data,
                                    integration = pyblp$Integration('halton', size = 250L,
                                                                    specification_options = dict("seed" = 97L)))
  original_markets <- as.numeric(original_problem$unique_market_ids)
  cs_observed <- model$compute_consumer_surpluses()
  original_cs <- data.table(market_ids = original_markets, ConsumerSurplus_Observed = cs_observed)
  
  data.new <- data %>% group_by(merger_carrier, Origin, Dest, Year,
                                Quarter, Year_Quarter_Effect, NonStop, 
                                market_ids, nesting_ids) %>%
    summarize(NonStopMiles = min(NonStopMiles),
              MktMilesFlown = min(MktMilesFlown),
              shares = sum(shares),
              prices = mean(prices),
              Tourism = mean(Tourism),
              market_min_miles = min(market_min_miles),
              unobserved.best = max(unobserved),
              unobserved.avg = mean(unobserved),
              unobserved.worst = min(unobserved),
              costs.min = min(cost),
              costs.mean = mean(cost),
              costs.max = max(cost),
              costs.min.95 = min(cost* 0.95^(merger_carrier == "JetBlue Airways")) ,
              costs.min.90 = min(cost* 0.90^(merger_carrier == "JetBlue Airways")) ,
              costs.mean.95 = mean(cost* 0.95^(merger_carrier == "JetBlue Airways")) ,
              costs.mean.90 = mean(cost* 0.90^(merger_carrier == "JetBlue Airways")) ,
              costs.max.95 = max(cost* 0.95^(merger_carrier == "JetBlue Airways")) ,
              costs.max.90 = max(cost * 0.90^(merger_carrier == "JetBlue Airways")) ,
              Potential_Passengers = mean(Potential_Passengers)) %>%
    mutate(Extra_Miles = MktMilesFlown - market_min_miles,
           MktMilesFlown_Sq = MktMilesFlown * MktMilesFlown,
           firm_ids = merger_carrier,
           NonStop = as.numeric(NonStop),
           Carrier = merger_carrier,
           UCC = as.numeric(Carrier %in% c("Spirit Air Lines", "Allegiant Air", "Frontier Airlines Inc."))) %>%
    as.data.table()
  
  data.price <- data %>% select(Origin, Dest, Year, Quarter, Year_Quarter_Effect,
                                NonStop, market_ids, Carrier, merger_carrier,prices, shares) %>%
    filter(!Carrier == "Spirit Air Lines") %>%
    mutate(Fare.Original = prices,
           Share.Original = shares,
           prices = NULL,
           shares = NULL) %>% as.data.table()
  
  data.price[, Share.WithinMarket.Original := Share.Original / sum(Share.Original),
             by = market_ids]
  
  data.new <- merge(data.new, data.price)
  
  remove(data); gc();
  
  # Recompute Origin Prescence Variable
  ratio_data <- readRDS("02.Intermediate/Merger_Service_Ratios.Rds")
  
  colnames(ratio_data) <- c("Year", "Quarter", "Carrier", "Origin",
                            "Origin_Firm_Destinations", "Origin_Firm_Service_Ratio")
  data.new <- merge(data.new, ratio_data, by = c("Year", "Quarter", "Carrier", "Origin"),
                    all.x = TRUE);
  
  # Need to remove Spirit Beta Coeff
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
  
  simulation.best <- pyblp$Simulation(product_formulations = components,
                                      product_data = data.new,
                                      beta = beta_vec,
                                      sigma = sigma_vec,
                                      rho = rho_est,
                                      integration = pyblp$Integration('product', 9L,
                                                                      specification_options = dict("seed" = 97L)),
                                      xi = data.new$unobserved.best)
  
  simulation.avg <- pyblp$Simulation(product_formulations = components,
                                     product_data = data.new,
                                     beta = beta_vec,
                                     sigma = sigma_vec,
                                     rho = rho_est,
                                     integration = pyblp$Integration('product', 9L,
                                                                     specification_options = dict("seed" = 97L)),
                                     xi = data.new$unobserved.avg)
  
  simulation.worst <- pyblp$Simulation(product_formulations = components,
                                       product_data = data.new,
                                       beta = beta_vec,
                                       sigma = sigma_vec,
                                       rho = rho_est,
                                       integration = pyblp$Integration('product', 9L,
                                                                       specification_options = dict("seed" = 97L)),
                                       xi = data.new$unobserved.worst)
  
  
  
  simulation.min <- simulation.best$replace_endogenous(costs = data.new$costs.min); gc();
  simulation.min.95 <- simulation.best$replace_endogenous(costs = data.new$costs.min.95); gc();
  simulation.min.90 <- simulation.best$replace_endogenous(costs = data.new$costs.min.90); gc();
  simulation.mean <- simulation.avg$replace_endogenous(costs = data.new$costs.mean); gc();
  simulation.mean.95 <- simulation.avg$replace_endogenous(costs = data.new$costs.mean.95); gc();
  simulation.mean.90 <- simulation.avg$replace_endogenous(costs = data.new$costs.mean.90); gc();
  simulation.max <- simulation.worst$replace_endogenous(costs = data.new$costs.max); gc()
  simulation.max.95 <- simulation.worst$replace_endogenous(costs = data.new$costs.max.95); gc()
  simulation.max.90 <- simulation.worst$replace_endogenous(costs = data.new$costs.max.90); gc()
  
  
  data.new[, Shares.MinCost.Sim := py_to_r(simulation.min$product_data$shares)]
  data.new[, Shares.MinCost.Sim.95 := py_to_r(simulation.min.95$product_data$shares)]
  data.new[, Shares.MinCost.Sim.90 := py_to_r(simulation.min.90$product_data$shares)]
  data.new[, Shares.MeanCost.Sim := py_to_r(simulation.mean$product_data$shares)]
  data.new[, Shares.MeanCost.Sim.95 := py_to_r(simulation.mean.95$product_data$shares)]
  data.new[, Shares.MeanCost.Sim.90 := py_to_r(simulation.mean.90$product_data$shares)]
  data.new[, Shares.MaxCost.Sim := py_to_r(simulation.max$product_data$shares)]
  data.new[, Shares.MaxCost.Sim.95 := py_to_r(simulation.max.95$product_data$shares)]
  data.new[, Shares.MaxCost.Sim.90 := py_to_r(simulation.max.90$product_data$shares)]
  
  data.new[, Prices.MinCost.Sim := py_to_r(simulation.min$product_data$prices)]
  data.new[, Prices.MinCost.Sim.95 := py_to_r(simulation.min.95$product_data$prices)]
  data.new[, Prices.MinCost.Sim.90 := py_to_r(simulation.min.90$product_data$prices)]
  data.new[, Prices.MeanCost.Sim := py_to_r(simulation.mean$product_data$prices)]
  data.new[, Prices.MeanCost.Sim.95 := py_to_r(simulation.mean.95$product_data$prices)]
  data.new[, Prices.MeanCost.Sim.90 := py_to_r(simulation.mean.90$product_data$prices)]
  data.new[, Prices.MaxCost.Sim := py_to_r(simulation.max$product_data$prices)]
  data.new[, Prices.MaxCost.Sim.95 := py_to_r(simulation.max.95$product_data$prices)]
  data.new[, Prices.MaxCost.Sim.90 := py_to_r(simulation.max.90$product_data$prices)]
  
  
  data.new[, Shares.WithinMarket.MinCost := Shares.MinCost.Sim / sum(Shares.MinCost.Sim),
           by = c("market_ids")]
  data.new[, Shares.WithinMarket.MinCost.95 := Shares.MinCost.Sim.95 / sum(Shares.MinCost.Sim.95),
           by = c("market_ids")]
  data.new[, Shares.WithinMarket.MinCost.90 := Shares.MinCost.Sim.90 / sum(Shares.MinCost.Sim.90),
           by = c("market_ids")]
  
  data.new[, Shares.WithinMarket.MeanCost := Shares.MeanCost.Sim / sum(Shares.MinCost.Sim),
           by = c("market_ids")]
  data.new[, Shares.WithinMarket.MeanCost.95 := Shares.MeanCost.Sim.95 / sum(Shares.MinCost.Sim.95),
           by = c("market_ids")]
  data.new[, Shares.WithinMarket.MeanCost.90 := Shares.MeanCost.Sim.90 / sum(Shares.MinCost.Sim.90),
           by = c("market_ids")]
  
  data.new[, Shares.WithinMarket.MaxCost := Shares.MaxCost.Sim / sum(Shares.MinCost.Sim),
           by = c("market_ids")]
  data.new[, Shares.WithinMarket.MaxCost.95 := Shares.MaxCost.Sim.95 / sum(Shares.MinCost.Sim.95),
           by = c("market_ids")]
  data.new[, Shares.WithinMarket.MaxCost.90 := Shares.MaxCost.Sim.90 / sum(Shares.MinCost.Sim.90),
           by = c("market_ids")]
  
  
  # Add each markets Consumer Surplus
  markets <- as.numeric(simulation.best$unique_market_ids)
  cs.best <- as.numeric(simulation.min$compute_consumer_surpluses())
  cs.avg <- as.numeric(simulation.mean$compute_consumer_surpluses())
  cs.worst <- as.numeric(simulation.max$compute_consumer_surpluses())
  
  cs_table <- data.table(market_ids = markets,
                         CS_Best = cs.best,
                         CS_Average = cs.avg,
                         CS_Worst = cs.worst)
  
  cs_table <- merge(cs_table, original_cs, by = "market_ids", all.x = TRUE)
  data.new <- merge(data.new, cs_table, by = "market_ids", all.x = TRUE)
  
  saveRDS(data.new, file = data_out)
}
