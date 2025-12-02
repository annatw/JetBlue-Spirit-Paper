frontier_merger_simulation_advanced <- function(model_in = "03.Output/random_coeff_nested_logit_results.pickle",
                                       data_in = "02.Intermediate/Product_Data.rds",
                                       data_out = "03.Output/Frontier_Merger_Sim_Data.rds",
                                       linear = pyblp$Formulation('0 + prices + NonStop + MktMilesFlown + I(MktMilesFlown**2) + Origin_Firm_Service_Ratio + Extra_Miles + Extra_Miles_Sq + Tourism + C(Year_Quarter_Effect) + C(Carrier)'),
                                       nonlinear = pyblp$Formulation("0 + prices + NonStop + MktMilesFlown"),
                                       mode = "rcl"){
  model <- py_load_object(model_in)
  data <- readRDS(data_in)
  
  data[, merger_carrier := Carrier]
  data[Carrier == "Spirit Air Lines", merger_carrier := "Frontier Airlines Inc."]
  data[, unobserved := model$xi]
  
  # Compute Costs
  data[, cost := model$compute_costs()]
  
  # Get Original Markets, Consumer Surplus
  original_problem <- pyblp$Problem(c(linear, nonlinear), data,
                                    integration = pyblp$Integration('halton', size = 250L,
                                                                    specification_options = dict("seed" = 97L)))
  original_markets <- as.numeric(original_problem$unique_market_ids)
  
  cs_observed <- model$compute_consumer_surpluses() * 100
  original_cs <- data.table(market_ids = original_markets, ConsumerSurplus_Observed_Normalized = as.numeric(cs_observed))
  
  data.new <- data %>% group_by(merger_carrier, Origin, Dest, Year,
                                Quarter, Year_Quarter_Effect, NonStop, 
                                market_ids, Potential_Passengers, nesting_ids) %>%
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
  
  if(mode == "rcl"){
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
  } else {
    simulation.best <- pyblp$Simulation(product_formulations = components,
                                        product_data = data.new,
                                        beta = beta_vec,
                                        rho = rho_est,
                                        integration = pyblp$Integration('product', 9L,
                                                                        specification_options = dict("seed" = 97L)),
                                        xi = data.new$unobserved.best)
    
    simulation.avg <- pyblp$Simulation(product_formulations = components,
                                       product_data = data.new,
                                       beta = beta_vec,
                                       rho = rho_est,
                                       integration = pyblp$Integration('product', 9L,
                                                                       specification_options = dict("seed" = 97L)),
                                       xi = data.new$unobserved.avg)
    
    simulation.worst <- pyblp$Simulation(product_formulations = components,
                                         product_data = data.new,
                                         beta = beta_vec,
                                         rho = rho_est,
                                         integration = pyblp$Integration('product', 9L,
                                                                         specification_options = dict("seed" = 97L)),
                                         xi = data.new$unobserved.worst)
  }
  
  
  
  
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
  cs.best <- as.numeric(simulation.min$compute_consumer_surpluses()) * 100
  cs.avg <- as.numeric(simulation.mean$compute_consumer_surpluses()) * 100
  cs.worst <- as.numeric(simulation.max$compute_consumer_surpluses()) * 100
  
  cs_table <- data.table(market_ids = markets,
                         CS_Best_Normalized = cs.best,
                         CS_Average_Normalized = cs.avg,
                         CS_Worst_Normalized = cs.worst)
  
  cs_table <- merge(cs_table, original_cs, by = "market_ids", all.x = TRUE)
  data.new <- merge(data.new, cs_table, by = "market_ids", all.x = TRUE)
  
  # Unnormalize the Consumer Surplus Estimates
  data.new <- as.data.table(data.new)
  data.new[, ConsumerSurplus_Observed := ConsumerSurplus_Observed_Normalized * Potential_Passengers]
  data.new[, CS_Best := CS_Best_Normalized * Potential_Passengers]
  data.new[, CS_Avg := CS_Average_Normalized * Potential_Passengers]
  data.new[, CS_Worst := CS_Worst_Normalized * Potential_Passengers]
  
  saveRDS(data.new, file = data_out)
}

frontier_merger_results_table <- function(merger_post = "03.Output/Frontier_Merger_Sim_Data.rds",
                                 observed_post = "02.Intermediate/Product_Data.rds",
                                 merger_pre = "03.Output/Frontier_Prepandemic_Merger_Sim_Data.rds",
                                 observed_pre = "02.Intermediate/prepandemic.rds",
                                 table_out = "06.Tables/Frontier_Merger_Results.tex",
                                 mode = 0,
                                 include_median = FALSE){
  output_rows <- function(merger_in, observed_in, median_rows = include_median){
    merger <- readRDS(merger_in)
    observed <- readRDS(observed_in)
    
    # Rename Variables in Efficiency Case
    if(mode == 5){
      merger$Prices.MinCost.Sim <- merger$Prices.MinCost.Sim.95
      merger$Prices.MeanCost.Sim <- merger$Prices.MeanCost.Sim.95
      merger$Prices.MaxCost.Sim <- merger$Prices.MaxCost.Sim.95
      
      merger$Shares.WithinMarket.MaxCost <- merger$Shares.WithinMarket.MaxCost.95
      merger$Shares.WithinMarket.MinCost <- merger$Shares.WithinMarket.MinCost.95
      merger$Shares.WithinMarket.MeanCost <- merger$Shares.WithinMarket.MeanCost.95
    } else if (mode == 10){
      merger$Prices.MinCost.Sim <- merger$Prices.MinCost.Sim.90
      merger$Prices.MeanCost.Sim <- merger$Prices.MeanCost.Sim.90
      merger$Prices.MaxCost.Sim <- merger$Prices.MaxCost.Sim.90
      merger$Shares.WithinMarket.MaxCost <- merger$Shares.WithinMarket.MaxCost.90
      merger$Shares.WithinMarket.MinCost <- merger$Shares.WithinMarket.MinCost.90
      merger$Shares.WithinMarket.MeanCost <- merger$Shares.WithinMarket.MeanCost.90
    }
    
    # For each market, identify if Frontier was in that Market
    observed[, Frontier := Carrier == "Frontier Airlines Inc."]
    
    # Identify markets which merger impacted
    impactedMarkets <- observed %>% group_by(market_ids) %>%
      summarize(Spirit = max(Spirit),
                Frontier = max(Frontier)) %>% 
      filter(Spirit == 1, Frontier == 1); impactedMarkets <- impactedMarkets$market_ids
    
    # Restrict
    merger <- merger[market_ids %in% impactedMarkets,]
    observed <- observed[market_ids %in% impactedMarkets]
    
    # Cluster 1: Prices
    price.obs <- six_statistic_row_make("Observed", observed$prices)
    price.best <- six_statistic_row_make("Best Case", merger$Prices.MinCost.Sim)
    price.avg <- six_statistic_row_make("Average Case", merger$Prices.MeanCost.Sim)
    price.worst <- six_statistic_row_make("Worst Case", merger$Prices.MaxCost.Sim)
    
    # Summarize Mean Price Change in Each Market
    mergerPrice <- merger %>% group_by(market_ids) %>%
      summarize(MeanPrice.MaxCost = sum(Shares.WithinMarket.MaxCost * Prices.MaxCost.Sim)/sum(Shares.WithinMarket.MaxCost),
                MeanPrice.MinCost = sum(Shares.WithinMarket.MinCost * Prices.MinCost.Sim)/sum(Shares.WithinMarket.MinCost),
                MeanPrice.MeanCost = sum(Shares.WithinMarket.MeanCost * Prices.MaxCost.Sim)/sum(Shares.WithinMarket.MeanCost))
    
    observed[, withinMarketShares := sum(shares), by = market_ids]
    observedPrice <- observed %>% group_by(market_ids) %>%
      summarize(MeanPrice.Real = sum(withinMarketShares * prices) / sum(withinMarketShares))
    
    mergerPrice <- merge(mergerPrice, observedPrice, by = "market_ids", all.x = TRUE)
    
    # Cluster 2: Market Mean Price
    meanPrice.obs <- six_statistic_row_make("Observed", mergerPrice$MeanPrice.Real)
    meanPrice.best <- six_statistic_row_make("Best Case", mergerPrice$MeanPrice.MinCost)
    meanPrice.avg <- six_statistic_row_make("Average Case", mergerPrice$MeanPrice.MeanCost)
    meanPrice.worst <- six_statistic_row_make("Worst Case", mergerPrice$MeanPrice.MaxCost)
    
    # Cluster 3: % Change in Market Mean Price
    change.best <- six_statistic_row_make("Best Case", (mergerPrice$MeanPrice.MinCost - mergerPrice$MeanPrice.Real)/mergerPrice$MeanPrice.Real * 100)
    change.avg <- six_statistic_row_make("Average Case", (mergerPrice$MeanPrice.MeanCost - mergerPrice$MeanPrice.Real)/mergerPrice$MeanPrice.Real * 100)
    change.worst <- six_statistic_row_make("Worst Case", (mergerPrice$MeanPrice.MaxCost - mergerPrice$MeanPrice.Real)/mergerPrice$MeanPrice.Real * 100)
    
    # Cluster 4: Median Price
    # Calculate Est Passengers
    merger$Passengers.BestCase <- round(merger$Potential_Passengers * merger$Shares.MinCost.Sim)
    merger$Passengers.AvgCase <- round(merger$Potential_Passengers * merger$Shares.MeanCost.Sim)
    merger$Passengers.WorstCase <- round(merger$Potential_Passengers * merger$Shares.MaxCost.Sim)
    mergerMedian <- merger %>% group_by(market_ids) %>%
      summarize(MedianBest = median(rep(Prices.MinCost.Sim, Passengers.BestCase), na.rm = TRUE),
                MedianAvg = median(rep(Prices.MeanCost.Sim, Passengers.AvgCase), na.rm = TRUE),
                MedianWorst = median(rep(Prices.MaxCost.Sim, Passengers.WorstCase), na.rm = TRUE))
    observedMedian <- observed %>% group_by(market_ids) %>%
      summarize(Median = median(rep(prices, Passengers.Product)))
    
    medianPrice.obs <- six_statistic_row_make("Observed", observedMedian$Median)
    medianPrice.best <- six_statistic_row_make("Best Case", mergerMedian$MedianBest)
    medianPrice.avg <- six_statistic_row_make("Average Case", mergerMedian$MedianAvg)
    medianPrice.worst <- six_statistic_row_make("Worst Case", mergerMedian$MedianWorst)
    
    if(median_rows == TRUE){
      table <- rbind(price.obs, price.best, price.avg, price.worst,
                     meanPrice.obs, meanPrice.best, meanPrice.avg, meanPrice.worst,
                     change.best, change.avg, change.worst,
                     medianPrice.obs, medianPrice.best, medianPrice.avg, medianPrice.worst)
    } else {
      table <- rbind(price.obs, price.best, price.avg, price.worst,
                     meanPrice.obs, meanPrice.best, meanPrice.avg, meanPrice.worst,
                     change.best, change.avg, change.worst)
    }
    
    rownames(table) <- NULL
    return(table)
  }
  
  post_pandemic <- output_rows(merger_in = merger_post,
                               observed_in = observed_post)
  pre_pandemic <- output_rows(merger_in = merger_pre,
                              observed_in = observed_pre)
  
  title_row <- c("", "N", "Mean", "(SD)", "Minimum", "Median", "Maximum")
  table <- rbind(pre_pandemic, post_pandemic)
  
  if(include_median == TRUE){
    kbl(table,
        format = "latex", col.names = title_row,
        escape = TRUE, booktabs = TRUE) %>%
      pack_rows(group_label = "Pre-Pandemic", 1, 15) %>%
      pack_rows(group_label = "Product Prices (100s, 2017 USD)", 1, 4) %>%
      pack_rows(group_label = "Market Average Price (100s, 2017 USD)", 5, 8) %>%
      pack_rows(group_label = "% Change Average Price", 9, 11) %>%
      pack_rows(group_label = "Median Price (100s, 2017 USD)", 12, 15) %>%
      pack_rows(group_label = "Post-Pandemic", 16, 30) %>%
      pack_rows(group_label = "Product Prices  (100s, 2017 USD)", 16, 19) %>%
      pack_rows(group_label = "Market Average Price (100s, 2017 USD)", 20, 23) %>%
      pack_rows(group_label = "% Change Average Price", 24, 26) %>%
      pack_rows(group_label = "Median Price (100s, 2017 USD)", 27, 30) %>%
      row_spec(row = 15, hline_after = TRUE) %>%
      save_kable(file = table_out)
  } else {
    kbl(table,
        format = "latex", col.names = title_row,
        escape = TRUE, booktabs = TRUE) %>%
      pack_rows(group_label = "Pre-Pandemic", 1, 11) %>%
      pack_rows(group_label = "Product Prices (100s, 2017 USD)", 1, 4) %>%
      pack_rows(group_label = "Market Average Price (100s, 2017 USD)", 5, 8) %>%
      pack_rows(group_label = "% Change Average Price", 9, 11) %>%
      pack_rows(group_label = "Post-Pandemic", 12, 22) %>%
      pack_rows(group_label = "Product Prices  (100s, 2017 USD)", 12, 15) %>%
      pack_rows(group_label = "Market Average Price (100s, 2017 USD)", 16, 19) %>%
      pack_rows(group_label = "% Change Average Price", 20, 22) %>%
      row_spec(row = 11, hline_after = TRUE) %>%
      save_kable(file = table_out)
  }
}

frontier_minimum_fare_percent_increase_table <- function(merger_data.post = "03.Output/Frontier_Merger_Sim_Data.rds",
                                                observed.post = "02.Intermediate/Product_Data.rds",
                                                merger_data.pre =  "03.Output/Frontier_Prepandemic_Merger_Sim_Data.rds",
                                                observed.pre = "02.Intermediate/prepandemic.rds",
                                                table_out = "06.Tables/Frontier_MinimumFareChange_Percent.tex",
                                                mode = 0,
                                                buckets = c("$<$ 0", "0-20", "20-40", "40-60",
                                                            "60-80", "80 $<$"),
                                                break_points = c(-Inf, 0, 20, 40, 60, 80, Inf)){
  period_table_make <- function(real, merger, buckets, break_points){
    real <- readRDS(real); merger <- readRDS(merger)
    
    real[, Spirit_Prescence := max(Spirit_Prescence), by = c("Year", "Quarter", "Origin",
                                                             "Dest")]
    real[, Frontier_Prescence := (Carrier == "Frontier Airlines Inc."), 
             by = c("Year", "Quarter", "Origin", "Dest")]
    
    # Compute Costs
    shared_markets <- unique(real[Spirit_Prescence == 1 & Frontier_Prescence == 1, market_ids])
    
    # For each market, identify its minimum price pre, post merger. 
    if(mode == 5){
      merger$Prices.MinCost.Sim <- merger$Prices.MinCost.Sim.95
      merger$Prices.MeanCost.Sim <- merger$Prices.MeanCost.Sim.95
      merger$Prices.MaxCost.Sim <- merger$Prices.MaxCost.Sim.95
      merger$Shares.WithinMarket.MaxCost <- merger$Shares.WithinMarket.MaxCost.95
      merger$Shares.WithinMarket.MinCost <- merger$Shares.WithinMarket.MinCost.95
      merger$Shares.WithinMarket.MeanCost <- merger$Shares.WithinMarket.MeanCost.95
    } else if (mode == 10){
      merger$Prices.MinCost.Sim <- merger$Prices.MinCost.Sim.90
      merger$Prices.MeanCost.Sim <- merger$Prices.MeanCost.Sim.90
      merger$Prices.MaxCost.Sim <- merger$Prices.MaxCost.Sim.90
      merger$Shares.WithinMarket.MaxCost <- merger$Shares.WithinMarket.MaxCost.90
      merger$Shares.WithinMarket.MinCost <- merger$Shares.WithinMarket.MinCost.90
      merger$Shares.WithinMarket.MeanCost <- merger$Shares.WithinMarket.MeanCost.90
      
    }
    
    merger <- merger %>% filter(market_ids %in% shared_markets) %>% group_by(market_ids, Year, Quarter, Origin, Dest) %>%
      summarize(Best.Min = min(Prices.MinCost.Sim),
                Avg.Min = min(Prices.MeanCost.Sim),
                Worst.Min = min(Prices.MaxCost.Sim))
    
    real <- real %>% filter(market_ids %in% shared_markets) %>% group_by(market_ids) %>%
      summarize(Prices.Real = min(prices))
    
    out <- as.data.table(merge(real, merger, by = "market_ids"))
    out[, Best := round(100*(Best.Min - Prices.Real)/Prices.Real, digits = 2)]
    out[, Average := round(100*(Avg.Min - Prices.Real)/Prices.Real, digits = 2)]
    out[, Worst := round(100*(Worst.Min - Prices.Real)/Prices.Real, digits = 2)]
    
    
    out[, bucket.Best := cut(Best,
                             breaks = break_points,
                             labels = buckets)]
    out[, bucket.Average := cut(Average,
                                breaks = break_points,
                                labels = buckets)]
    out[, bucket.Worst := cut(Worst,
                              breaks = break_points,
                              labels = buckets)]
    best <- out[, .N, by = bucket.Best]; average <- out[, .N, by = bucket.Average];
    worst <- out[, .N, by = bucket.Worst]
    
    colnames(best) <- c("Bucket", "Best"); colnames(average) <- c("Bucket", "Average");
    colnames(worst) <- c("Bucket", "Worst")
    
    out <- merge(best, average, by = "Bucket")
    out <- merge(out, worst, by = "Bucket")
    
    #Re-Order Rows, Fix Missing Rows
    re_order_frame <- data.table(Bucket = buckets, Val = rep(0, times = length(buckets)))
    out <- merge(re_order_frame, out, all.x = TRUE, all.y = TRUE)
    out$Val <- NULL
    
    # Correct Any NA Values created by Merging to be 0's
    col_correct <- c("Best", "Average", "Worst")
    setDT(out)[, (col_correct) := lapply(.SD, replace_na, 0), .SDcols = col_correct]
    
    return(out)
  }
  
  postPandemic <- period_table_make(real = observed.post, merger = merger_data.post, buckets, break_points)
  prePandemic <- period_table_make(real = observed.pre, merger = merger_data.pre, buckets, break_points)
  
  colnames(postPandemic) <- c("Bucket", paste("Post.", colnames(postPandemic)[2:4], sep = ""))
  colnames(prePandemic) <- c("Bucket", paste("Pre.", colnames(prePandemic)[2:4], sep = ""))
  
  output <- cbind(prePandemic, postPandemic[, 2:4])
  colnames <- c("", "Best", "Average", "Worst", "Best", "Average", "Worst")
  
  rownames(output) <- NULL
  
  kbl(output,
      format = "latex", 
      escape = FALSE, booktabs = TRUE,
      col.names = colnames, linesep = "") %>%
    add_header_above(c(" ", "Pre-Pandemic" = 3, "Post-Pandemic" = 3)) %>%
    save_kable(file = table_out)
}


