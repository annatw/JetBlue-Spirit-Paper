# Mode is One of 0, 5, 10 to indicate marginal cost discounts
merger_results_table <- function(merger_post = "03.Output/Adv_Merger_Sim_Data.rds",
                                 observed_post = "02.Intermediate/Product_Data.rds",
                                 merger_pre = "03.Output/PrePandemic_Adv_Merger_Sim_Data.rds",
                                 observed_pre = "02.Intermediate/prepandemic.rds",
                                 table_out = "06.Tables/Merger_Results.tex",
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
    
    # Identify markets which merger impacted
    impactedMarkets <- observed %>% group_by(market_ids) %>%
      summarize(Spirit = max(Spirit),
                JetBlue = max(JetBlue)) %>% 
      filter(Spirit == 1, JetBlue == 1); impactedMarkets <- impactedMarkets$market_ids
    
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

merger_results_spirit_markets <- function(merger_post = "03.Output/Adv_Merger_Sim_Data.rds",
                                          observed_post = "02.Intermediate/Product_Data.rds",
                                          merger_pre = "03.Output/PrePandemic_Adv_Merger_Sim_Data.rds",
                                          observed_pre = "02.Intermediate/prepandemic.rds",
                                          table_out = "06.Tables/Merger_Spirit_Market_Results.tex"){
  output_rows <- function(merger_in, observed_in){
    merger <- readRDS(merger_in)
    observed <- readRDS(observed_in)
    
    # Identify markets with Spirit, No JB
    impactedMarkets <- observed %>% group_by(market_ids) %>%
      summarize(Spirit = max(Spirit),
                JetBlue = max(JetBlue)) %>% 
      filter(Spirit == 1, JetBlue == 0); impactedMarkets <- impactedMarkets$market_ids
    
    # Restrict
    merger <- merger[market_ids %in% impactedMarkets,]
    observed <- observed[market_ids %in% impactedMarkets]
    
    # Cluster 1: Prices
    price.obs <- six_statistic_row_make("Observed", observed$prices)
    price.best <- six_statistic_row_make("Simulated", merger$Prices.MinCost.Sim)

    # Summarize Mean Price Change in Each Market
    merger <- merger %>% group_by(market_ids) %>%
      summarize(MeanPrice.MaxCost = sum(Shares.WithinMarket.MaxCost * Prices.MaxCost.Sim)/sum(Shares.WithinMarket.MaxCost),
                MeanPrice.MinCost = sum(Shares.WithinMarket.MinCost * Prices.MinCost.Sim)/sum(Shares.WithinMarket.MinCost),
                MeanPrice.MeanCost = sum(Shares.WithinMarket.MeanCost * Prices.MaxCost.Sim)/sum(Shares.WithinMarket.MeanCost))
    
    observed[, withinMarketShares := sum(shares), by = market_ids]
    observed <- observed %>% group_by(market_ids) %>%
      summarize(MeanPrice.Real = sum(withinMarketShares * prices) / sum(withinMarketShares))
    
    merger <- merge(merger, observed, by = "market_ids", all.x = TRUE)
    
    # Cluster 2: Market Mean Price
    meanPrice.obs <- six_statistic_row_make("Observed", merger$MeanPrice.Real)
    meanPrice.best <- six_statistic_row_make("Simulated", merger$MeanPrice.MinCost)

    # Cluster 3: % Change in Market Mean Price
    change.best <- six_statistic_row_make("Simulated", (merger$MeanPrice.MinCost - merger$MeanPrice.Real)/merger$MeanPrice.Real * 100)

    table <- rbind(price.obs, price.best, 
                   meanPrice.obs, meanPrice.best,
                   change.best )
    rownames(table) <- NULL
    return(table)
  }
  
  post_pandemic <- output_rows(merger_in = merger_post,
                               observed_in = observed_post)
  pre_pandemic <- output_rows(merger_in = merger_pre,
                              observed_in = observed_pre)
  
  title_row <- c("", "N", "Mean", "(SD)", "Minimum", "Median", "Maximum")
  table <- rbind(pre_pandemic, post_pandemic)
  
  kbl(table,
      format = "latex", col.names = title_row,
      escape = TRUE, booktabs = TRUE) %>%
    pack_rows(group_label = "Pre-Pandemic", 1, 5) %>%
    pack_rows(group_label = "Prices (100s, 2017 USD)", 1, 2) %>%
    pack_rows(group_label = "Market Average Price", 3, 4) %>%
    pack_rows(group_label = "% Change Average Price", 5,5) %>%
    pack_rows(group_label = "Post-Pandemic", 6, 10) %>%
    pack_rows(group_label = "Prices  (100s, 2017 USD)", 6, 7) %>%
    pack_rows(group_label = "Market Average Price", 8, 9) %>%
    pack_rows(group_label = "% Change Average Price", 10,10) %>%
    row_spec(row = 5, hline_after = TRUE) %>%
    save_kable(file = table_out)
  
}


elasticity_compare_table <- function(postPand_input = "03.Output/random_coeff_nested_logit_results.pickle",
                                     prePand_input = "03.Output/prepandemic_random_coeff_nested_logit.pickle",
                                     table_out = "06.Tables/Elasticity_Compare_Table.tex"){
  postPandemic <- py_load_object(postPand_input);
  prePandemic <- py_load_object(prePand_input);
  
  header <- c("Paper", "Period Analyzed", "Estimated Average Elasticity", "Notes")
  row1 <- c("Berry and Jia (2010)", "1999", "-1.69", "")
  row2 <- c("Berry and Jia (2010)", "2006", "-1.67", "")
  row3 <- c("Gayle (2013)", "2006Q1 - 2006Q4","-4.72", "")
  row4 <- c("Ciliberto and Williams (2014)", "2006Q1 - 2008Q4", "-4.320", "")
  row5 <- c("Ciliberto and Williams (2021)", "2012Q2", "[-7.281, -7.063]", "Median, Exogeneous Entry Model")
  row6 <- c("Ciliberto and Williams (2021)", "2012Q2", "[-4.105, -4.007]", "Median, Endogeneous Entry Model")
  row7 <- c("Turner (2022)", "2000Q3", "-2.107", "")
  row8 <- c("Turner (2022)", "2018Q3", "-4.102", "")
  prePandemic_Row <- c("", "2017Q1 - 2019Q4", round(mean(prePandemic$extract_diagonal_means(prePandemic$compute_elasticities())), digits = 3), "")
  postPandemic_Row <- c("", "2021Q2 - 2023Q2", round(mean(postPandemic$extract_diagonal_means(postPandemic$compute_elasticities())), digits = 3), "")
  
  table <- rbind(row1, row2, row3, row4, row5, row6, row7, row8,
                 prePandemic_Row, postPandemic_Row)  
  
  rownames(table) <- NULL
  
  kbl(table,
      format = "latex", 
      escape = FALSE, booktabs = TRUE,
      col.names = header) %>%
    pack_rows("Prior Literature", start_row = 1, end_row = 8) %>%
    pack_rows("New Results", start_row = 9, end_row = 10) %>%
    save_kable(file = table_out)
}

minimum_fare_increase_table <- function(merger_data.post = "03.Output/Adv_Merger_Sim_Data.rds",
                                        observed.post = "02.Intermediate/Product_Data.rds",
                                        merger_data.pre =  "03.Output/PrePandemic_Adv_Merger_Sim_Data.rds",
                                        observed.pre = "02.Intermediate/prepandemic.rds",
                                        table_out = "06.Tables/MinimumFareChange.tex",
                                        mode = 0){
  period_table_make <- function(real, merger){
    real <- readRDS(real); merger <- readRDS(merger)
    
    real[, Spirit_Prescence := max(Spirit_Prescence), by = c("Year", "Quarter", "Origin",
                                                                 "Dest")]
    real[, JetBlue_Prescence := max(JetBlue_Prescence), by = c("Year", "Quarter", "Origin",
                                                                   "Dest")]
    
    # Compute Costs
    shared_markets <- unique(real[Spirit_Prescence == 1 & JetBlue_Prescence == 1, market_ids])
    
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
    out[, Best := round(100*(Best.Min - Prices.Real), digits = 2)]
    out[, Average := round(100*(Avg.Min - Prices.Real), digits = 2)]
    out[, Worst := round(100*(Worst.Min - Prices.Real), digits = 2)]
    
    out[, bucket.Best := cut(Best,
                             breaks = c(-Inf, 0, 20, 40, 60, 80, Inf),
                             labels = c("$<$ 0", "0-20", "20-40", "40-60",
                                        "60-80", "80 $<$"))]
    out[, bucket.Average := cut(Average,
                                breaks = c(-Inf, 0, 20, 40, 60, 80, Inf),
                                labels = c("$<$ 0", "0-20", "20-40", "40-60",
                                           "60-80", "80 $<$"))]
    out[, bucket.Worst := cut(Worst,
                              breaks = c(-Inf, 0, 20, 40, 60, 80, Inf),
                              labels = c("$<$ 0", "0-20", "20-40", "40-60",
                                         "60-80", "80 $<$"))]
    best <- out[, .N, by = bucket.Best]; average <- out[, .N, by = bucket.Average];
    worst <- out[, .N, by = bucket.Worst]
    
    colnames(best) <- c("Bucket", "Best"); colnames(average) <- c("Bucket", "Average");
    colnames(worst) <- c("Bucket", "Worst")
    
    out <- merge(best, average, by = "Bucket")
    out <- merge(out, worst, by = "Bucket")
    
    return(out)
  }
  
  postPandemic <- period_table_make(real = observed.post, merger = merger_data.post)
  prePandemic <- period_table_make(real = observed.pre, merger = merger_data.pre)
  
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

minimum_fare_percent_increase_table <- function(merger_data.post = "03.Output/Adv_Merger_Sim_Data.rds",
                                        observed.post = "02.Intermediate/Product_Data.rds",
                                        merger_data.pre =  "03.Output/PrePandemic_Adv_Merger_Sim_Data.rds",
                                        observed.pre = "02.Intermediate/prepandemic.rds",
                                        table_out = "06.Tables/MinimumFareChange_Percent.tex",
                                        mode = 0){
  period_table_make <- function(real, merger){
    real <- readRDS(real); merger <- readRDS(merger)
    
    real[, Spirit_Prescence := max(Spirit_Prescence), by = c("Year", "Quarter", "Origin",
                                                             "Dest")]
    real[, JetBlue_Prescence := max(JetBlue_Prescence), by = c("Year", "Quarter", "Origin",
                                                               "Dest")]
    
    # Compute Costs
    shared_markets <- unique(real[Spirit_Prescence == 1 & JetBlue_Prescence == 1, market_ids])
    
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
    out[, Average := round(100*(Avg.Min - Prices.Real/Prices.Real), digits = 2)]
    out[, Worst := round(100*(Worst.Min - Prices.Real/Prices.Real), digits = 2)]
    
    out[, bucket.Best := cut(Best,
                             breaks = c(-Inf, 0, 20, 40, 60, 80, Inf),
                             labels = c("$<$ 0", "0-20", "20-40", "40-60",
                                        "60-80", "80 $<$"))]
    out[, bucket.Average := cut(Average,
                                breaks = c(-Inf, 0, 20, 40, 60, 80, Inf),
                                labels = c("$<$ 0", "0-20", "20-40", "40-60",
                                           "60-80", "80 $<$"))]
    out[, bucket.Worst := cut(Worst,
                              breaks = c(-Inf, 0, 20, 40, 60, 80, Inf),
                              labels = c("$<$ 0", "0-20", "20-40", "40-60",
                                         "60-80", "80 $<$"))]
    best <- out[, .N, by = bucket.Best]; average <- out[, .N, by = bucket.Average];
    worst <- out[, .N, by = bucket.Worst]
    
    colnames(best) <- c("Bucket", "Best"); colnames(average) <- c("Bucket", "Average");
    colnames(worst) <- c("Bucket", "Worst")
    
    out <- merge(best, average, by = "Bucket")
    out <- merge(out, worst, by = "Bucket")
    
    return(out)
  }
  
  postPandemic <- period_table_make(real = observed.post, merger = merger_data.post)
  prePandemic <- period_table_make(real = observed.pre, merger = merger_data.pre)
  
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


merger_simulation_hhi_change <- function(merger_post = "03.Output/Adv_Merger_Sim_Data.rds",
                                         observed_post = "02.Intermediate/Product_Data.rds",
                                         merger_pre = "03.Output/PrePandemic_Adv_Merger_Sim_Data.rds",
                                         observed_pre = "02.Intermediate/prepandemic.rds",
                                         table_out = "06.Tables/Merger_HHI_Results.tex"){
  table_make <- function(merger_in, observed_in){
    observed <- readRDS(observed_in)
    
    observed[, Spirit_Prescence := max(Spirit_Prescence), by = c("Year", "Quarter", "Origin",
                                                                 "Dest")]
    observed[, JetBlue_Prescence := max(JetBlue_Prescence), by = c("Year", "Quarter", "Origin",
                                                                   "Dest")]
    
    # Compute Costs
    shared_markets <- unique(observed[Spirit_Prescence == 1 & JetBlue_Prescence == 1, market_ids])
    
    
    observed[, totalRevShare := sum(prices * shares), by = c("market_ids")]
    observed[, totalShare := sum(shares), by = c("market_ids")]
    observed[, ownRevShare := prices * shares / totalRevShare * 100]
    observed[, ownShare := shares / totalShare * 100]
    observed <- observed %>% filter(market_ids %in% shared_markets) %>% group_by(market_ids) %>%
      summarize(HHI.Share = sum(ownShare^2),
                HHI.Rev = sum(ownRevShare^2))
    
    merger <- readRDS(merger_in)
    merger[, totalShare.Max := sum(Shares.MaxCost.Sim), by = c("market_ids")]
    merger[, totalRevShare.Max := sum(Shares.MaxCost.Sim * Prices.MaxCost.Sim), by = c("market_ids")]
    merger[, ownShare.Max := Shares.MaxCost.Sim / totalShare.Max * 100]
    merger[, ownRevShare.Max := Prices.MaxCost.Sim * Shares.MaxCost.Sim / totalRevShare.Max * 100]
    merger[, totalShare.Avg := sum(Shares.MeanCost.Sim), by = c("market_ids")]
    merger[, totalRevShare.Avg := sum(Shares.MeanCost.Sim * Prices.MaxCost.Sim), by = c("market_ids")]
    merger[, ownShare.Avg := Shares.MeanCost.Sim / totalShare.Avg * 100]
    merger[, ownRevShare.Avg := Prices.MeanCost.Sim * Shares.MeanCost.Sim / totalRevShare.Avg * 100]
    merger[, totalShare.Min := sum(Shares.MinCost.Sim), by = c("market_ids")]
    merger[, totalRevShare.Min := sum(Shares.MinCost.Sim * Prices.MaxCost.Sim), by = c("market_ids")]
    merger[, ownShare.Min := Shares.MinCost.Sim / totalShare.Min * 100]
    merger[, ownRevShare.Min := Prices.MinCost.Sim * Shares.MinCost.Sim / totalRevShare.Min * 100]
    
    merger <- merger %>% filter(market_ids %in% shared_markets) %>% group_by(market_ids) %>%
      summarize(HHI.Max = sum(ownShare.Max^2),
                HHI.Rev.Max = sum(ownRevShare.Max^2),
                HHI.Avg = sum(ownShare.Avg^2),
                HHI.Rev.Avg = sum(ownRevShare.Avg^2),
                HHI.Min = sum(ownShare.Min^2),
                HHI.Rev.Min = sum(ownRevShare.Min^2))
    
    data <- as.data.table(merge(merger, observed, by = "market_ids"))
    print(paste("Number of Markets HHI < 1800: ", nrow(data[HHI.Share < 1800,])))
    print(paste("Number of Markets HHI, Revenue < 1800:", nrow(data[HHI.Rev < 1800,])))
    
    data[, HHI.Max.Change := HHI.Max - HHI.Share]
    data[, HHI.Avg.Change := HHI.Avg - HHI.Share]
    data[, HHI.Min.Change := HHI.Min - HHI.Share]
    data[, HHI.Rev.Max.Change := HHI.Rev.Max - HHI.Rev]
    data[, HHI.Rev.Avg.Change := HHI.Rev.Avg - HHI.Rev]
    data[, HHI.Rev.Min.Change := HHI.Rev.Min - HHI.Rev]
    data <- data[, .(market_ids, HHI.Min.Change, HHI.Avg.Change, HHI.Max.Change,
                     HHI.Rev.Min.Change, HHI.Rev.Avg.Change, HHI.Rev.Max.Change)]
    
    data[, Bucket.HHI.Min := cut(HHI.Min.Change,
                                 breaks = c(-Inf, 0, 100, 1000, 3000, Inf),
                                 labels = c("$<$ 0", "0 - 100", "100 - 1000",
                                            "1000 - 3000", "3000 $<$"))]
    data[, Bucket.HHI.Rev.Min := cut(HHI.Rev.Min.Change,
                                     breaks = c(-Inf, 0, 100, 1000, 3000, Inf),
                                     labels = c("$<$ 0", "0 - 100", "100 - 1000",
                                                "1000 - 3000", "3000 $<$"))]
    data[, Bucket.HHI.Mean := cut(HHI.Avg.Change,
                                  breaks = c(-Inf, 0, 100, 1000, 3000, Inf),
                                  labels = c("$<$ 0", "0 - 100", "100 - 1000",
                                             "1000 - 3000", "3000 $<$"))]
    data[, Bucket.HHI.Rev.Mean := cut(HHI.Rev.Avg.Change,
                                      breaks = c(-Inf, 0, 100, 1000, 3000, Inf),
                                      labels = c("$<$ 0", "0 - 100", "100 - 1000",
                                                 "1000 - 3000", "3000 $<$"))]
    data[, Bucket.HHI.Max := cut(HHI.Max.Change,
                                 breaks = c(-Inf, 0, 100, 1000, 3000, Inf),
                                 labels = c("$<$ 0", "0 - 100", "100 - 1000",
                                            "1000 - 3000", "3000 $<$"))]
    data[, Bucket.HHI.Rev.Max := cut(HHI.Rev.Max.Change,
                                     breaks = c(-Inf, 0, 100, 1000, 3000, Inf),
                                     labels = c("$<$ 0", "0 - 100", "100 - 1000",
                                                "1000 - 3000", "3000 $<$"))]
    
    hhiCount.Min <- data[, .N, by = c("Bucket.HHI.Min")]; colnames(hhiCount.Min) <- c("Bucket", "Min")
    hhiCount.Mean <- data[, .N, by = c("Bucket.HHI.Mean")]; colnames(hhiCount.Mean) <- c("Bucket", "Mean")
    hhiCount.Max <- data[, .N, by = c("Bucket.HHI.Max")]; colnames(hhiCount.Max) <- c("Bucket", "Max")
    hhiCount.Rev.Min <- data[, .N, by = c("Bucket.HHI.Rev.Min")]; colnames(hhiCount.Rev.Min) <- c("Bucket", "MinRev")
    hhiCount.Rev.Mean <- data[, .N, by = c("Bucket.HHI.Rev.Mean")]; colnames(hhiCount.Rev.Mean) <- c("Bucket", "MeanRev")
    hhiCount.Rev.Max <- data[, .N, by = c("Bucket.HHI.Rev.Max")]; colnames(hhiCount.Rev.Max) <- c("Bucket", "MaxRev")
    
    out <- merge(merge(merge(merge(merge(hhiCount.Min, hhiCount.Mean, by = "Bucket", all.x = TRUE, all.y = TRUE), 
                                   hhiCount.Max, by = "Bucket", all.x = TRUE, all.y = TRUE), 
                             hhiCount.Rev.Min, by = "Bucket", all.x = TRUE, all.y = TRUE),
                       hhiCount.Rev.Mean, by = "Bucket", all.x = TRUE, all.y = TRUE),
                 hhiCount.Rev.Max, by = "Bucket", all.x = TRUE, all.y = TRUE)
    out <- as.data.frame(out)
    for(i in 1:nrow(out)){
      for(j in 2:ncol(out)){
        if(is.na(out[i,j])){
          out[i,j] <- 0
        }
      }
    }
    
    return(out)
  }
  
  pre_table <- table_make(merger_pre, observed_pre)
  post_table <- table_make(merger_post, observed_post)
  
  out <- rbind(pre_table, post_table)
  
  out <- as.data.table(out)
  colnames <- c("", "Best", "Average", "Worst", "Best", "Average", "Worst")
  rownames(out) <- NULL;
  
  kbl(out,
      format = "latex", 
      escape = FALSE, booktabs = TRUE,
      col.names = colnames) %>%
    add_header_above(c(" ", "Passenger Shares" = 3, "Revenue Shares" = 3)) %>%
    pack_rows(group_label = "Pre-Pandemic", 1, 5) %>%
    pack_rows(group_label = "Post-Pandemic", 6, 10) %>%
    row_spec(row = 5, hline_after = TRUE) %>%
    save_kable(file = table_out)
  
}

merger_simulation_cs_change_percent <- function(merger_post = "03.Output/Adv_Merger_Sim_Data.rds",
                                        observed_post = "02.Intermediate/Product_Data.rds",
                                        merger_pre = "03.Output/PrePandemic_Adv_Merger_Sim_Data.rds",
                                        observed_pre = "02.Intermediate/prepandemic.rds",
                                        table_out = "06.Tables/Merger_CS_Percent_Change.tex"){
  output_rows <- function(merger_in, observed_in){
    merger <- readRDS(merger_in)
    observed <- readRDS(observed_in)
    
    # Identify markets which merger impacted
    impactedMarkets <- observed %>% group_by(market_ids) %>%
      summarize(Spirit = max(Spirit),
                JetBlue = max(JetBlue)) %>% 
      filter(Spirit == 1, JetBlue == 1); impactedMarkets <- impactedMarkets$market_ids
    
    # Restrict
    merger <- merger[market_ids %in% impactedMarkets,]
    
    # Calculate Change in Market Consumer Surplus
    merger <- merger %>% select(market_ids, CS_Best, CS_Avg, CS_Worst, ConsumerSurplus_Observed) %>%
      unique()
    
    merger[, CS_Best_Change := (CS_Best - ConsumerSurplus_Observed)/ConsumerSurplus_Observed * 100]
    merger[, CS_Average_Change := (CS_Avg - ConsumerSurplus_Observed)/ConsumerSurplus_Observed * 100]
    merger[, CS_Worst_Change := (CS_Worst - ConsumerSurplus_Observed)/ConsumerSurplus_Observed * 100]
    
    # Cluster 1: Consumer Surplus Change 
    cs.best <- five_statistic_row_make("Best Case", merger$CS_Best_Change)
    cs.avg <- five_statistic_row_make("Average Case", merger$CS_Average_Change)
    cs.worst <- five_statistic_row_make("Worst Case", merger$CS_Worst_Change)
    
    obs_row <- c("N Markets", nrow(merger), "", "", "", "")
    
    table <- rbind(cs.best, cs.avg, cs.worst, obs_row)
    rownames(table) <- NULL
    return(table)
  }
  
  pre_rows <- output_rows(merger_in = merger_pre, observed_pre)
  post_rows <- output_rows(merger_in = merger_post, observed_post)
  
  
  table <- rbind(pre_rows, post_rows); rownames(table) <- NULL
  title_row <- c("", "Mean", "(SD)", "Minimum", "Median", "Maximum")
  
  kbl(table,
      format = "latex", col.names = title_row,
      escape = TRUE, booktabs = TRUE) %>%
    pack_rows(group_label = "Pre-Pandemic", 1, 4) %>%
    pack_rows(group_label = "Post-Pandemic", 5, 8) %>%
    row_spec(row = 3, hline_after = TRUE) %>%
    row_spec(row = 4, hline_after = TRUE) %>%
    row_spec(row = 7, hline_after = TRUE) %>%
    save_kable(file = table_out)
}

merger_simulation_cs_change <- function(merger_post = "03.Output/Adv_Merger_Sim_Data.rds",
                                                observed_post = "02.Intermediate/Product_Data.rds",
                                                merger_pre = "03.Output/PrePandemic_Adv_Merger_Sim_Data.rds",
                                                observed_pre = "02.Intermediate/prepandemic.rds",
                                                table_out = "06.Tables/Merger_CS_Results.tex"){
  output_rows <- function(merger_in, observed_in, period_name = NULL){
    merger <- readRDS(merger_in)
    observed <- readRDS(observed_in)
    
    # Identify markets which merger impacted
    impactedMarkets <- observed %>% group_by(market_ids) %>%
      summarize(Spirit = max(Spirit),
                JetBlue = max(JetBlue)) %>% 
      filter(Spirit == 1, JetBlue == 1); impactedMarkets <- impactedMarkets$market_ids
    
    # Restrict
    merger <- merger[market_ids %in% impactedMarkets,]
    
    # Calculate Change in Market Consumer Surplus
    merger <- merger %>% select(market_ids, CS_Best, CS_Avg, CS_Worst, ConsumerSurplus_Observed) %>%
      unique()
    
    # Prices are in Hundreds of Dollars, This Adjusts to be in Single Dollars
    merger[, CS_Best_Change := (CS_Best - ConsumerSurplus_Observed) * 100]
    merger[, CS_Average_Change := (CS_Avg - ConsumerSurplus_Observed) * 100]
    merger[, CS_Worst_Change := (CS_Worst - ConsumerSurplus_Observed) * 100]
    
    # Cluster 1: Consumer Surplus Change 
    cs.best <- five_statistic_row_make("Best Case", merger$CS_Best_Change)
    cs.avg <- five_statistic_row_make("Average Case", merger$CS_Average_Change)
    cs.worst <- five_statistic_row_make("Worst Case", merger$CS_Worst_Change)
    
    obs_row <- c("N Markets", nrow(merger), "", "", "", "")
    
    if(!is.null(period_name)){
      # Report Change in Consumer Surplus Estimates
      current_cs <- sum(merger$ConsumerSurplus_Observed) * 100;
      best_cs <- sum(merger$CS_Best) * 100;
      avg_cs <- sum(merger$CS_Avg) * 100;
      worst_cs <- sum(merger$CS_Worst) * 100
      
      print(paste("Current Period:", period_name));
      print(paste("Current Consumer Surplus (2017 USD):", current_cs))
      print(paste("Best Case Consumer Surplus (2017 USD):", best_cs))
      print(paste("Average Case Consumer Surplus (2017 USD):", avg_cs))
      print(paste("Worst Case Consumer Surplus (2017 USD):", worst_cs))
    }
    
    table <- rbind(cs.best, cs.avg, cs.worst, obs_row)
    rownames(table) <- NULL
    return(table)
  }
  
  pre_rows <- output_rows(merger_in = merger_pre, observed_pre, period_name = "Pre-Pandemic")
  post_rows <- output_rows(merger_in = merger_post, observed_post, period_name = "Post-Pandemic")
  
  table <- rbind(pre_rows, post_rows); rownames(table) <- NULL
  title_row <- c("", "Mean", "(SD)", "Minimum", "Median", "Maximum")
  
  kbl(table,
      format = "latex", col.names = title_row,
      escape = TRUE, booktabs = TRUE) %>%
    pack_rows(group_label = "Pre-Pandemic", 1, 4) %>%
    pack_rows(group_label = "Post-Pandemic", 5, 8) %>%
    row_spec(row = 3, hline_after = TRUE) %>%
    row_spec(row = 4, hline_after = TRUE) %>%
    row_spec(row = 7, hline_after = TRUE) %>%
    save_kable(file = table_out)
}

# For routes with price increase > 40, how many times do they appear?
route_consistency_table <- function(merger_post = "03.Output/Adv_Merger_Sim_Data.rds",
                                    observed_post = "02.Intermediate/Product_Data.rds",
                                    merger_pre = "03.Output/PrePandemic_Adv_Merger_Sim_Data.rds",
                                    observed_pre = "02.Intermediate/prepandemic.rds",
                                    table_out = "06.Tables/Route_Consistency.tex"){
  frame_make <- function(real, merger){
    real <- readRDS(real); merger <- readRDS(merger)
    
    real[, Spirit_Prescence := max(Spirit_Prescence), by = c("Year", "Quarter", "Origin",
                                                             "Dest")]
    real[, JetBlue_Prescence := max(JetBlue_Prescence), by = c("Year", "Quarter", "Origin",
                                                               "Dest")]
    
    # Compute Costs
    shared_markets <- unique(real[Spirit_Prescence == 1 & JetBlue_Prescence == 1, market_ids])
    
    # For each market, identify its minimum price pre, post merger. 
    merger <- merger %>% filter(market_ids %in% shared_markets) %>% group_by(market_ids, Year, Quarter, Origin, Dest) %>%
      summarize(Best.Min = min(Prices.MinCost.Sim),
                Avg.Min = min(Prices.MeanCost.Sim),
                Worst.Min = min(Prices.MaxCost.Sim))
    
    real <- real %>% filter(market_ids %in% shared_markets) %>% group_by(market_ids) %>%
      summarize(Prices.Real = min(prices))
    
    out <- as.data.table(merge(real, merger, by = "market_ids"))
    out[, Best := round(100*(Best.Min - Prices.Real), digits = 2)]
    out[, Average := round(100*(Avg.Min - Prices.Real), digits = 2)]
    out[, Worst := round(100*(Worst.Min - Prices.Real), digits = 2)]
    
    return(out)
  }
  
  panel_make <- function(route_data){
    best_col <- route_data[Best > 40, .N, by = c("Origin", "Dest")]
    best_col[N > 5, N := 5]
    best_col <- best_col[, .N, by = "N"]
    colnames(best_col) <- c("Quarters", "Best")
    average_col <- route_data[Average > 40, .N, by = c("Origin", "Dest")]
    average_col[N > 5, N := 5]
    average_col <- average_col[, .N, by = "N"]
    colnames(average_col) <- c("Quarters", "Average")
    worst_col <- route_data[Worst > 40, .N, by = c("Origin", "Dest")]
    worst_col[N > 5, N := 5]
    worst_col <- worst_col[, .N, by = "N"]
    colnames(worst_col) <- c("Quarters", "Worst")
    
    out <- merge(best_col, average_col, by = "Quarters", all.x = TRUE, all.y = TRUE)
    out <- merge(out, worst_col, by = "Quarters", all.x = TRUE, all.y = TRUE)
    
    out[, Quarters := as.character(Quarters)]
    out[Quarters == 5, Quarters := "5+"]
    out[is.na(Best), Best := 0]
    out[is.na(Average), Average := 0]
    out[is.na(Worst), Worst := 0]
    
    return(out)
  }
  
  post <- panel_make(frame_make(observed_post, merger_post))
  pre <- panel_make(frame_make(observed_pre, merger_pre))
  
  out <- rbind(pre, post)
  colnames(out) <- c("Quarters", "Best Case", "Average Case", "Worst Case")
  
  kbl(out,
      format = "latex",
      escape = TRUE, booktabs = TRUE) %>%
    pack_rows(group_label = "Pre-Pandemic", 1, 5) %>%
    pack_rows(group_label = "Post-Pandemic", 6, 10) %>%
    save_kable(file = table_out)
}

merger_results_table_5_efficiency <- function(merger_post = "03.Output/Adv_Merger_Sim_Data.rds",
                                 observed_post = "02.Intermediate/Product_Data.rds",
                                 merger_pre = "03.Output/PrePandemic_Adv_Merger_Sim_Data.rds",
                                 observed_pre = "02.Intermediate/prepandemic.rds",
                                 table_out = "06.Tables/Merger_Results_5Percent_Efficient.tex"){
  output_rows <- function(merger_in, observed_in){
    merger <- readRDS(merger_in)
    observed <- readRDS(observed_in)
    
    # Identify markets which merger impacted
    impactedMarkets <- observed %>% group_by(market_ids) %>%
      summarize(Spirit = max(Spirit),
                JetBlue = max(JetBlue)) %>% 
      filter(Spirit == 1, JetBlue == 1); impactedMarkets <- impactedMarkets$market_ids
    
    # Restrict
    merger <- merger[market_ids %in% impactedMarkets,]
    observed <- observed[market_ids %in% impactedMarkets]
    
    # Cluster 1: Prices
    price.obs <- six_statistic_row_make("Observed", observed$prices)
    price.best <- six_statistic_row_make("Best Case", merger$Prices.MinCost.Sim.95)
    price.avg <- six_statistic_row_make("Average Case", merger$Prices.MeanCost.Sim.95)
    price.worst <- six_statistic_row_make("Worst Case", merger$Prices.MaxCost.Sim.95)
    
    # Summarize Mean Price Change in Each Market
    merger <- merger %>% group_by(market_ids) %>%
      summarize(MeanPrice.MaxCost = sum(Shares.WithinMarket.MaxCost.95 * Prices.MaxCost.Sim.95)/sum(Shares.WithinMarket.MaxCost.95),
                MeanPrice.MinCost = sum(Shares.WithinMarket.MinCost.95 * Prices.MinCost.Sim.95)/sum(Shares.WithinMarket.MinCost.95),
                MeanPrice.MeanCost = sum(Shares.WithinMarket.MeanCost.95 * Prices.MaxCost.Sim.95)/sum(Shares.WithinMarket.MeanCost.95))
    
    observed[, withinMarketShares := sum(shares), by = market_ids]
    observed <- observed %>% group_by(market_ids) %>%
      summarize(MeanPrice.Real = sum(withinMarketShares * prices) / sum(withinMarketShares))
    
    merger <- merge(merger, observed, by = "market_ids", all.x = TRUE)
    
    # Cluster 2: Market Mean Price
    meanPrice.obs <- six_statistic_row_make("Observed", merger$MeanPrice.Real)
    meanPrice.best <- six_statistic_row_make("Best Case", merger$MeanPrice.MinCost)
    meanPrice.avg <- six_statistic_row_make("Average Case", merger$MeanPrice.MeanCost)
    meanPrice.worst <- six_statistic_row_make("Worst Case", merger$MeanPrice.MaxCost)
    
    # Cluster 3: % Change in Market Mean Price
    change.best <- six_statistic_row_make("Best Case", (merger$MeanPrice.MinCost - merger$MeanPrice.Real)/merger$MeanPrice.Real * 100)
    change.avg <- six_statistic_row_make("Average Case", (merger$MeanPrice.MeanCost - merger$MeanPrice.Real)/merger$MeanPrice.Real * 100)
    change.worst <- six_statistic_row_make("Worst Case", (merger$MeanPrice.MaxCost - merger$MeanPrice.Real)/merger$MeanPrice.Real * 100)
    
    table <- rbind(price.obs, price.best, price.avg, price.worst,
                   meanPrice.obs, meanPrice.best, meanPrice.avg, meanPrice.worst,
                   change.best, change.avg, change.worst)
    rownames(table) <- NULL
    return(table)
  }
  
  post_pandemic <- output_rows(merger_in = merger_post,
                               observed_in = observed_post)
  pre_pandemic <- output_rows(merger_in = merger_pre,
                              observed_in = observed_pre)
  
  title_row <- c("", "N", "Mean", "(SD)", "Minimum", "Median", "Maximum")
  table <- rbind(pre_pandemic, post_pandemic)
  
  kbl(table,
      format = "latex", col.names = title_row,
      escape = TRUE, booktabs = TRUE) %>%
    pack_rows(group_label = "Pre-Pandemic", 1, 11) %>%
    pack_rows(group_label = "Product Prices (100s, 2017 USD)", 1, 4) %>%
    pack_rows(group_label = "Market Average Price", 5, 8) %>%
    pack_rows(group_label = "% Change Average Price", 9, 11) %>%
    pack_rows(group_label = "Post-Pandemic", 12, 22) %>%
    pack_rows(group_label = "Product Prices  (100s, 2017 USD)", 12, 15) %>%
    pack_rows(group_label = "Market Average Price", 16, 19) %>%
    pack_rows(group_label = "% Change Average Price", 20, 22) %>%
    row_spec(row = 11, hline_after = TRUE) %>%
    save_kable(file = table_out)
}

merger_results_table_10_efficiency <- function(merger_post = "03.Output/Adv_Merger_Sim_Data.rds",
                                              observed_post = "02.Intermediate/Product_Data.rds",
                                              merger_pre = "03.Output/PrePandemic_Adv_Merger_Sim_Data.rds",
                                              observed_pre = "02.Intermediate/prepandemic.rds",
                                              table_out = "06.Tables/Merger_Results_10Percent_Efficient.tex"){
  output_rows <- function(merger_in, observed_in){
    merger <- readRDS(merger_in)
    observed <- readRDS(observed_in)
    
    # Identify markets which merger impacted
    impactedMarkets <- observed %>% group_by(market_ids) %>%
      summarize(Spirit = max(Spirit),
                JetBlue = max(JetBlue)) %>% 
      filter(Spirit == 1, JetBlue == 1); impactedMarkets <- impactedMarkets$market_ids
    
    # Restrict
    merger <- merger[market_ids %in% impactedMarkets,]
    observed <- observed[market_ids %in% impactedMarkets]
    
    # Cluster 1: Prices
    price.obs <- six_statistic_row_make("Observed", observed$prices)
    price.best <- six_statistic_row_make("Best Case", merger$Prices.MinCost.Sim.90)
    price.avg <- six_statistic_row_make("Average Case", merger$Prices.MeanCost.Sim.90)
    price.worst <- six_statistic_row_make("Worst Case", merger$Prices.MaxCost.Sim.90)
    
    # Summarize Mean Price Change in Each Market
    merger <- merger %>% group_by(market_ids) %>%
      summarize(MeanPrice.MaxCost = sum(Shares.WithinMarket.MaxCost.90 * Prices.MaxCost.Sim.90)/sum(Shares.WithinMarket.MaxCost.90),
                MeanPrice.MinCost = sum(Shares.WithinMarket.MinCost.90 * Prices.MinCost.Sim.90)/sum(Shares.WithinMarket.MinCost.90),
                MeanPrice.MeanCost = sum(Shares.WithinMarket.MeanCost.90 * Prices.MaxCost.Sim.90)/sum(Shares.WithinMarket.MeanCost.90))
    
    observed[, withinMarketShares := sum(shares), by = market_ids]
    observed <- observed %>% group_by(market_ids) %>%
      summarize(MeanPrice.Real = sum(withinMarketShares * prices) / sum(withinMarketShares))
    
    merger <- merge(merger, observed, by = "market_ids", all.x = TRUE)
    
    # Cluster 2: Market Mean Price
    meanPrice.obs <- six_statistic_row_make("Observed", merger$MeanPrice.Real)
    meanPrice.best <- six_statistic_row_make("Best Case", merger$MeanPrice.MinCost)
    meanPrice.avg <- six_statistic_row_make("Average Case", merger$MeanPrice.MeanCost)
    meanPrice.worst <- six_statistic_row_make("Worst Case", merger$MeanPrice.MaxCost)
    
    # Cluster 3: % Change in Market Mean Price
    change.best <- six_statistic_row_make("Best Case", (merger$MeanPrice.MinCost - merger$MeanPrice.Real)/merger$MeanPrice.Real * 100)
    change.avg <- six_statistic_row_make("Average Case", (merger$MeanPrice.MeanCost - merger$MeanPrice.Real)/merger$MeanPrice.Real * 100)
    change.worst <- six_statistic_row_make("Worst Case", (merger$MeanPrice.MaxCost - merger$MeanPrice.Real)/merger$MeanPrice.Real * 100)
    
    table <- rbind(price.obs, price.best, price.avg, price.worst,
                   meanPrice.obs, meanPrice.best, meanPrice.avg, meanPrice.worst,
                   change.best, change.avg, change.worst)
    rownames(table) <- NULL
    return(table)
  }
  
  post_pandemic <- output_rows(merger_in = merger_post,
                               observed_in = observed_post)
  pre_pandemic <- output_rows(merger_in = merger_pre,
                              observed_in = observed_pre)
  
  title_row <- c("", "N", "Mean", "(SD)", "Minimum", "Median", "Maximum")
  table <- rbind(pre_pandemic, post_pandemic)
  
  kbl(table,
      format = "latex", col.names = title_row,
      escape = TRUE, booktabs = TRUE) %>%
    pack_rows(group_label = "Pre-Pandemic", 1, 11) %>%
    pack_rows(group_label = "Product Prices (100s, 2017 USD)", 1, 4) %>%
    pack_rows(group_label = "Market Average Price", 5, 8) %>%
    pack_rows(group_label = "% Change Average Price", 9, 11) %>%
    pack_rows(group_label = "Post-Pandemic", 12, 22) %>%
    pack_rows(group_label = "Product Prices  (100s, 2017 USD)", 12, 15) %>%
    pack_rows(group_label = "Market Average Price", 16, 19) %>%
    pack_rows(group_label = "% Change Average Price", 20, 22) %>%
    row_spec(row = 11, hline_after = TRUE) %>%
    save_kable(file = table_out)
}
