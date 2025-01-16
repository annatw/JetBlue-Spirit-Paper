merger_results_table <- function(merger_data = "03.Output/Adv_Merger_Sim_Data.rds",
                                 observed_data = "02.Intermediate/Product_Data.rds", 
                                 table_out = "06.Tables/Merger_Results.tex"){
  merger <- readRDS(merger_data)
  observed <- readRDS(observed_data)
  
  # Identify markets which merger impacted
  impactedMarkets <- observed %>% group_by(market_ids) %>%
    summarize(Spirit = max(Spirit),
              JetBlue = max(JetBlue)) %>% 
    filter(Spirit == 1, JetBlue == 1); impactedMarkets <- impactedMarkets$market_ids
  
  # Restrict
  merger <- merger[market_ids %in% impactedMarkets,]
  
  # Cluster 1: Prices 
  price.best <- five_statistic_row_make("Best Case", merger$Prices.MinCost.Sim,
                                        weight_variable = merger$Potential_Passengers *
                                          merger$Shares.WithinMarket.MinCost)
  price.avg <- five_statistic_row_make("Average Case", merger$Prices.MeanCost.Sim,
                                       weight_variable = merger$Potential_Passengers *
                                         merger$Shares.WithinMarket.MinCost)
  price.worst <- five_statistic_row_make("Worst Case", merger$Prices.MaxCost.Sim,
                                         weight_variable = merger$Potential_Passengers *
                                           merger$Shares.WithinMarket.MinCost)
  
  # Cluster 2: Price Change
  priceChange.best <- five_statistic_row_make("Best Case", merger$Prices.MinCost.Sim - merger$Fare.Original,
                                              weight_variable = merger$Potential_Passengers *
                                                merger$Shares.WithinMarket.MinCost)
  priceChange.avg <- five_statistic_row_make("Average Case", merger$Prices.MeanCost.Sim - merger$Fare.Original,
                                             weight_variable = merger$Potential_Passengers *
                                               merger$Shares.WithinMarket.MinCost)
  priceChange.worst <- five_statistic_row_make("Worst Case", merger$Prices.MaxCost.Sim - merger$Fare.Original,
                                               weight_variable = merger$Potential_Passengers *
                                                 merger$Shares.WithinMarket.MinCost)
  
  table <- rbind(price.best, price.avg, price.worst,
                 priceChange.best, priceChange.avg, priceChange.worst)
  rownames(table) <- NULL
  
  title_row <- c("", "Mean", "(SD)", "Minimum", "Median", "Maximum")
  
  kbl(table,
      format = "latex", col.names = title_row,
      escape = TRUE, booktabs = TRUE) %>%
    pack_rows(group_label = "Prices", 1, 3) %>%
    pack_rows(group_label = "Price Change", 4, 6) %>%
    save_kable(file = table_out)
  
  # pack_rows(group_label = "Within Market Shares", 7, 9) %>%
  # pack_rows(group_label = "Within Market Shares Change", 10, 12) %>%
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
                                        table_out = "06.Tables/MinimumFareChange.tex"){
  period_table_make <- function(real, merger){
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
    
    out[, bucket.Best := cut(Best,
                             breaks = c(-Inf, 20, 40, 60, 80, Inf),
                             labels = c("$<$ 20", "20-40", "40-60",
                                        "60-80", "80 $<$"))]
    out[, bucket.Average := cut(Average,
                             breaks = c(-Inf, 20, 40, 60, 80, Inf),
                             labels = c("$<$ 20", "20-40", "40-60",
                                        "60-80", "80 $<$"))]
    out[, bucket.Worst := cut(Worst,
                             breaks = c(-Inf, 20, 40, 60, 80, Inf),
                             labels = c("$<$ 20", "20-40", "40-60",
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
      col.names = colnames) %>%
    add_header_above(c(" ", "Pre-Pandemic" = 3, "Post-Pandemic" = 3)) %>%
    save_kable(file = table_out)
}

merger_simulation_hhi_change <- function(merger_data = "03.Output/Adv_Merger_Sim_Data.rds",
                                         observed_data = "02.Intermediate/Product_Data.rds", 
                                         table_out = "06.Tables/Merger_HHI_Results.tex"){
  observed <- readRDS(observed_data)
  
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
  
  merger <- readRDS(merger_data)
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
  out <- as.data.table(out)
  colnames <- c("", "Best", "Average", "Worst", "Best", "Average", "Worst")
  rownames(out) <- NULL;
  
  kbl(out,
      format = "latex", 
      escape = FALSE, booktabs = TRUE,
      col.names = colnames) %>%
    add_header_above(c(" ", "Passenger Shares" = 3, "Revenue Shares" = 3)) %>%
    save_kable(file = table_out)
  
}
