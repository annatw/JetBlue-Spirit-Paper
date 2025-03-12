minimum_fare_change_inspect <- function(merger_data = "03.Output/Adv_Merger_Sim_Data.rds",
                                     observed_data = "02.Intermediate/Product_Data.rds",
                                     table_out = "02.Intermediate/Minimum_Fare_Change_Table.rds"){
  merger <- readRDS(merger_data)
  observed <- readRDS(observed_data)
  
  observed[, Spirit_Prescence := max(Spirit_Prescence), by = c("Year", "Quarter", "Origin",
                                                               "Dest")]
  observed[, JetBlue_Prescence := max(JetBlue_Prescence), by = c("Year", "Quarter", "Origin",
                                                                 "Dest")]
  # Compute Minimum Fares
  shared_markets <- unique(observed[Spirit_Prescence == 1 & JetBlue_Prescence == 1, market_ids])
  
  merger <- merger %>% filter(market_ids %in% shared_markets) %>%
    group_by(market_ids) %>%
    summarize(Prices.MinCost = min(Prices.MinCost.Sim) * 100,
              Prices.MeanCost = min(Prices.MeanCost.Sim) * 100,
              Prices.MaxCost = min(Prices.MaxCost.Sim)* 100) %>% as.data.table()
  
  observed <- observed %>% filter(market_ids %in% shared_markets) %>%
    group_by(market_ids) %>%
    summarize(MinPrice = min(prices)* 100) %>% as.data.table()
  
  result <- merge(merger, observed, by = "market_ids")
  result[, `Best Case Scenario` := Prices.MinCost - MinPrice]
  result[, `Average Case Scenario` := Prices.MeanCost - MinPrice]
  result[, `Worst Case Scenario` := Prices.MaxCost - MinPrice]
  result.dollar <- result[, .(market_ids, `Best Case Scenario`, `Average Case Scenario` , `Worst Case Scenario`)] %>% unique()
  
  result.per <- result[, .(market_ids, `Best Case %`, `Average Case %` , `Worst Case %`)] %>% unique()
  
  result.melt <- melt(result.dollar, id.vars = c("market_ids")) %>% data.table()
  colnames(result.melt) <- c("Market", "Simulation", "Change")
  
}
