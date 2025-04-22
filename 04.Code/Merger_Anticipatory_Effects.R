merger_panel_make <- function(input = "02.Intermediate/Construct_DB1B/DB1B_Condensed.rds",
                              output = "02.Intermediate/Merger_Panel_Data.Rds"){
  db1b <- as.data.table(readRDS(input))
  
  # Remove Outlier Markets on Distance, Those from
  # Small Markets
  db1b <- db1b[!is.na(Origin.Population),]
  db1b <- db1b[!is.na(Destination.Population),]
  db1b <- db1b[MktMilesFlown < quantile(MktMilesFlown, probs = (0.99))]
  db1b <- db1b[, MktMiles.Min := min(MktMilesFlown), by = c("Origin", "Dest")]
  db1b <- db1b[MktMiles.Min > 150,]; db1b[,MktMiles.Min := NULL]
  
  # Now, For each Market, Identify
  # 1. If JB/Spirit are in it
  # 2. Average Fare, Minimum Fare
  # 3. Number of Passengers
  
  db1b[, SP_Market := max(Carrier == "Spirit Air Lines"),
       by = c("Origin", "Dest", "Year", "Quarter")]
  db1b[, JB_Market := max(Carrier == "JetBlue Airways"), 
       by = c("Origin", "Dest", "Year", "Quarter")]
 
  # Identify Period
  db1b[, Period := -99]
  db1b[Year == 2022 & Quarter == 1, Period := -1]
  db1b[Year == 2022 & Quarter > 1, Period := Quarter - 2]
  db1b[Year == 2023 & Quarter < 3, Period := Quarter + 2]
  # 21Q4 = -2, 21Q3 = -3, 21Q2 = -4
  db1b[Year == 2021 & Quarter >= 2, Period := Quarter - 6]
  db1b[Year == 2019, Period := Quarter - 9]
  # Remove Excluded Periods
  db1b <- db1b[Period > -99]
  
  # Restrict Each Carrier to 1 Product for Each Period
  db1b <- db1b %>% filter(!is.na(Avg.Fare)) %>%
    group_by(Carrier, Origin, Dest, Year, Quarter, Period,
                            SP_Market, JB_Market, Market_HHI,
                            National_Average_JF_Price) %>%
    summarize(Avg.Fare = sum(Avg.Fare * Passengers.Product)/sum(Passengers.Product)) %>%
    mutate(Avg.Fare.Log = log(Avg.Fare),
           Period = factor(Period,
                           levels = -8:4),
           SP_JB_Market = SP_Market * JB_Market,
           Route = paste(Origin, Dest),
           Spirit = Carrier == "Spirit Air Lines",
           JetBlue = Carrier == "JetBlue Airways",
           OtherC = !Carrier %in% c("Spirit Air Lines", 
                                    "JetBlue Airways")) %>% as.data.table()

  saveRDS(db1b, output)
}

merger_anticipatory_analysis <- function(input = "02.Intermediate/Merger_Panel_Data.Rds"){
  panel_data <- readRDS(input)
  
  
  # Use lm_robust to absorb large number of route fixed effects
  regression1 <- lm_robust(formula = 
                             Avg.Fare.Log ~ 
                             Period:SP_JB_Market,
                           fixed_effects = ~ Route:Carrier + Period,
                    data = panel_data,
                    se_type = "HC1")
  summary(regression1)
}
