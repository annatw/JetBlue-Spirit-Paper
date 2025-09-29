merger_panel_make <- function(input = "02.Intermediate/DB1B_With_Controls.Rds",
                              output = "02.Intermediate/Merger_Panel_Data.Rds"){
  db1b <- as.data.table(readRDS(input))
  
  # Remove Outlier Markets on Distance, Those from
  # Small Markets
  db1b <- db1b[!is.na(Origin.Population),]
  db1b <- db1b[!is.na(Destination.Population),]
  db1b <- db1b[MktMilesFlown < quantile(MktMilesFlown, probs = (0.99))]
  db1b <- db1b[, MktMiles.Min := min(MktMilesFlown), by = c("Origin", "Dest")]
  db1b <- db1b[MktMiles.Min > 150,]
  
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
  
  # Now, For each Market, Identify
  # 1. If JB/Spirit are in it
  # 2. Average Fare, Minimum Fare
  # 3. Number of Passengers
  db1b[, Route := paste(Origin, Dest)]
  db1b[, SP_Route := max(Carrier == "Spirit Air Lines"),
       by = c("Origin", "Dest")]
  db1b[, JB_Route := max(Carrier == "JetBlue Airways"),
       by = c("Origin", "Dest")]
  db1b[, Non_JB_SP_Route := (SP_Route == 0 & JB_Route == 0)]

  # Determine Which Routes had Consistent JB, Spirit Presence
  spirit_offerings <- unique(db1b[Carrier == "Spirit Air Lines", 
                                  .(Origin, Dest, Carrier,
                                    Year, Quarter)])
  spirit_serve_freq <- spirit_offerings[, .N, by = .(Origin, Dest)]
  spirit_consistent <- unique(spirit_serve_freq[N ==13, .(Origin, Dest)])
  spirit_consistent[, Route := paste(Origin, Dest)]
  
  jetblue_offerings <- unique(db1b[Carrier == "JetBlue Airways",
                                   .(Origin, Dest, Carrier, Year, Quarter)])
  jetblue_serve_freq <- jetblue_offerings[, .N, by = .(Origin, Dest)]
  jetblue_consistent <- unique(jetblue_serve_freq[N == 13, .(Origin, Dest)])
  jetblue_consistent[, Route := paste(Origin, Dest)]
  
  db1b[, Consistent_Spirit_Offering := Route %in% spirit_consistent$Route]
  db1b[, Consistent_JetBlue_Offering := Route %in% jetblue_consistent$Route]
  
  db1b[, SP_Market := max(Carrier == "Spirit Air Lines"),
       by = c("Origin", "Dest", "Year", "Quarter")]
  db1b[, JB_Market := max(Carrier == "JetBlue Airways"), 
       by = c("Origin", "Dest", "Year", "Quarter")]
 
  # Restrict to Three Types of Routes:
  # 1) Never JetBlue/Spirit
  # 2) Consistent Spirit
  # 3) Consistent JetBlue
  db1b[, keep := FALSE]
  db1b[Non_JB_SP_Route == TRUE, keep := TRUE]
  db1b[Consistent_Spirit_Offering == TRUE, keep := TRUE]
  db1b[Consistent_JetBlue_Offering == TRUE, keep := TRUE]
  
  db1b <- db1b[keep == TRUE,]
  
  # Restrict Each Carrier to 1 Product for Each Period
  db1b <- db1b %>% filter(!is.na(Avg.Fare)) %>%
    group_by(Carrier, Origin, Dest, Route, Year, Quarter, Period,
                            SP_Market, JB_Market, Market_HHI,
             Market_HHI.Lag,
                            National_Average_JF_Price, MktMiles.Min) %>%
    summarize(Avg.Fare = sum(Avg.Fare * Passengers.Product)/sum(Passengers.Product),
              Avg.Distance = sum(MktMilesFlown * Passengers.Product)/sum(Passengers.Product)) %>%
    mutate(Avg.Fare.Log = log(Avg.Fare),
           Treated = Period >= 0,
           Period_M8 = as.numeric(Period == -8),
           Period_M7 = as.numeric(Period == -7),
           Period_M6 = as.numeric(Period == -6),
           Period_M5 = as.numeric(Period == -5),
           Period_M4 = as.numeric(Period == -4),
           Period_M3 = as.numeric(Period == -3),
           Period_M2 = as.numeric(Period == -2),
           Period_M1 = as.numeric(Period == -1),
           Period_M0 = as.numeric(Period == 0),
           Period_P1 = as.numeric(Period == 1),
           Period_P2 = as.numeric(Period == 2),
           Period_P3 = as.numeric(Period == 3),
           Period_P4 = as.numeric(Period == 4),
           Accept = as.numeric(Period > 0),
           Period = factor(Period),
           SP_JB_Market = SP_Market * JB_Market,
           Treated_Market = max(SP_Market, JB_Market),
           Carrier_Route = paste(Route, Carrier),
           Time_Treat = factor(paste(Period, "Treat", SP_JB_Market)),
           Spirit = Carrier == "Spirit Air Lines",
           JetBlue = Carrier == "JetBlue Airways",
           Merging_Firm = Carrier %in% c("Spirit Air Lines", "JetBlue Airways"),
           OtherC = !Carrier %in% c("Spirit Air Lines", 
                                    "JetBlue Airways")) %>% as.data.table()

  saveRDS(db1b, output)
}

merger_anticipatory_analysis <- function(input = "02.Intermediate/Merger_Panel_Data.Rds",
                                         table_output = "06.Tables/Merger_Antipate_Regression.tex",
                                         figure_output = "05.Figures/Merger_Anticipate_Figure.pdf"){
  panel_data <- readRDS(input)
  panel_data[, Period := relevel(Period, ref = "-1")]

  # Differences in Differences
  regression_did <- lm_robust(formula = 
          Avg.Fare.Log ~ Accept + SP_JB_Market +
            Accept:SP_JB_Market, 
          fixed_effects = ~ Route + Carrier,
          data = panel_data, clusters = Route,
          se_type = "stata")
  summary(regression_did)
  
  # Event Study
  regression_dynamic <- lm_robust(formula = 
     Avg.Fare.Log ~ Period + 
       SP_JB_Market:Period_M8 + 
       SP_JB_Market:Period_M7 + SP_JB_Market:Period_M6 +  SP_JB_Market:Period_M5 + 
       SP_JB_Market:Period_M4 + SP_JB_Market:Period_M3 + SP_JB_Market:Period_M2 +
       SP_JB_Market:Period_M0 +
       SP_JB_Market:Period_P1 + SP_JB_Market:Period_P2 + SP_JB_Market:Period_P3 + 
       SP_JB_Market:Period_P4,
                           fixed_effects = ~ Route + Carrier,
                    data = panel_data, clusters = Route,
                    se_type = "stata")
  
  summary(regression_dynamic)
  
  regression_dynamic_result <- data.table(summary(regression_dynamic)$coefficients)
  
 regression_dynamic_result$row <- rownames(summary(regression_dynamic)$coefficients)
  
  keep <- grepl(regression_dynamic_result$row,
                pattern = "SP_JB_Market:")
  
  regression_dynamic_result <- regression_dynamic_result[keep,]
  
  regression_dynamic_result[, Period := as.numeric(substr(row, 22, 22))]
  
  regression_dynamic_result[grepl(x = row, pattern = "Period_M"), 
                            Period := Period * -1]
  regression_dynamic_result <- regression_dynamic_result[, c(1,2,8,9)]
  
  colnames(regression_dynamic_result) <- c("Estimate", "StdError", 
                                           "row", "Period")
  
  ggplot(regression_dynamic_result, aes(x = Period, y = Estimate)) + 
    geom_point() + 
    geom_errorbar(aes(ymin = Estimate-1.96*StdError, ymax = Estimate + 1.96 * StdError)) +
    geom_hline(aes(yintercept = 0),color = "grey", linetype = "dashed") +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    labs(x = "Period", y = "Estimated Coefficient")
  
  
  ggsave(figure_output, units = "in", width = 7, height = 3)
}
