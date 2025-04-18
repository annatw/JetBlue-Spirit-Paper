crude_oil_graph <- function(input_raw = "01.Input/13.Oil_Spot_Data/Jet_Fuel_Spot.csv",
                            input_cleaned = "02.Intermediate/jet_fuel.rds",
                            output_raw = "05.Figures/JetFuelGraph_Raw.pdf",
                            output_clean = "05.Figures/JetFuelGraph_Raw.pdf"){
  jetfuel_raw <- fread(input_raw)
  jetfuel_clean <- readRDS(input_cleaned)
  
  colnames(jetfuel_raw) <- c("Date", "Nominal_Price")
  jetfuel_raw$Date <- mdy(jetfuel_raw$Date)
  jetfuel_raw <- jetfuel_raw[Date >= mdy("January 1, 1990")]
  
  jetfuel_clean[, Month := 0]
  jetfuel_clean[Quarter == 1, Month := 2]
  jetfuel_clean[Quarter == 2, Month := 5]
  jetfuel_clean[Quarter == 3, Month := 8]
  jetfuel_clean[Quarter == 4, Month := 11]
  jetfuel_clean[, Date := my(paste(Month, Year))]
  jetfuel_clean <- jetfuel_clean[, .(Date, Jet_Fuel_Price)]
  colnames(jetfuel_clean) <- c("Date", "Real_Price")
  
  ggplot(jetfuel_raw, aes(x = Date, y = Nominal_Price)) + 
    geom_line()
  ggsave(output_raw, units = "in", width = 5, height = 3)
  
  ggplot(jetfuel_clean, aes(x = Date, y = Real_Price)) +
    geom_line()
  ggsave(output_clean, units = "in", width = 5, height = 3)
}

oil_compare_graph <- function(firm_in = "02.Intermediate/JetFuel_Reports.rds",
                              graph_out = "05.Figures/Fuel_Price_Compare.pdf"){
  firm <- readRDS(firm_in)
  firm[, Price_Less_National := Jet_Fuel_Price - National_Average_JF_Price]
  
  firm[, Month := 0]
  firm[Quarter == 1, Month := 2]
  firm[Quarter == 2, Month := 5]
  firm[Quarter == 3, Month := 8]
  firm[Quarter == 4, Month := 11]
  firm[, Date := my(paste(Month, Year))]
  
  firm$Carrier <- factor(x = firm$Carrier, levels = c("Alaska Airlines Inc.",
            "Allegiant Air", "American Airlines Inc.", "Delta Air Lines Inc.",
            "Frontier Airlines Inc.", "JetBlue Airways", "Southwest Airlines Co.",
            "Spirit Air Lines", "United Air Lines Inc."),
            labels = c("Alaska", "Allegiant", "American", "Delta", "Frontier", 
                       "JetBlue", "Southwest", "Spirit", "United"))
  
  ggplot(data = firm, mapping = aes(x = Date, y = Price_Less_National,
                                    color = Carrier)) + 
    geom_line() +     
    scale_y_continuous(expand = c(0,0),
                       limits = c(-1, 2.25)) + 
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    labs(y = "Firm Price - National Spot")
    
  ggsave(graph_out, units = "in", height = 3.5, width = 6)
}

lcc_in_market_graph <- function(data_in = "02.Intermediate/DB1B_With_Controls.Rds",
                                graph_out = "05.Figures/LCC_Density.pdf"){
  market_data <- readRDS(data_in)

  market_data <- market_data %>% filter(Year %in% c(2018, 2019, 2022, 2023)) %>%
    group_by(Year, Quarter, Origin, Dest) %>%
    summarize(Alaska_Prescence = max(Alaska_Prescence),
              Frontier_Prescence = max(Frontier_Prescence),
              Hawaiian_Prescence = max(Hawaiian_Prescence),
              Allegiant_Prescence = max(Allegiant_Prescence),
              Spirit_Prescence = max(Spirit_Prescence),
              JetBlue_Prescence = max(JetBlue_Prescence),
              Southwest_Prescence = max(Southwest_Prescence)) %>%
    mutate(LCC_Count = Alaska_Prescence + Frontier_Prescence + Hawaiian_Prescence +
                             Allegiant_Prescence + Spirit_Prescence + 
                             JetBlue_Prescence + Southwest_Prescence)
  
  ggplot(market_data, aes(x = LCC_Count)) + 
    geom_histogram(binwidth = 1) +
    facet_grid(rows = vars(Year)) +
  theme(panel.background = element_blank(), 
        axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "bottom") +
    scale_y_continuous(expand = c(0,0)) + 
    labs(y = "Number of Markets", x = "Number of LCC, ULCC in Market")
    
  ggsave(graph_out, units = "in", height = 7, width = 5)
}

