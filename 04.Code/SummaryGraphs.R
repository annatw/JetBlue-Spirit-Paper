lcc_market_graph <- function(post_pandemic_in = "02.Intermediate/Product_Data.rds",
                             pre_pandemic_in = "02.Intermediate/prepandemic.rds",
                   output = "05.Figures/LCC_Market_Graph.pdf"){
 
  product_data <- rbind(readRDS(post_pandemic_in), readRDS(pre_pandemic_in),
                        fill = TRUE)

  product_data <- product_data %>%
    group_by(Year, Quarter, Origin, Dest) %>%
    summarize(Allegiant = max(Carrier == "Allegiant Air"),
              Frontier = max(Carrier == "Frontier Airlines Inc."),
              JetBlue = max(Carrier == "JetBlue Airways"),
              Southwest = max(Carrier == "Southwest Airlines Co."),
              Spirit = max(Carrier == "Spirit Air Lines")) %>%
    mutate(LCC_Market = Allegiant + Frontier + JetBlue +
                              Southwest + Spirit) %>%
    as.data.table()
  
  product_data[, Period := "Post-Pandemic"]
  product_data[Year < 2020, Period := "Pre-Pandemic"]
  
  ggplot(product_data, aes(x = LCC_Market)) +
    geom_bar() + 
    scale_y_continuous(expand = c(0,0),
                       labels = comma,
                       limits = c(0, 90000)) +
    # facet_grid(~Period) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    labs(x = "Low-Cost & Ultra-Low Cost Carriers in Market",
         y = "Markets")
  
  ggsave(output, units = "in", width = 7, height = 3)
}

airfare_trend_graph <- function(input = "02.Intermediate/Construct_DB1B/DB1B_Condensed.rds",
                                output_graph = "05.Figures/Fare_Per_Mile_Graph.tex"){
  db1b_data <- readRDS(input)
  
  db1b_data[, Month := 0]
  db1b_data[Quarter == 1, Month := 2]
  db1b_data[Quarter == 2, Month := 5]
  db1b_data[Quarter == 3, Month := 8]
  db1b_data[Quarter == 4, Month := 11]
  db1b_data[, Date := my(paste(Month, Year))]
  
  year_quarter_evolution <- db1b_data %>%
    group_by(Date) %>%
    summarize(Fare_Per_Mile = sum(Avg.Fare / MktMilesFlown * Passengers.Product) / sum(Passengers.Product))
  
  ggplot(year_quarter_evolution, aes(x = Date, y = Fare_Per_Mile)) +
    geom_line() + theme(panel.background = element_blank(), 
        axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "bottom") +
    labs(x = "Date", y = "Fare Per Mile (Nominal Prices)")
    
  ggsave(output_graph, units = "in", width = 7, height = 3)
}

