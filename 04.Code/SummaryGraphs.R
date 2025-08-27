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
