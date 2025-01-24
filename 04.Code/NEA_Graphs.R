# Generate Figures of Each Firms Average 
avg_mile_price_evolve_graph <- function(input = "02.Intermediate/Airport_Mile_Price.rds",
                                        output = "05.Figures/NEA_Graphs/NEA_Evolvement.pdf"){
  carriers = c("Delta Air Lines Inc.",
               "JetBlue Airways",
               "American Airlines Inc.",
               "Spirit Air Lines")
  
  price_data <- readRDS(input)
  price_data <- price_data[Carrier %in% carriers,]
  price_data[, Period := paste(substr(Year, start = 3, stop = 4), "Q", Quarter, sep = "")]
  
  # JFK
  price.jfk <- price_data[Origin == "JFK",]
  jfk.avg <- price.jfk
  jfk.avg$Carrier <- "Airport Average"
  jfk.avg$Avg_Rev_Per_Mile <- jfk.avg$City_Avg_Rev;
  jfk.avg <- unique(jfk.avg)
  price.jfk <- rbind(price.jfk, jfk.avg)
  
  # BOS
  price.bos <- price_data[Origin == "BOS",]
  bos.avg <- price.bos
  bos.avg$Carrier <- "Airport Average"
  bos.avg$Avg_Rev_Per_Mile <- bos.avg$City_Avg_Rev
  bos.avg <- unique(bos.avg)
  price.bos <- rbind(price.bos, bos.avg)
  
  # LGA
  price.lga <- price_data[Origin == "LGA",]
  lga.avg <- price.lga
  lga.avg$Carrier <- "Airport Average"
  lga.avg$Avg_Rev_Per_Mile <- lga.avg$City_Avg_Rev
  lga.avg <- unique(lga.avg)
  price.lga <- rbind(price.lga, lga.avg)
  
  # Newark - EWR
  price.ewr <- price_data[Origin == "EWR",]
  ewr.avg <- price.ewr
  ewr.avg$Carrier <- "Airport Average"
  ewr.avg$Avg_Rev_Per_Mile <- ewr.avg$City_Avg_Rev
  ewr.avg <- unique(ewr.avg)
  price.ewr <- rbind(price.ewr, ewr.avg)
  
  # Combine All into One
  price_data <- rbind(price.jfk,
                      price.bos,
                      price.lga,
                      price.ewr)
  
  price_data[Carrier == "American Airlines Inc.", Carrier := "American"]
  price_data[Carrier == "Delta Air Lines Inc.", Carrier := "Delta"]
  price_data[Carrier == "JetBlue Airways", Carrier := "JetBlue"]
  price_data[Carrier == "Spirit Air Lines", Carrier := "Spirit"]
  
  ggplot(price_data, aes(x = Period, y = Avg_Rev_Per_Mile,
                         color = Carrier, group = Carrier)) +
    geom_line() +
    theme(axis.text.x = element_text(angle = 90,
                                     vjust = 0.5, hjust = 1),
          panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    labs(y = "Average Revenue Per Mile") +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0, 0.62)) + 
    annotate(geom = "rect", xmin = "21Q1", xmax = "23Q3", ymin = 0, ymax = Inf,
             fill = "gray", alpha = 0.4) +
    facet_grid(rows = vars(Origin))
  
  ggsave(filename = output,
         units = "in", 
         width = 6, height = 7)
}

avg_mile_price_evolve_graph_onestop <- function(input = "02.Intermediate/DB1B_With_Controls.Rds",
                                                output = "05.Figures/NEA_Graphs/NEA_Evolvement_OneStop.pdf"){
  db1b <- data.table(readRDS(input))
  db1b <- db1b[MktCoupons == 2,]
  db1b <- db1b[!is.na(Avg.Fare),]
  
  db1b[, Rev_Per_Mile := Avg.Fare / MktMilesFlown]
  db1b[, Rev_Passengers := Rev_Per_Mile * Passengers.Product]
  db1b[, City_Airline_Passengers := sum(Passengers.Product), 
       by = c("Year", "Quarter", "Origin", "Carrier")]
  db1b[, City_Passengers := sum(Passengers.Product),
       by = c("Year", "Quarter", "Origin")]
  db1b[, Avg_Rev_Per_Mile := sum(Rev_Passengers) / City_Airline_Passengers,
       by = c("Year", "Quarter", "Origin", "Carrier")]
  db1b[, City_Avg_Rev := sum(Rev_Passengers) / City_Passengers,
       by = c("Year", "Quarter", "Origin")]
  
  
  db1b <- db1b %>% select(Year, Quarter, Origin, Origin.City,
                          Origin_MSA, Carrier, Avg_Rev_Per_Mile,
                          City_Avg_Rev) %>%
    unique() %>% as.data.table()
  
  
  carriers = c("Delta Air Lines Inc.",
               "JetBlue Airways",
               "American Airlines Inc.",
               "Spirit Air Lines")
  
  price_data <- db1b
  price_data <- price_data[Carrier %in% carriers,]
  price_data[, Period := paste(substr(Year, start = 3, stop = 4), "Q", Quarter, sep = "")]
  
  # JFK
  price.jfk <- price_data[Origin == "JFK",]
  jfk.avg <- price.jfk
  jfk.avg$Carrier <- "Airport Average"
  jfk.avg$Avg_Rev_Per_Mile <- jfk.avg$City_Avg_Rev;
  jfk.avg <- unique(jfk.avg)
  price.jfk <- rbind(price.jfk, jfk.avg)
  
  # BOS
  price.bos <- price_data[Origin == "BOS",]
  bos.avg <- price.bos
  bos.avg$Carrier <- "Airport Average"
  bos.avg$Avg_Rev_Per_Mile <- bos.avg$City_Avg_Rev
  bos.avg <- unique(bos.avg)
  price.bos <- rbind(price.bos, bos.avg)
  
  # LGA
  price.lga <- price_data[Origin == "LGA",]
  lga.avg <- price.lga
  lga.avg$Carrier <- "Airport Average"
  lga.avg$Avg_Rev_Per_Mile <- lga.avg$City_Avg_Rev
  lga.avg <- unique(lga.avg)
  price.lga <- rbind(price.lga, lga.avg)
  
  # Newark - EWR
  price.ewr <- price_data[Origin == "EWR",]
  ewr.avg <- price.ewr
  ewr.avg$Carrier <- "Airport Average"
  ewr.avg$Avg_Rev_Per_Mile <- ewr.avg$City_Avg_Rev
  ewr.avg <- unique(ewr.avg)
  price.ewr <- rbind(price.ewr, ewr.avg)
  
  # Combine All into One
  price_data <- rbind(price.jfk,
                      price.bos,
                      price.lga,
                      price.ewr)
  
  price_data[Carrier == "American Airlines Inc.", Carrier := "American"]
  price_data[Carrier == "Delta Air Lines Inc.", Carrier := "Delta"]
  price_data[Carrier == "JetBlue Airways", Carrier := "JetBlue"]
  price_data[Carrier == "Spirit Air Lines", Carrier := "Spirit"]
  
  ggplot(price_data, aes(x = Period, y = Avg_Rev_Per_Mile,
                         color = Carrier, group = Carrier)) +
    geom_line() +
    theme(axis.text.x = element_text(angle = 90,
                                     vjust = 0.5, hjust = 1),
          panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    labs(y = "Average Revenue Per Mile") +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0, 0.32)) + 
    annotate(geom = "rect", xmin = "21Q1", xmax = "23Q3", ymin = 0, ymax = Inf,
             fill = "gray", alpha = 0.4) +
    facet_grid(rows = vars(Origin))
  
  ggsave(filename = output,
         units = "in", 
         width = 6, height = 7)
}

nea_operating_graph <- function(input = "02.Intermediate/NEA_OPCarrier_Switch.Rds",
                                output = "05.Figures/NEA_Operating_Graph.pdf"){
  db1b <- readRDS(input)
  db1b <- db1b[MktCoupons == 1,]
  db1b <- db1b %>% select(Year, Quarter, Origin, Dest, OpCarrier) %>%
  unique() %>%  data.table(); gc();
  
  db1b[, Route_ID := paste(Year, Quarter, Origin, Dest)]
  
  db1b.jb <- db1b[OpCarrier == "B6",]
  db1b.aa <- db1b[OpCarrier == "AA",]
  
  db1b[, AA_Prescence := Route_ID %in% db1b.aa$Route_ID]
  db1b[, JB_Prescence := Route_ID %in% db1b.jb$Route_ID]
  
  db1b.summarized <- db1b %>% filter(OpCarrier %in% c("B6","AA"),
                                     Origin %in% c("EWR", "BOS", "JFK", "LGA"),
                                     AA_Prescence == TRUE & JB_Prescence == TRUE) %>%
    group_by(Year, Quarter, Origin) %>%
    summarize(Shared_Routes = n()) %>%
    as.data.table()

  # Need date for line graph
  db1b.summarized[, Month := ""]
  db1b.summarized[Quarter == 1, Month := "February"]
  db1b.summarized[Quarter == 2, Month := "May"]
  db1b.summarized[Quarter == 3, Month := "August"]
  db1b.summarized[Quarter == 4, Month := "October"]
  db1b.summarized[, Date := my(paste(Month, Year))]
  
  ggplot(db1b.summarized, aes(x = Date, y = Shared_Routes)) + 
    geom_line(linewidth = 1) +
    facet_wrap(~Origin, ncol = 2, nrow = 2) +
    theme(axis.text.x = element_text(angle = 90,
                                     vjust = 0.5, hjust = 1),
          panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    scale_color_manual(values = c("darkgrey", "black")) +
    labs(y = "Number of Shared Operating Routes") +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0, 35)) +
    geom_vline(xintercept = dmy("1 January 2021"))
  
  ggsave(output, units = "in", width = 7, height = 6)
}

nea_op_carrier_graphs <- function(input  = "02.Intermediate/NEA_OPCarrier_Switch.Rds",
                                 aa_b6_switch = "05.Figures/NEA_OPCarrier_Switch_Graph.pdf",
                                 aa_b6op = "05.Figures/NEA_AATk_B6Op.pdf",
                                 b6_aaop = "05.Figures/NEA_B6TK_AAOp.pdf",
                                 tk_groups = "05.Figures/NEA_TkGroups.pdf"){
  nea_data <- readRDS(input)
  
  # Now, Condense Data
  nea_data.switch <- nea_data %>% group_by(Year, Quarter, Origin, Dest, AA_B6, TkCarrier) %>%
    summarize(Passengers := sum(Passengers),
              NEA_Route := mean(NEA_Market)) %>% data.table()
  nea_data.switch[, Market_Passengers := sum(Passengers), by = c("Year", "Quarter", "Origin",
                                                        "Dest")]
  
  
  nea_data.switch[, Passengers.Period := sum(Passengers), by = c("Year", "Quarter")]
  nea_data.switch[, Switch_Passengers := sum(Passengers), by = c("Year", "Quarter",
                                                          "AA_B6")]
  nea_data.switch <- nea_data.switch[Year >= 2017,]
  nea_data.switch <- unique(nea_data.switch[AA_B6 == 1, .(Year, Quarter, Passengers.Period, Switch_Passengers)])
  
  full_range <- expand.grid(Year = 2017:2023, Quarter = 1:4)
  nea_data.switch <- merge(nea_data.switch, full_range, all.y = TRUE)
  nea_data.switch[is.na(Switch_Passengers), Switch_Passengers := 0]
  
  nea_data.switch[, Month := ""]
  nea_data.switch[Quarter == 1, Month := "February"]
  nea_data.switch[Quarter == 2, Month := "May"]
  nea_data.switch[Quarter == 3, Month := "August"]
  nea_data.switch[Quarter == 4, Month := "October"]
  nea_data.switch[, Date := my(paste(Month, Year))]
  
  nea_data.switch[Year == 2023 & Quarter == 4, Switch_Passengers := NA]
  
  ggplot(nea_data.switch, aes(x = Date, y = Switch_Passengers)) +
    geom_line() + 
    scale_y_continuous(expand = c(0,0),
                       limits = c(0, 45000)) + 
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    labs(y = "Passengers", x = "Time")
  
  ggsave(aa_b6_switch, units = "in", width = 7, height = 3)
  
  # Now, AA Tk Carriers, with B6 Op
  nea_data.aaB6 <- nea_data %>% filter(TkCarrier == "AA",
                                         JetBlue_Op == TRUE) %>%
    group_by(Year, Quarter, Origin, Dest) %>%
    summarize(Passengers := 10 * sum(Passengers),
              NEA_Route := mean(NEA_Market)) %>% data.table()
  
  nea_data.aaB6[, Split_Passengers := sum(Passengers), by = c("Year", "Quarter")]
  nea_data.aaB6 <- unique(nea_data.aaB6[, .(Year, Quarter, Split_Passengers)])
  nea_data.aaB6 <- merge(nea_data.aaB6, full_range, all.y = TRUE)
  nea_data.aaB6[is.na(Split_Passengers), Split_Passengers := 0]
  
  nea_data.aaB6[, Month := ""]
  nea_data.aaB6[Quarter == 1, Month := "February"]
  nea_data.aaB6[Quarter == 2, Month := "May"]
  nea_data.aaB6[Quarter == 3, Month := "August"]
  nea_data.aaB6[Quarter == 4, Month := "October"]
  nea_data.aaB6[, Date := my(paste(Month, Year))]
  nea_data.aaB6[Year == 2023 & Quarter == 4, Split_Passengers := NA]
  
  ggplot(nea_data.aaB6, aes(x = Date, y = Split_Passengers)) +
    geom_line() + 
    scale_y_continuous(expand = c(0,0),
                       limits = c(0, 350000),
                       labels = comma) + 
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    labs(y = "Passegners", x = "Time")
  ggsave(aa_b6op, units = "in", height = 3, width = 7)
  
  nea_data.B6AA <- nea_data %>% filter(TkCarrier == "B6",
                                       American_Op == TRUE) %>%
    group_by(Year, Quarter) %>%
    summarize(Passengers := 10 * sum(Passengers)) %>% data.table()
  
  nea_data.B6AA <- merge(nea_data.B6AA, full_range, all.y = TRUE)
  nea_data.B6AA[is.na(Passengers), Passengers := 0]
  
  nea_data.B6AA[, Month := ""]
  nea_data.B6AA[Quarter == 1, Month := "February"]
  nea_data.B6AA[Quarter == 2, Month := "May"]
  nea_data.B6AA[Quarter == 3, Month := "August"]
  nea_data.B6AA[Quarter == 4, Month := "October"]
  nea_data.B6AA[, Date := my(paste(Month, Year))]
  nea_data.B6AA[Year == 2023 & Quarter == 4, Passengers := NA]
  
  ggplot(nea_data.B6AA, aes(x = Date, y = Passengers)) +
    geom_line() + 
    scale_y_continuous(expand = c(0,0),
                       limits = c(0, 350000),
                       labels = comma) + 
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    labs(y = "Passegners", x = "Time")
  
  ggsave(b6_aaop, units = "in", width = 7, height = 3)
  
  switch_tk <- nea_data %>% filter(TkCarrierGroup %in% c("AA:B6", "B6:AA"),
                                   Year >= 2017) %>%
    group_by(Year, Quarter) %>%
    summarize(Passengers := 10 * sum(Passengers)) %>% data.table()
  
  switch_tk <- merge(switch_tk, full_range, all.y = TRUE)
  switch_tk[is.na(Passengers), Passengers := 0]
  
  switch_tk[, Month := ""]
  switch_tk[Quarter == 1, Month := "February"]
  switch_tk[Quarter == 2, Month := "May"]
  switch_tk[Quarter == 3, Month := "August"]
  switch_tk[Quarter == 4, Month := "October"]
  switch_tk[, Date := my(paste(Month, Year))]
  switch_tk[Year == 2023 & Quarter == 4, Passengers := NA]
  
  ggplot(switch_tk, aes(x = Date, y = Passengers)) +
    geom_line() + 
    scale_y_continuous(expand = c(0,0),
                       limits = c(0, 3500),
                       labels = comma) + 
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    labs(y = "Passegners", x = "Time")
  
  ggsave(tk_groups, units = "in", width = 7, height = 3)
  
  # Print 500,000 divided by total AA, B6 passengers in products in Quarter 2, 2022
  # Impacted by NEA
  # Number of Products in 2022, Q1 NEA
  NEAProd <- nea_data %>% filter(Year == 2022, Quarter == 2,
                                    TkCarrier %in% c("AA", "B6"),
                                 NEA_Market == TRUE)
  switchProd <- nea_data %>% filter(Year == 2022, Quarter == 2,
       (TkCarrier == "AA" & JetBlue_Op == TRUE) | (TkCarrier == "B6" & American_Op),
       NEA_Route == TRUE)
  print(paste("Percent of Switch Products", round(sum(switchProd$Passengers) / sum(NEAProd$Passengers) * 100,
                                                  digits = 2)))
}

nea_op_carrier_graph <- function(input = "02.Intermediate/NEA_OPCarrier_Switch.Rds",
                                 graph = "05.Figures/NEA_OperationsGraph.pdf"){
  nea_data <- readRDS(input)
  
  neaProd <- nea_data %>% mutate(Joint_Operate = pmin(JetBlue_Op, American_Op) * Passengers,
                                 AmericanTicket_JBOp = pmin(Carrier == "AA", JetBlue_Op) * Passengers,
                                 JBTicket_AAOp = pmin(Carrier == "B6", American_Op) * Passengers) %>%
    group_by(Year, Quarter) %>%
    summarize(Joint_Operate = sum(Joint_Operate),
              AmericanTicket_JBOp = sum(AmericanTicket_JBOp),
              JBTicket_AAOp = sum(JBTicket_AAOp)) %>% as.data.table()
  
  neaProd[Quarter == 1, Month := "February"]
  neaProd[Quarter == 2, Month := "May"]
  neaProd[Quarter == 3, Month := "August"]
  neaProd[Quarter == 4, Month := "October"]
  neaProd[, Date := my(paste(Month, Year))]
  
  neaProd <- neaProd[, .(Date, Joint_Operate, AmericanTicket_JBOp, JBTicket_AAOp)]
  neaProd.melt <- melt(neaProd, id.vars = "Date") %>% as.data.table()
  neaProd.melt[, variable := factor(x = variable,
         levels = c("Joint_Operate", "AmericanTicket_JBOp", "JBTicket_AAOp"),
         labels = c("Joint Itinerary", "AA Ticket, JB Operator", "JB Ticket, AA Operator"))]
  neaProd.melt <- neaProd.melt[Date > mdy("January 1, 2018"),]
  
  ggplot(data = neaProd.melt, aes(x = Date, y = value, linetype = variable)) +
    geom_line() +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0, 225000),
                       labels = comma) +
    geom_vline(xintercept = mdy("January 1, 2021")) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    labs(x = "Date", y = "Passengers")

  ggsave(filename = graph, units = "in", 
         height = 3, width = 7)
  
}
