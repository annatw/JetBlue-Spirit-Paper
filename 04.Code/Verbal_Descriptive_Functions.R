# Compute Share of JB, SP products that are within 150 miles
# Compute Share of JB, SP passengers within 150 miles
spirit_jetblue_near_markets_share <- function(input = "02.Intermediate/Construct_DB1B/DB1B_Clarified.Rds"){
  db1b <- readRDS(input)
  
  # Restrict to Sample Years
  db1b <- db1b[Year > 2016,]
  db1b <- db1b[Year != 2020,]
  db1b <- db1b[!(Year == 2021 & Quarter == 1),]
  
  # Restrict Carrier to JetBlue, Spirit
  db1b <- db1b[Carrier %in% c("JetBlue Airways",
                              "Spirit Air Lines"),]
  gc(); gc();
  
  db1b[, NonStop := nchar(AirportGroup) == 7]
  db1b[, MinDistance := min(MktMilesFlown), by = c("Origin",
                                                   "Dest")]
  db1b[, NearMarket := MinDistance < 150]
  
  
  # Product Count
  db1b.product <- unique(db1b[, .(Carrier, Origin, Dest,
                                  NonStop, Year, Quarter,
                                  MinDistance, NearMarket)])
  
  db1b.product.summary <- db1b.product %>%
    group_by(Carrier) %>%
    summarize(Share_Near = sum(NearMarket)/n())
  
  print(paste("JetBlue Min Distance Excluded Products:", db1b.product.summary[1,2]))
  print(paste("Spirit Min Distance Excluded Products:", db1b.product.summary[2,2]))
  
  # Passenger Count
  db1b.passenger <- db1b %>% group_by(Carrier) %>%
    summarize(Share_Near = sum(NearMarket * Passengers) / sum(Passengers))
  
  print(paste("JetBlue Min Distance Excluded Passengers:", db1b.passenger[1,2]))
  print(paste("Spirit Min Distance Excluded Passengers:", db1b.passenger[2,2]))
  
}

# Compute Share of all JB, SP Passengers, Products in Markets with under 500 Passengers
spirit_jetblue_small_markets_share <- function(input = "02.Intermediate/Construct_DB1B/DB1B_Clarified.Rds"){
  db1b <- readRDS(input)
  
  # Restrict to Sample Years
  db1b <- db1b[Year > 2016,]
  db1b <- db1b[Year != 2020,]
  db1b <- db1b[!(Year == 2021 & Quarter == 1),]
  
  
  db1b.small_market <- db1b %>% group_by(Year, Quarter, Origin, Dest) %>%
    summarize(Passengers = sum(Passengers)) %>% 
    mutate(Market = paste(Year, Quarter, Origin, Dest)) %>%
    filter(Passengers < 50) %>%
    as.data.table()
  
  # Flag Markets That Are Small
  db1b[, Small_Market := paste(Year, Quarter, Origin, Dest) %in% db1b.small_market$Market,]
  
  db1b <- db1b[Carrier %in% c("JetBlue Airways",
                              "Spirit Air Lines"),]
  
  # Identify Number of Products in Small Markets
  db1b.product <- unique(db1b[, .(Year, Quarter, Origin, Dest, Carrier, Small_Market)])
  db1b.product.summary <- db1b.product %>%
    group_by(Carrier) %>%
    summarize(Small_Share = sum(Small_Market)/n())
  
  
  # Identify Number of Passengers in Small Markets
  db1b.passenger.summary <- db1b %>% group_by(Carrier) %>%
    summarize(Small_Share = sum(Small_Market * Passengers)/sum(Passengers))
  
}
