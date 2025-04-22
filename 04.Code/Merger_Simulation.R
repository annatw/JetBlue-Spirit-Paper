merger_sim_data_generate <- function(input_file = "02.Intermediate/DB1B_With_Controls.Rds",
                                     output_file = "02.Intermediate/Product_Data.rds",
                                     years_allowed = 2021:2023,
                                     fast = FALSE,
                                     market_group=c("Year", "Quarter", "Origin", "Origin_MSA", "Dest", "Destination_MSA"),
                                     product_group = c("Year", "Quarter", "Origin", "Origin_MSA", 
                                                        "Dest", "Destination_MSA", "Carrier", "NonStop")){
  # First, Generate Product Data
  product_data <- as.data.table(readRDS(input_file))
  
  # Drop Products with Severe Outliers for Distance
  product_data <- product_data[MktMilesFlown < quantile(MktMilesFlown, probs = (0.99))]
  
  # Restrict to sample period
  product_data <- product_data[Year %in% years_allowed, ]; gc();

  if(2021 %in% years_allowed){
    product_data <- product_data[!(Year == 2021 & Quarter < 2),]
  }
  
  # Add Numeric ID for market to help PyBLP
  if(identical(market_group,c("Year", "Quarter", "Origin", "Origin_MSA", "Dest", "Destination_MSA"))){
    product_data[, Market := paste(Year, Quarter, Origin, Dest)]
  } else if (identical(market_group, c("Year", "Quarter", "Origin.City", "Destination.City"))){
    product_data[, Market := paste(Year, Quarter, Origin.City, Destination.City)]
  } else{
    return(-1)
  }
  product_data[, Market := factor(Market)]
  product_data[, Market_ID := as.numeric(Market)]
  
  # Remove All Airports out of the top 100 in Q2 2022
  product_data[, Is.Small := is.na(Origin.Population) | is.na(Destination.Population)]
  small <- unique(product_data[, .(Market_ID, Is.Small, Passengers.Inside.Market)])
  numerator <- nrow(small[Is.Small == TRUE,])
  denominator <- nrow(small) 
  print(paste("Small Markets:", numerator / denominator * 100, " Of Sample"))
  num.passengers <- sum(small[Is.Small == TRUE, Passengers.Inside.Market])
  den.passengers <- sum(small[, Passengers.Inside.Market])
  print(paste("Small Markets Ridership Share:", num.passengers / den.passengers * 100, 
              "Of Sample"))
  
  product_data <- product_data[!is.na(Origin.Population),]
  product_data <- product_data[!is.na(Destination.Population),]
  gc();
  
  
  # Drop Hawaiian, Alaskan Markets
  hi_ak <- c("Kahului-Wailuku-Lahaina, HI", "San Juan-Carolina-Caguas, PR", "Kapaa, HI Mic", "Hilo, HI Mic",
             "Urban Honolulu, HI", "Anchorage, AK")
  product_data <- product_data[!(Origin_MSA %in% hi_ak),]
  product_data <- product_data[!(Destination_MSA %in% hi_ak),]
  gc();
  
  
  product_data[, Potential_Passengers := sqrt(Origin.Population) * sqrt(Destination.Population)]
  product_data[, Share := Passengers.Product / Potential_Passengers]
  
  product_data[, Period := Year + 0.1 * Quarter]
  
  product_data[, Product_Name := paste(Origin, Dest, Carrier, MktMilesFlown, NonStop)]
  product_data[, Product_Name := factor(Product_Name)]
  
  gc(); 
  
  # Remove All Products Previously Completely Excluded from the Sample
  product_data <- product_data[Avg.Fare > 0,]
  product_data <- product_data[!is.na(Avg.Fare),]
  
  product_data[, NonStop := as.numeric(NonStop)]
  
  # Calculate Extra Miles over Minimum in the Market
  product_data[, market_min_miles := min(MktMilesFlown) / 1000, by = market_group]
  product_data[, Extra_Miles := (MktMilesFlown) / 1000 - market_min_miles]
  product_data[, Extra_Miles_Sq := Extra_Miles * Extra_Miles]
  
  # Calculate Counts of Firm Types
  ucc <- c("Spirit Air Lines", "Allegiant Air", "Frontier Airlines Inc.")
  lcc <- c("JetBlue Airways","Southwest Airlines Co.")
  leg <- c("Delta Air Lines Inc.", "United Air Lines Inc.", "American Airlines Inc.")
  
  # product_data[, Other_UCC_Count := Spirit_Prescence + Allegiant_Prescence + Frontier_Prescence]
  # product_data[, Other_LCC_Count := Alaska_Prescence + JetBlue_Prescence + Southwest_Prescence]
  # product_data[, Other_Leg_Count := Delta_Prescence + United_Prescence + American_Prescence]
 
  # product_data[, UCC := 0]
  # product_data[Carrier %in% ucc, UCC := 1]
  # product_data[Carrier %in% ucc, Other_UCC_Count := Other_UCC_Count - 1]
  # product_data[Carrier %in% lcc, Other_LCC_Count := Other_LCC_Count - 1]
  # product_data[Carrier %in% leg, Other_Leg_Count := Other_Leg_Count - 1]
  
  product_data[, Market_Passengers := sum(Passengers.Product), by = market_group]
  product_data[, Own_Passengers := sum(Passengers.Product), by = c(market_group, "Carrier")]
  
  # Dummy Variables for Carriers
  product_data[, AmericanAir := as.numeric(Carrier == "American Airlines Inc.")]
  product_data[, Delta := as.numeric(Carrier == "Delta Air Lines Inc.")]
  product_data[, Southwest := as.numeric(Carrier == "Southwest Airlines Co.")]
  product_data[, United := as.numeric(Carrier == "United Air Lines Inc.")]
  product_data[, Spirit := as.numeric(Carrier == "Spirit Air Lines")]
  product_data[, JetBlue := as.numeric(Carrier == "JetBlue Airways")]
  product_data[, Other_Carrier := 1 - AmericanAir - Delta - Southwest - United - Spirit - JetBlue]
  
  # Mkt Miles Flown in Hundreds
  product_data[, MktMilesFlown := MktMilesFlown / 1000]
  
  # Squared Distance Term
  product_data[, MktMilesFlown_Sq := MktMilesFlown * MktMilesFlown]
  
  # Calculate number of products in the market
  product_data[, Num_Products_In_Market := .N, by = market_group]
  product_data[, Other_Products_In_Market := .N - 1, by = market_group]
  product_data[, Num_NonStop_In_Market := sum(NonStop), by = market_group]
  product_data[, Firm_Products_In_Market := .N, by = c(market_group, "Carrier")]
  product_data[, Rival_Products_In_Market := Num_Products_In_Market - Firm_Products_In_Market]
  product_data[, Num_Firms_In_Market := length(unique(Carrier)), by = market_group]
  
  
  # Calculate Average Other Distance in the Market 
  product_data[, Average_Other_Distance := sum(MktMilesFlown), by = market_group]
  product_data[, Average_Other_Distance := (Average_Other_Distance - MktMilesFlown) / (Num_Products_In_Market - 1)]
  
  # Calculate Average Other Products' Distance Square
  product_data[, Average_Other_Distance_Square := sum(MktMilesFlown_Sq), by = market_group]
  product_data[, Average_Other_Distance_Square := (Average_Other_Distance_Square - MktMilesFlown_Sq) / (Num_Products_In_Market - 1)]

  # Calculate Number of Rival Direct Products in the Market
  product_data[, Rival_Direct := sum(NonStop), by = market_group]
  product_data[, Rival_Direct := Rival_Direct - sum(NonStop), by = c(market_group, "Carrier")]
  
  # Calculate Average Rival Firm Origin, Destination Shares
  product_data[, Fractional_Origin_Ratio := Origin_Firm_Service_Ratio / Firm_Products_In_Market ]
  product_data[, Sum_Origin_Ratios := sum(Fractional_Origin_Ratio), by = market_group]
  product_data[, Other_Origin_Ratios := Sum_Origin_Ratios - Origin_Firm_Service_Ratio]
  product_data[, Average_Rival_Origin_Ratio := Other_Origin_Ratios / (Num_Firms_In_Market - 1)]
  product_data[, Fractional_Origin_Ratio := NULL]
  product_data[, Sum_Origin_Ratios := NULL]
  product_data[, Other_Origin_Ratios := NULL]
  
  product_data[, Fractional_Dest_Ratio := Destination_Firm_Service_Ratio / Firm_Products_In_Market]
  product_data[, Sum_Dest_Ratios := sum(Fractional_Dest_Ratio), by = market_group]
  product_data[, Other_Dest_Ratios := Sum_Dest_Ratios - Destination_Firm_Service_Ratio]
  product_data[, Average_Rival_Dest_Ratio := Other_Dest_Ratios / (Num_Firms_In_Market - 1)]
  product_data[, Fractional_Dest_Ratio := NULL]
  product_data[, Sum_Dest_Ratios := NULL]
  product_data[, Other_Dest_Ratios := NULL]

    # Tourism Dummy from Tracing the Woes
  product_data[, Tourism := FALSE]
  product_data[Destination_State == "FL", Tourism := TRUE]
  product_data[Origin_State == "FL", Tourism := TRUE]
  product_data[Origin_MSA == "Las Vegas-Henderson-Paradise, NV", Tourism := TRUE]
  product_data[Destination_MSA == "Las Vegas-Henderson-Paradise, NV", Tourism := TRUE]
  product_data[, Tourism := as.numeric(Tourism)]
  
  # Exogeneous Characteristic Products Interactions
  product_data[, MktMiles_Origin_Share := MktMilesFlown * Origin_Firm_Service_Ratio]
  product_data[, MktMiles_Destination_Share := MktMilesFlown * Destination_Firm_Service_Ratio]
  product_data[, MktMiles_Sq_Origin_Share := MktMilesFlown_Sq * Origin_Firm_Service_Ratio]
  product_data[, MktMiles_Sq_Destination_Share := MktMilesFlown_Sq * Destination_Firm_Service_Ratio]
  product_data[, MktMiles_NonStop := MktMilesFlown * NonStop]
  product_data[, MktMiles_Sq_NonStop := MktMilesFlown_Sq * NonStop]
  product_data[, MktMiles_Tourism := MktMilesFlown * Tourism]
  product_data[, MktMiles_Extra := MktMilesFlown * Extra_Miles]
  product_data[, MKtMiles_Sq_Tourism := MktMilesFlown_Sq * Tourism]
  product_data[, MktMiles_Sq_Extra:= MktMilesFlown_Sq * Extra_Miles ]
  product_data[, NonStop_Origin_Share := NonStop * Origin_Firm_Service_Ratio]
  product_data[, Direct_Destination_Share := NonStop * Destination_Firm_Service_Ratio]
  product_data[, OriginRatio_Extra := Origin_Firm_Service_Ratio * Extra_Miles]
  product_data[, DestinationRation_Extra := Destination_Firm_Service_Ratio * Extra_Miles]
  product_data[, NonStop_LCC := NonStop * (1 - Delta - United - AmericanAir)]
  product_data[, Extra_LCC := Extra_Miles * (1 - Delta - United - AmericanAir)]
  product_data[, Origin_LCC := Origin_Firm_Service_Ratio * (1 - Delta - United - AmericanAir)]
  product_data[, Destination_LCC := Destination_Firm_Service_Ratio * (1 - Delta - United - AmericanAir)]
  product_data[, MktMiles_OriginHub := MktMilesFlown * Origin_Hub]
  product_data[, MktMiles_DestinationHub := MktMilesFlown * Destination_Hub]
  product_data[, MktMilesSq_OriginHub := MktMilesFlown_Sq * Origin_Hub]
  product_data[, MktMilesSq_DestinationHub := MktMilesFlown_Sq * Destination_Hub]
  product_data[, NonStop_OriginHub := NonStop * Origin_Hub]
  product_data[, NonStop_DestinationHub := NonStop * Destination_Hub]
  product_data[, ExtraMiles_OriginHub := Extra_Miles * Origin_Hub]
  product_data[, ExtraMiles_Destination_Hub := Extra_Miles * Destination_Hub]
  product_data[, OriginRatio_OriginHub := Origin_Firm_Service_Ratio * Origin_Hub]
  product_data[, OriginRatio_DestinationHub := Origin_Firm_Service_Ratio * Destination_Hub]
  product_data[, DestinationRatio_OriginHub := Destination_Firm_Service_Ratio * Origin_Hub]
  product_data[, DestinationRatio_DestinationHub := Destination_Firm_Service_Ratio * Destination_Hub]
  product_data[, NonStop_MktSize := NonStop * Potential_Passengers / 100000]
  product_data[, OriginRatio_Size := Origin_Firm_Service_Ratio * Potential_Passengers / 100000]
  product_data[, DestinationRatio_Size := Destination_Firm_Service_Ratio * Potential_Passengers / 100000]
  product_data[, MktMiles_Size := MktMilesFlown * Potential_Passengers / 100000]
  product_data[, NonStop_ExtraMiles := Extra_Miles * NonStop]
  product_data[, MktMiles_Sq_Interact := MktMilesFlown * MktMilesFlown_Sq]
  
  # Add Variable which is 1 if either side is a hub
  product_data[, Hub_Endpoint := Origin_Hub | Destination_Hub]
  product_data[, Hub_End_MktMiles := Hub_Endpoint * MktMilesFlown]
  product_data[, Hub_End_MktMiles_Sq := Hub_Endpoint * MktMilesFlown * MktMilesFlown]
  product_data[, Hub_End_NonStop := Hub_Endpoint * NonStop]

  # Variable for Year-Quarter Fixed Effects
  product_data[, Year_Quarter_Effect := paste(Year, " X ", Quarter)]
  
  # Drop all Monopoly Markets
  # product_data <- product_data[Num_Firms_In_Market > 1,]
  
  # Align Names with PyBLP Requirements
  product_data$prices <- product_data$Avg.Fare;
  product_data$Avg.Fare <- NULL
  
  # Price in 100s
  product_data[, prices := prices / 100]
  
  # Nominal Prices
  product_data[, nominal_prices := prices]
  # Real prices
  product_data[, prices := prices / (price_index / 100)]
  product_data[, Jet_Fuel_Price := Jet_Fuel_Price / (price_index / 100)]
  product_data[, Jet_Fuel_Square := Jet_Fuel_Price^2]
  
  product_data$market_ids <- product_data$Market_ID
  product_data$Market_ID <- NULL;
  
  product_data$product_ids <- product_data$Product_Name
  
  product_data$firm_ids <- as.numeric(as.factor(product_data$Carrier))
  
  product_data$shares <- product_data$Share
  product_data$Share <- NULL
  
  # Nevo (2001) Table 5 Outcome Variable
  product_data[, Outside_Share := 1 - sum(shares), by = market_group]
  product_data[, Share_Differences := log(shares) - log(Outside_Share)]
  product_data[, Log_Share_Ratio := log(shares / sum(shares)), by = market_group]
  
  # Gas Price Based Supply Shifters
  product_data[, GasMiles := National_Average_JF_Price * MktMilesFlown]
  product_data[, GasMiles_Sq := GasMiles*GasMiles]
  product_data[, GasMiles_Origin_Dest := GasMiles * Origin_Firm_Destinations]
  product_data[, GasMiles_Origin_Prescence := GasMiles * Origin_Firm_Service_Ratio]
  product_data[, GasMiles_Dest_Prescence := GasMiles * Destination_Firm_Service_Ratio]
  product_data[, GasMiles_Dest_Dest := GasMiles * Destination_Firm_Destinations]
  product_data[, GasMiles_ExtraMiles := GasMiles * Extra_Miles]
  product_data[, GasMiles_LCC := GasMiles * (1 - Delta - United - AmericanAir)]
  product_data[, GasMiles_Hub := GasMiles * Destination_Hub]
  product_data[, GasMiles_OriginHub := GasMiles * Origin_Hub]
  product_data[, GasMiles_NonStop := GasMiles * NonStop]
  product_data[, GasMiles_AllHub := GasMiles * Hub_Endpoint]

  # Gas Price Carrier Interactions
  product_data[, GasMiles_Delta := (Carrier == "Delta Air Lines Inc.") * GasMiles]
  product_data[, GasMiles_United := (Carrier == "United Air Lines Inc.") * GasMiles]
  product_data[, GasMiles_American:= (Carrier == "American Airlines Inc.") * GasMiles]
  product_data[, GasMiles_JetBlue := (Carrier == "JetBlue Airways") * GasMiles]
  product_data[, GasMiles_American:= (Carrier == "American Airlines Inc.") * GasMiles]
  product_data[, GasMiles_Spirit := (Carrier == "Spirit Air Lines") * GasMiles]
  
  # Hausman, Per Mile Instruments
  product_data[, Product_Per_Mile_Price :=  prices / MktMilesFlown]
  product_data[, Firm_Mkt_Pass := sum(Passengers.Product),
               by =  c("Year", "Quarter", "Origin", "Dest", "Carrier")]
  product_data[, Firm_Per_Mile_Mkt := sum(Product_Per_Mile_Price * Passengers.Product) / Firm_Mkt_Pass, 
               by = c("Year", "Quarter", "Origin", "Dest", "Carrier")]
  product_data.haus <- unique(product_data[, .(Year, Quarter, Origin, Dest, Carrier, Firm_Per_Mile_Mkt,
                                        Firm_Mkt_Pass)])
  product_data.haus[, Firm_Time_Pass := sum(Firm_Mkt_Pass), by = c("Year", "Quarter", "Carrier")]
  product_data.haus[, Firm_Other_Mkt_Per_Mile := (sum(Firm_Per_Mile_Mkt * Firm_Mkt_Pass) - Firm_Per_Mile_Mkt)/
                      (Firm_Time_Pass - Firm_Mkt_Pass),
                    by = c("Year", "Quarter", "Carrier")]
  product_data.haus <- product_data.haus[, .(Year, Quarter, Carrier, Origin, Dest,
                                             Firm_Other_Mkt_Per_Mile)]
  product_data <- merge(product_data, product_data.haus, all.x = TRUE, by = c("Year", "Quarter", "Carrier",
                                                                              "Origin", "Dest"))
  product_data[, Product_Per_Mile_Price := NULL]
  product_data[, Firm_Mkt_Pass := NULL]
  product_data[, Firm_Per_Mile_Mkt := NULL]
  
  # Drop Near Markets
  product_data <- product_data[, MktMiles.Min := min(MktMilesFlown), by = c("Origin", "Dest")]
  product_data <- product_data[MktMiles.Min > 0.150,]
  
  ### Generate Differentiation Instruments ###
  if(!fast){
    gandhi_instruments <- pyblp$build_differentiation_instruments(
      pyblp$Formulation("NonStop + MktMilesFlown + I(MktMilesFlown**2) + Extra_Miles + Origin_Firm_Service_Ratio"),
      product_data,
      interact = F)
  
    gandhi_instruments_R <- as.data.table(py_to_r(gandhi_instruments))
  
    # Remove Instruments with Zero Variance
    variance <- c();
    for(i in 1:ncol(gandhi_instruments_R)){
      variance <- c(variance, var(gandhi_instruments_R[1:nrow(gandhi_instruments_R), ..i]))
    }
    real_variation <- (1:ncol(gandhi_instruments_R))[variance > 0]
    gandhi_instruments_R <- gandhi_instruments_R[, ..real_variation]
  
    # Keep Unique Instruments Only
    gandhi_instruments_R <- transpose(gandhi_instruments_R)
    gandhi_instruments_R <- unique(gandhi_instruments_R)
    gandhi_instruments_R <- transpose(gandhi_instruments_R)
  
    colnames(gandhi_instruments_R) <- paste("demand_instruments", 0:(ncol(gandhi_instruments_R)-1), sep = "")
  
    product_data <- cbind(product_data, gandhi_instruments_R)
  }
  
  
  product_data[, nesting_ids := 1]
  
  product_data[, clustering_ids := as.numeric(as.factor(paste(Origin, Dest)))]
  
  write_rds(product_data, output_file)
  # For Stata
  write_csv(product_data, file = gsub(pattern = ".rds", replacement = ".csv",
                                      x = output_file), na = ".")
}

airport_service_ratios_merger <- function(db1b){
  db1b[, Next.Airport := Dest]
  nonstop_plane <- db1b[, .(Year, Quarter, Origin, Next.Airport,
                            merger_carrier)]
  db1b[, Next.Airport := NULL]
  nonstop_plane <- unique(nonstop_plane)
  
  # Evaluate all destinations available
  # (t = total)
  nonstop_plane_t <- nonstop_plane;
  nonstop_plane_t$merger_carrier <- NULL
  nonstop_plane_t <- unique(nonstop_plane_t)
  nonstop_plane_t[, Destinations.Available := .N, by = c("Year", "Quarter", "Origin")]
  nonstop_plane_t[, Next.Airport := NULL]
  nonstop_plane_t <- unique(nonstop_plane_t)
  
  # Now, For Each Firm, Identify Number of Destinations They Serve
  nonstop_plane[, Firm.Destinations := .N, by = c("merger_carrier", "Year", "Quarter", "Origin")]
  
  # Merge, Calculate Ratio
  nonstop_plane <- merge(nonstop_plane, nonstop_plane_t, by = c("Year", "Quarter", "Origin"),
                         all.x = TRUE)
  nonstop_plane[is.na(Firm.Destinations), Firm.Destinations := 1]
  nonstop_plane[, Firm.Ratio := Firm.Destinations / Destinations.Available * 100]
  
  return(ratio_data)
}


merger_simulation_advanced <- function(model_in = "03.Output/random_coeff_nested_logit_results.pickle",
                                       data_in = "02.Intermediate/Product_Data.rds",
                                       data_out = "03.Output/Adv_Merger_Sim_Data.rds",
                                       linear = pyblp$Formulation('0 + prices + NonStop + MktMilesFlown + I(MktMilesFlown**2) + Origin_Firm_Service_Ratio + Extra_Miles + Extra_Miles_Sq + Tourism + C(Year_Quarter_Effect) + C(Carrier)'),
                                       nonlinear = pyblp$Formulation("0 + prices + NonStop + MktMilesFlown"),
                                       mode = "rcl"){
  model <- py_load_object(model_in)
  data <- readRDS(data_in)

  data[, merger_carrier := Carrier]
  data[Carrier == "Spirit Air Lines", merger_carrier := "JetBlue Airways"]
  data[, unobserved := model$xi]

  # Compute Costs
  data[, cost := model$compute_costs()]

  # Get Original Markets, Consumer Surplus
 original_problem <- pyblp$Problem(c(linear, nonlinear), data,
                                   integration = pyblp$Integration('halton', size = 250L,
                                                 specification_options = dict("seed" = 97L)))
 original_markets <- as.numeric(original_problem$unique_market_ids)
  cs_observed <- model$compute_consumer_surpluses()
  original_cs <- data.table(market_ids = original_markets, ConsumerSurplus_Observed = cs_observed)

  data.new <- data %>% group_by(merger_carrier, Origin, Dest, Year,
                                Quarter, Year_Quarter_Effect, NonStop, 
                                market_ids, nesting_ids) %>%
    summarize(NonStopMiles = min(NonStopMiles),
              MktMilesFlown = min(MktMilesFlown),
              shares = sum(shares),
              prices = mean(prices),
              Tourism = mean(Tourism),
              market_min_miles = min(market_min_miles),
              unobserved.best = max(unobserved),
              unobserved.avg = mean(unobserved),
              unobserved.worst = min(unobserved),
              costs.min = min(cost),
              costs.mean = mean(cost),
              costs.max = max(cost),
              costs.min.95 = min(cost* 0.95^(merger_carrier == "JetBlue Airways")) ,
              costs.min.90 = min(cost* 0.90^(merger_carrier == "JetBlue Airways")) ,
              costs.mean.95 = mean(cost* 0.95^(merger_carrier == "JetBlue Airways")) ,
              costs.mean.90 = mean(cost* 0.90^(merger_carrier == "JetBlue Airways")) ,
              costs.max.95 = max(cost* 0.95^(merger_carrier == "JetBlue Airways")) ,
              costs.max.90 = max(cost * 0.90^(merger_carrier == "JetBlue Airways")) ,
              Potential_Passengers = mean(Potential_Passengers)) %>%
    mutate(Extra_Miles = MktMilesFlown - market_min_miles,
           MktMilesFlown_Sq = MktMilesFlown * MktMilesFlown,
           firm_ids = merger_carrier,
           NonStop = as.numeric(NonStop),
           Carrier = merger_carrier,
           UCC = as.numeric(Carrier %in% c("Spirit Air Lines", "Allegiant Air", "Frontier Airlines Inc."))) %>%
    as.data.table()

  data.price <- data %>% select(Origin, Dest, Year, Quarter, Year_Quarter_Effect,
                                NonStop, market_ids, Carrier, merger_carrier,prices, shares) %>%
    filter(!Carrier == "Spirit Air Lines") %>%
    mutate(Fare.Original = prices,
           Share.Original = shares,
           prices = NULL,
           shares = NULL) %>% as.data.table()

  data.price[, Share.WithinMarket.Original := Share.Original / sum(Share.Original),
             by = market_ids]

  data.new <- merge(data.new, data.price)

  remove(data); gc();

  # Recompute Origin Prescence Variable
  ratio_data <- readRDS("02.Intermediate/Merger_Service_Ratios.Rds")

  colnames(ratio_data) <- c("Year", "Quarter", "Carrier", "Origin",
            "Origin_Firm_Destinations", "Origin_Firm_Service_Ratio")
  data.new <- merge(data.new, ratio_data, by = c("Year", "Quarter", "Carrier", "Origin"),
                      all.x = TRUE);

  # Need to remove Spirit Beta Coeff
  beta_vec <- model$beta
  beta_labels <- model$beta_labels
  beta_vec <- beta_vec[!grepl(pattern = "Spirit", x = beta_labels)]
  rho_est <- model$rho
  
  
  if(mode == "rcl"){
    components <- c(linear, nonlinear)
    sigma_vec <- model$sigma;
  } else {
    components <- linear
  }

  remove(model); gc()

  simulation.best <- pyblp$Simulation(product_formulations = components,
                                 product_data = data.new,
                                 beta = beta_vec,
                                 sigma = sigma_vec,
                                 rho = rho_est,
                                 integration = pyblp$Integration('product', 9L,
                                            specification_options = dict("seed" = 97L)),
                                 xi = data.new$unobserved.best)

  simulation.avg <- pyblp$Simulation(product_formulations = components,
                                      product_data = data.new,
                                      beta = beta_vec,
                                      sigma = sigma_vec,
                                     rho = rho_est,
                                      integration = pyblp$Integration('product', 9L,
                                                                      specification_options = dict("seed" = 97L)),
                                      xi = data.new$unobserved.avg)

  simulation.worst <- pyblp$Simulation(product_formulations = components,
                                      product_data = data.new,
                                      beta = beta_vec,
                                      sigma = sigma_vec,
                                      rho = rho_est,
                                      integration = pyblp$Integration('product', 9L,
                                                                      specification_options = dict("seed" = 97L)),
                                      xi = data.new$unobserved.worst)



  simulation.min <- simulation.best$replace_endogenous(costs = data.new$costs.min); gc();
  simulation.min.95 <- simulation.best$replace_endogenous(costs = data.new$costs.min.95); gc();
  simulation.min.90 <- simulation.best$replace_endogenous(costs = data.new$costs.min.90); gc();
  simulation.mean <- simulation.avg$replace_endogenous(costs = data.new$costs.mean); gc();
  simulation.mean.95 <- simulation.avg$replace_endogenous(costs = data.new$costs.mean.95); gc();
  simulation.mean.90 <- simulation.avg$replace_endogenous(costs = data.new$costs.mean.90); gc();
  simulation.max <- simulation.worst$replace_endogenous(costs = data.new$costs.max); gc()
  simulation.max.95 <- simulation.worst$replace_endogenous(costs = data.new$costs.max.95); gc()
  simulation.max.90 <- simulation.worst$replace_endogenous(costs = data.new$costs.max.90); gc()
  

  data.new[, Shares.MinCost.Sim := py_to_r(simulation.min$product_data$shares)]
  data.new[, Shares.MinCost.Sim.95 := py_to_r(simulation.min.95$product_data$shares)]
  data.new[, Shares.MinCost.Sim.90 := py_to_r(simulation.min.90$product_data$shares)]
  data.new[, Shares.MeanCost.Sim := py_to_r(simulation.mean$product_data$shares)]
  data.new[, Shares.MeanCost.Sim.95 := py_to_r(simulation.mean.95$product_data$shares)]
  data.new[, Shares.MeanCost.Sim.90 := py_to_r(simulation.mean.90$product_data$shares)]
  data.new[, Shares.MaxCost.Sim := py_to_r(simulation.max$product_data$shares)]
  data.new[, Shares.MaxCost.Sim.95 := py_to_r(simulation.max.95$product_data$shares)]
  data.new[, Shares.MaxCost.Sim.90 := py_to_r(simulation.max.90$product_data$shares)]
  
  data.new[, Prices.MinCost.Sim := py_to_r(simulation.min$product_data$prices)]
  data.new[, Prices.MinCost.Sim.95 := py_to_r(simulation.min.95$product_data$prices)]
  data.new[, Prices.MinCost.Sim.90 := py_to_r(simulation.min.90$product_data$prices)]
  data.new[, Prices.MeanCost.Sim := py_to_r(simulation.mean$product_data$prices)]
  data.new[, Prices.MeanCost.Sim.95 := py_to_r(simulation.mean.95$product_data$prices)]
  data.new[, Prices.MeanCost.Sim.90 := py_to_r(simulation.mean.90$product_data$prices)]
  data.new[, Prices.MaxCost.Sim := py_to_r(simulation.max$product_data$prices)]
  data.new[, Prices.MaxCost.Sim.95 := py_to_r(simulation.max.95$product_data$prices)]
  data.new[, Prices.MaxCost.Sim.90 := py_to_r(simulation.max.90$product_data$prices)]
  

  data.new[, Shares.WithinMarket.MinCost := Shares.MinCost.Sim / sum(Shares.MinCost.Sim),
           by = c("market_ids")]
  data.new[, Shares.WithinMarket.MinCost.95 := Shares.MinCost.Sim.95 / sum(Shares.MinCost.Sim.95),
           by = c("market_ids")]
  data.new[, Shares.WithinMarket.MinCost.90 := Shares.MinCost.Sim.90 / sum(Shares.MinCost.Sim.90),
           by = c("market_ids")]
  
  data.new[, Shares.WithinMarket.MeanCost := Shares.MeanCost.Sim / sum(Shares.MinCost.Sim),
           by = c("market_ids")]
  data.new[, Shares.WithinMarket.MeanCost.95 := Shares.MeanCost.Sim.95 / sum(Shares.MinCost.Sim.95),
           by = c("market_ids")]
  data.new[, Shares.WithinMarket.MeanCost.90 := Shares.MeanCost.Sim.90 / sum(Shares.MinCost.Sim.90),
           by = c("market_ids")]
  
  data.new[, Shares.WithinMarket.MaxCost := Shares.MaxCost.Sim / sum(Shares.MinCost.Sim),
           by = c("market_ids")]
  data.new[, Shares.WithinMarket.MaxCost.95 := Shares.MaxCost.Sim.95 / sum(Shares.MinCost.Sim.95),
           by = c("market_ids")]
  data.new[, Shares.WithinMarket.MaxCost.90 := Shares.MaxCost.Sim.90 / sum(Shares.MinCost.Sim.90),
           by = c("market_ids")]
  

  # Add each markets Consumer Surplus
  markets <- as.numeric(simulation.best$unique_market_ids)
  cs.best <- as.numeric(simulation.min$compute_consumer_surpluses())
  cs.avg <- as.numeric(simulation.mean$compute_consumer_surpluses())
  cs.worst <- as.numeric(simulation.max$compute_consumer_surpluses())

  cs_table <- data.table(market_ids = markets,
                         CS_Best = cs.best,
                         CS_Average = cs.avg,
                         CS_Worst = cs.worst)

  cs_table <- merge(cs_table, original_cs, by = "market_ids", all.x = TRUE)
  data.new <- merge(data.new, cs_table, by = "market_ids", all.x = TRUE)

  saveRDS(data.new, file = data_out)
}

bankruptcy_simulation <- function(model_in = "03.Output/random_coeff_nested_logit_results.pickle",
                                  data_in = "02.Intermediate/Product_Data.rds",
                                  data_out = "03.Output/Bankruptcy_Sim_Data.rds",
                                  linear = pyblp$Formulation('0 + prices + NonStop + MktMilesFlown + I(MktMilesFlown**2) + Origin_Firm_Service_Ratio + Extra_Miles + Extra_Miles_Sq + Tourism + C(Year_Quarter_Effect) + C(Carrier)'),
                                  nonlinear = pyblp$Formulation("0 + prices + NonStop + MktMilesFlown"),
                                  mode = "rcl"){
    model <- py_load_object(model_in)
    data <- readRDS(data_in)
    
    data[, merger_carrier := Carrier]
    data[, unobserved := model$xi]
    
    # Compute Costs
    data[, cost := model$compute_costs()]
    
    data.new <- data %>% filter(Carrier != "Spirit Air Lines") %>%
      mutate(Fare.Original = prices,
             Share.Original = shares,
             prices = NULL,
             shares = NULL,
             Origin_Firm_Destinations = NULL,
             Origin_Firm_Service_Ratio = NULL) %>%
      as.data.table()

    data.new[, Share.WithinMarket.Original := Share.Original / sum(Share.Original),
               by = market_ids]
    
    remove(data); gc();
    
    # Recompute Origin Prescence Variable
    ratio_data <- readRDS("02.Intermediate/Merger_Service_Ratios.Rds")
    
    colnames(ratio_data) <- c("Year", "Quarter", "Carrier", "Origin",
                              "Origin_Firm_Destinations", "Origin_Firm_Service_Ratio")
    data.new <- merge(data.new, ratio_data, by = c("Year", "Quarter", "Carrier", "Origin"),
                      all.x = TRUE);
    
    # Need to remove Spirit Beta Coeff
    beta_vec <- model$beta
    # beta_labels <- model$beta_labels
    # beta_vec <- beta_vec[!grepl(pattern = "Spirit", x = beta_labels)]
    
    rho_est <- model$rho
    
    if(mode == "rcl"){
      components <- c(linear, nonlinear)
      sigma_vec <- model$sigma;
    } else {
      components <- linear
    }
    
    remove(model); gc()
    
    simulation.new <- pyblp$Simulation(product_formulations = components,
                                        product_data = data.new,
                                        beta = beta_vec,
                                        sigma = sigma_vec,
                                         rho = rho_est,
                                        integration = pyblp$Integration('product', 9L,
                                          specification_options = dict("seed" = 97L)),
                                        xi = data.new$unobserved)
    
    simulation.drop <- simulation.new$replace_endogenous(costs = data.new$cost); gc();

    data.new[, Shares.Drop.Sim := py_to_r(simulation.new$product_data$shares)]
    data.new[, Prices.Drop.Sim := py_to_r(simulation.new$product_data$prices)]
    
    data.new[, Shares.WithinMarket.Drop := Shares.Drop.Sim / sum(Shares.Drop.Sim),
             by = c("market_ids")]
    
    saveRDS(data.new, file = data_out)
}
