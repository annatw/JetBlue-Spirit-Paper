# Generate a DataTable that allows for comparing the estimated MC for the scenario in which an auxiliary fee fix
# Has been used and one in which it has not
product_mc_compare_generate <- function(model_in_unadjust = "03.Output/random_coeff_nested_logit_results.pickle",
                               data_in_unadjust = "02.Intermediate/Product_Data.rds",
                               model_in_adjust = "03.Output/Auxilary_Scale_postpandemic_model.pickle",
                               data_in_adjust = "02.Intermediate/Auxilary_Scale_postpandemic.rds",
                               data_out = "02.Intermediate/Product_Data_Compare.rds"){
  # Function that Recovers MC, then Removes Model to Conserve Memory
  mc_recover <- function(product_data, model){
    model <- py_load_object(model);
    product_data <- readRDS(product_data)
    
    product_data[, marginal_cost := model$compute_costs()]
    remove(model); gc(); gc();
    return(product_data)
  }
  
  # _nf is no fix
  # _f is fix
  data_nf <- mc_recover(product_data = data_in_unadjust,
                        model = model_in_unadjust)
  data_f <- mc_recover(product_data = data_in_adjust,
                       model = model_in_adjust)
  
  # For Each Data Table, Grab The Following Variables:
  # Product Identifiers, Market Identifiers, 
  # Price, Marginal Cost, Shares, Ridership
  data_nf.reduce <- data_nf[, .(Year, Quarter, Origin, Dest,
                                Carrier, NonStop, product_ids, market_ids,
                                shares, Own_Passengers,JetBlue_Prescence,
                                prices, marginal_cost)]
  data_f.reduce <- data_f[, .(Year, Quarter, Origin, Dest,
                              Carrier, NonStop, product_ids, market_ids,
                              shares, Own_Passengers,JetBlue_Prescence,
                              prices, marginal_cost)]
  
  shared_col_names <- c("Year", "Quarter", "Origin", "Dest", "Carrier",
                        "NonStop", "product_ids", "market_ids",
                        "shares", "Own_Passengers", "JetBlue_Prescence")
  
  colnames(data_nf.reduce) <- c(shared_col_names, "price_unadjust", "marginal_cost_unadjust")
  colnames(data_f.reduce) <- c(shared_col_names, "price_adjust", "marginal_cost_adjust")
  
  data_combine <- merge(data_nf.reduce, data_f.reduce, by = shared_col_names)
  
  saveRDS(data_combine, file = data_out)
}

# Generate a Graph Showing the Difference in Estimated Marginal Costs for Each Spirit Product
spirit_mc_adjust_graph <- function(data_source = "02.Intermediate/Product_Data_Compare.rds",
                                   dollar_graph_out = "05.Figures/Fee_Fix_MC_Graph_Dollar.pdf",
                                   percent_graph_out = "05.Figures/Fee_Fix_MC_Graph_Percent.pdf",
                                   JB_restrict = FALSE,
                                   dollar_xlim = c(0, 325), 
                                   percent_xlim = c(0, 415)){
  product_data <- readRDS(data_source)
  
  product_data <- product_data[Carrier == "Spirit Air Lines"]
  
  if(JB_restrict == TRUE){
    product_data <- product_data[JetBlue_Prescence == TRUE]
  }
  # Adjust to be in  Dollars from 100 Dollars
  product_data[, MC_Difference := 100 * (marginal_cost_adjust - marginal_cost_unadjust)]
  product_data[, MC_Percent := (marginal_cost_adjust - marginal_cost_unadjust)/marginal_cost_unadjust * 100]
  
  # Generate Graphs
  ggplot(product_data, aes(x = MC_Difference)) + 
    geom_histogram(binwidth = 10, boundary = 0 ) +
    scale_x_continuous(limits = dollar_xlim, expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") + labs(x = "Change (2017 USD)", y = "Product Change")
  
  ggsave(dollar_graph_out, units = "in", width = 7, height = 3)
  
  ggplot(product_data, aes(x = MC_Percent)) + 
    geom_histogram(binwidth = 10, boundary = 0) +
    scale_x_continuous(expand = c(0,0), 
                       limits = percent_xlim) + 
    scale_y_continuous(expand = c(0,0)) + 
    theme(panel.background = element_blank(), 
          axis.line = element_line(linewidth = 0.25, colour = "black", linetype=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom") + labs(x = "Change (%)", y = "Product Count")
  ggsave(percent_graph_out, units = "in", width = 7, height = 3)
}


