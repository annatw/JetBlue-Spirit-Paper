merger_simulation_bootstrap_prep <- function(model_in = "03.Output/random_coeff_nested_logit_results.pickle",
    data_in = "02.Intermediate/Product_Data.rds",
    markup_out = "02.Intermediate/bootstrap_markups.rds",
    costs_out = "02.Intermediate/bootstrap_marginal_costs.rds",
    linear = pyblp$Formulation('0 + prices + NonStop + MktMilesFlown + I(MktMilesFlown**2) + Origin_Firm_Service_Ratio + Extra_Miles + Extra_Miles_Sq + Tourism + C(Year_Quarter_Effect) + C(Carrier)'),
    nonlinear = pyblp$Formulation("0 + prices + NonStop + MktMilesFlown"),
    mode = "rcl", reps = 500){
  model <- py_load_object(model_in)
  data <- readRDS(data_in)
  
  data[, merger_carrier := Carrier]
  data[Carrier == "Spirit Air Lines", merger_carrier := "JetBlue Airways"]
  data[, unobserved := model$xi]
  
  markup_set <- c()
  cs_set <- c()
  costs_set <- c()
  for(i in 1:reps){
    bootstrap <- model$bootstrap(draws = 1L)
    markup_col <- as.data.table(bootstrap$compute_markups()[,,1]);
    costs_col <- as.data.table(bootstrap$compute_costs()[,,1]);
    if(i == 1){
      markup_set <- markup_col
      costs_set <- costs_col;
    } else {
      markup_set <- cbind(markup_set, markup_col)
      costs_set <- cbind(costs_set, costs_col)
      gc(); gc();
    }
  }
  
  write_rds(markup_set, markup_out)
  write_rds(costs_set, costs_out)
}


