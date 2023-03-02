RG_Changepoint_Three_Parameters_FUN_OBJETIVO<- function (slope_Temp, slope_Irrad, intercept, minimum, DF_input)
{
  -sum(abs(RG_Changepoint_Three_Parameters(slope_Temp, slope_Irrad, intercept, minimum, DF_input)$Power_residuals))
}

RG_Detect_Outlier_Model_Based_Ch_3P <- function (DF_input, threshold_outlier=2, pop_size= 100, max_iter=1000)
{
  slope_Temp_MIN  <- -max(DF_input$Power)/(max(DF_input$Temperature)-min(DF_input$Temperature))
  slope_Irrad_MIN <- -max(DF_input$Power)/max(DF_input$Solar.Irradiation)
  intercept_MAX   <- max(DF_input$Power)
  minimum_MAX     <- max(DF_input$Power)
  
  optimizado <- ga(
    type    = "real-valued",
    fitness = function (x) RG_Changepoint_Three_Parameters_FUN_OBJETIVO(slope_Temp  = x[1],
                                                                        slope_Irrad = x[2],
                                                                        intercept   = x[3],
                                                                        minimum     = x[4],
                                                                        DF_input    = DF_input),
    lower   = c(slope_Temp_MIN,slope_Irrad_MIN,0            ,0          ),
    upper   = c(0             ,0              ,intercept_MAX,minimum_MAX),
    popSize = pop_size,
    maxiter = max_iter,
    run     = 100,
    monitor = FALSE
  )
  
  parameters_OPT  <- optimizado@solution
  
  slope_Temp_OPT  <- optimizado@solution[1]
  slope_Irrad_OPT <- optimizado@solution[2]
  intercept_OPT   <- optimizado@solution[3]
  minimum_OPT     <- optimizado@solution[4]
  
  
  
  DF_Output <- RG_Changepoint_Three_Parameters(slope_Temp_OPT,slope_Irrad_OPT,intercept_OPT,minimum_OPT, DF_input)
  
  # RG_inspect_data (DF_Output, Var_list_input=c("Temperature", "Power"))
  # abline(h=minimum_OPT, col="blue")
  # abline(intercept_OPT,slope_Temp_OPT, col="blue")
  
  # RG_inspect_data (DF_Output, Var_list_input=c("Power", "Power_fitted"))
  # abline(0,1, col="red")
  
  DF_Output$IS_Outlier <- abs(DF_Output$Power_residuals-mean(DF_Output$Power_residuals)) > threshold_outlier * sd(DF_Output$Power_residuals)
  
  return( list(DF_Output, parameters_OPT))
}











