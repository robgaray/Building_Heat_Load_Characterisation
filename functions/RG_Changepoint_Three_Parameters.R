RG_Changepoint_Three_Parameters <- function(slope_Temp, slope_Irrad, intercept, minimum, DF_input)
{
  ###This function calculates a three parameter heat load curve
  ### Input: parameters and the boundary conditions
  ### Output: power
  ### Formula: P =max(slope_Temp*Temp+Slope_Irrad*Irrad+Intercept, minimum)
  ### DF_input: a dataset containing at least the following variables: "Temp", "Irrad"
  
  # Chechking of inputs
  # Returns warnings
  Out<-0          # if out=1, gets out of the functioh without doing nothing

    
  ## DF_input
  {
    if (!is.data.frame(DF_input))
    {
      warning ("the provided object as input for the data is not a data frame, the function didn`t do anything")
      Out<-1
    }
    if (!("Temperature" %in% names(DF_input)))
    {
      warning ("The provided data frame did not contain the temperature variable properly coded as 'Temperature', the function didn`t do anything")
      Out<-1
    }
    if (!("Solar.Irradiation" %in% names(DF_input)))
    {
      warning ("The provided data frame did not contain the irradiation variable properly coded as 'Solar.Irradiation', the function didn`t do anything")
      Out<-1
    }
    if (!is.numeric(slope_Temp))
    {
      warning ("The provided slope_Temp is not numeric, the function didn`t do anything")
      Out<-1
    }
    if (!is.numeric(slope_Irrad))
    {
      warning ("The provided slope_Irrad is not numeric, the function didn`t do anything")
      Out<-1
    }
    if (!is.numeric(intercept))
    {
      warning ("The provided intercept is not numeric, the function didn`t do anything")
      Out<-1
    }
    
    if (Out==0 && !("Power" %in% names(DF_input)))
    {
      warning ("The provided data frame did not contain the Power variable properly coded as 'Power', the function didn`t calculate the residuals")
      Out<-0.1
    }
    
    DF_output<-DF_input
    
    if (!Out==1)
    {
      output<-pmax(minimum,
                  intercept
                  + slope_Temp*DF_input$Temperature
                  + slope_Irrad*DF_input$Solar.Irradiation)
      DF_output$Power_fitted<-output
    }
    
    if (Out==0)
    {
      DF_output$Power_residuals<-DF_output$Power_fitted - DF_output$Power
    }
    else if (Out==0.1)
    {
      DF_output$Power_residuals<-rep(NA,dim(DF_output)[1])
    }
  }
  return(DF_output)
}

RG_R2_Changepoint_Three_Parameters<-function(slope_Temp, slope_Irrad, intercept, minimum, DF_input)
{
  cor(RG_Changepoint_Three_Parameters(slope_Temp,
                                      slope_Irrad,
                                      intercept,
                                      minimum,
                                      Dat)$Power,
      RG_Changepoint_Three_Parameters(slope_Temp,
                                      slope_Irrad,
                                      intercept,
                                      minimum,
                                      Dat)$Power_fitted)^2
}
  
  