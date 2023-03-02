RG_inspect_data <- function (DF_input, Var_list_input=c(), week_list_input=c(), month_list_input=c())
{
  ###
  # This function performs a summary of a data set.
  #
  # The data set is coded by name of variable.
  #   The following variable names should be present
  #     "DATE_hour_year"
  #     + variables to be inspected
  #   The following variable names are required to perform week and monthly inspections
  #     "DATE_week_year"
  #     "Month"
  #
  # Given a data  frame, performs a summary for 1 or 2 variables. As follows
  # If one variable is provided
  #   Var vs time for full year
  #   Var vs time for a list of selected weeks
  #   Var vs time for a list of selected months
  # If two variables is provided
  #   Var1 vs Var2 for full year
  #   Var1 vs Var2 for a list of selected weeks
  #   Var1 vs Var2 for a list of selected months
  ###
  
  
  # Chechking of inputs
  # Returns warnings
  {
    Out<-0          # if out=1, gets out of the functioh without doing nothing
    Out_weeks<-0    # if Out_weeks=1, gets out of the functioh without processing data for weeks
    Out_months<-0   # if Out_months=1, gets out of the functioh without processing data for months
    
    ## DF_input
    {
      if (!is.data.frame(DF_input))
      {
        warning ("the provided object as input for the data is not a data frame, the function didn`t do anything")
        Out<-1
      }
      if (!("DATE_hour_year" %in% names(DF_input)))
      {
        warning ("The provided data frame did not contain the hour variable properly coded as 'DATE_hour_year', the function didn`t do anything")
        Out<-1
      }
      if (!("DATE_week_year" %in% names(DF_input)))
      {
        warning ("The provided data frame did not contain the week of the year variable properly coded as 'DATE_week_year', the function didn`t plot the weeks")
        Out_weeks<-1
      }
      if (!("DATE_month_year" %in% names(DF_input)))
      {
        warning ("The provided data frame did not contain the month of the year variable properly coded as 'DATE_month_year', the function didn`t plot the months")
        Out_months<-1
      }
    }
    
    
    ## Var_list_input
    {
      if (is.null(Var_list_input))
      {
        warning ("list of input variable names was not provided, the function didn`t do anything")
        Out<-1
      }
      if (!(is.vector(Var_list_input) & !is.list(Var_list_input)))
      {
        warning ("list of input variable names was not properly coded as a vector, the function didn`t do anything")
        Out<-1
      }
      if (length(Var_list_input)>2)
      {
        warning ("list of input variable names was longer than 2 items, the function only used first two items")
      }
      for (i in 1:length(Var_list_input))
      {
        if (!((Var_list_input)[i] %in% names(DF_input)))
        {
          warning (paste("input variable name", i, "did not match the list of variables in the data frame, the function didn`t use it"))
        }
      }
    }
    
    
    ## week_list_input
    {
      if (is.null(week_list_input))
      {
        warning ("list of input week list was not provided, the function didn`t plot the weeks")
        Out_weeks<-1
      }
      else if (!(is.vector(week_list_input) & !is.list(week_list_input)))
      {
        warning ("list of weeks was not properly coded as a vector, the function didn`t plot the weeks")
        Out_weeks<-1
      }
      else
      {
        for (i in 1:length(week_list_input))
        {
          if (!((week_list_input)[i] %in% DF_input$DATE_week_year))
          {
            warning (paste("item", i, "in the week list did not match the weeks available in the  data frame"))
          }
        }
      }
    }
    
    
    ## month_list_input
    {
      if (is.null(month_list_input))
      {
        warning ("list of input month list was not provided, the function didn`t plot the months")
        Out_months<-1
      }
      else if (!(is.vector(month_list_input) & !is.list(month_list_input)))
      {
        warning ("list of months was not properly coded as a vector, the function didn`t plot the weeks")
        Out_months<-1
      }
      else
      {
        for (i in 1:length(month_list_input))
        {
          if (!((month_list_input)[i] %in% DF_input$DATE_month_year))
          {
            warning (paste("item", i, "in the month list did not match the weeks available in the  data frame"))
          }
        }
      }
    }
    
  }
  
  # General inspection
  if (Out == 0)
  {
    head(Dat)
    names(Dat)
    summary(Dat)  
  }
  
  # Ploting
  attach(DF_input)
  if (Out == 0)
  {
    # Define variables to plot
    if (length(Var_list_input)==1)
    {
      varX        <- DATE_hour_year
      x_scale     <- c(summary(DATE_hour_year)[1], summary(DATE_hour_year)[6])
      varY        <- get(Var_list_input[1])
      y_scale     <- c(summary(varY)[1], summary(varY)[6])
      x_label     <- "hour of the year"
      y_label     <- Var_list_input[1]
    }
    else
    {
      varX        <- get(Var_list_input[1])
      x_scale     <- c(summary(varX)[1], summary(varX)[6])
      varY        <- get(Var_list_input[2])
      y_scale     <- c(summary(varY)[1], summary(varY)[6])
      x_label     <- Var_list_input[1]
      y_label     <- Var_list_input[2]
    }
    
    
    # Plot Full dataset
    plot (varX,
          varY,
          xlim=x_scale,
          ylim=y_scale,
          xlab=x_label,
          ylab=y_label,
          main="full year"
    )
    
    # Plot Weeks
    if (Out_weeks==0)
    {
      for (i in 1:length(week_list_input))
      {
        if (((week_list_input)[i] %in% DF_input$DATE_week_year))
        {
          if (length(Var_list_input)==1)
          {
            x_scale     <- c(summary(DATE_hour_year[DATE_week_year==week_list_input[i]])[1], summary(DATE_hour_year[DATE_week_year==week_list_input[i]])[6])
          }
          plot (varX[DATE_week_year==week_list_input[i]],
                varY[DATE_week_year==week_list_input[i]],
                xlim=x_scale,
                ylim=y_scale,
                xlab=x_label,
                ylab=y_label,
                main=paste("week",week_list_input[i])
          )
        }  
      }
    }
    
    # Months
    if (Out_months==0)
    {
      for (i in 1:length(month_list_input))
      {
        if (((month_list_input)[i] %in% DF_input$DATE_month_year))
        {
          if (length(Var_list_input)==1)
          {
            x_scale     <- c(summary(DATE_hour_year[DATE_month_year==month_list_input[i]])[1], summary(DATE_hour_year[DATE_month_year==month_list_input[i]])[6])
          }
          plot (varX[DATE_month_year==month_list_input[i]],
                varY[DATE_month_year==month_list_input[i]],
                xlim=x_scale,
                ylim=y_scale,
                xlab=x_label,
                ylab=y_label,
                main=paste("month", month_list_input[i])
          )
        }  
      }
    }
  }
  detach(DF_input)
  
}

RG_HeatMap <- function(DF_input)
{
  ###
  # This function performs a heatmap of a data set.  
  # Requires a data frame as an input, with at least 3 variables
  # if only 3 variables are present
  #   one heat map is ploted
  # if more than 3 variables are present
  #   as many as number of variables-2 heat maps are ploted
  ###
  
  library(ggplot2)
  library(hrbrthemes)
  
  
  # Chechking of inputs
  # Returns warnings
  {
    Out<-0          # if out=1, gets out of the functioh without doing nothing
    
    ## DF_input
    {
      if (!is.data.frame(DF_input))
      {
        warning ("the provided object as input for the data is not a data frame, the function didn`t do anything")
        Out<-1
      }
      if(dim(DF_input)[2]<3)
      {
        warning ("the provided object does not have sufficient variables (min=3), the function didn`t do anything")
        Out<-1
      }
      for (i in 1:dim(DF_input)[2])
      {
        if(!(is.numeric(DF_input[,i])))
        {
          warning (paste("the data provided in col", i," is not numeric, the function didn`t do anything"))
          Out<-1  
        }
      }
    }
  }
  
  # Ploting
  if (Out==0)
  {
    headers<-  names(DF_input)
    for (i in 3:dim(DF_input)[2])
    {
      print(ggplot(DF_input,
                   aes(get(headers[1]),
                       get(headers[2]),
                       fill= get(headers[i])
                   )
      ) +
        geom_tile() +
        scale_fill_gradient(low="blue", high="red", name=paste(headers[i])) +
        scale_x_continuous(name=paste(headers[1])) +
        scale_y_continuous(name=paste(headers[2])) +
        theme_ipsum()
      )
    }
  }
}