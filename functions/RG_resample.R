RG_resample<- function (DF_input, sampling_flag_input=c(NA), list_vars_input=c(NA),list_funs_input=c(NA))
{
  ###
  # This function resamples the data to typical timestamps (day, week, month)
  # The timestamp is kept at the initial value
  # The other time marks are recalculated
  # The 
  # The summary is calculated for each of the segments
  # This summary is ploted in a boxplot & returned as a data frame
  #
  ###
  
  # Checking of inputs
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
      if(dim(DF_input)[2]<2)
      {
        warning ("the provided object does not have sufficient variables (min=2), the function didn`t do anything")
        Out<-1
      }
      if(!("DATE" %in% names(DF_input)))
      {
        warning ("the provided object does not have a properly labelled 'date' variable, the function didn`t do anything")
        Out<-1
      }
      else if (!is.POSIXct(DF_input$DATE))
      {
        warning ("the 'date' variable is not properly coded as POSIXct, the function didn`t do anything")
        Out<-1
      }
    }
    
    ## sampling_flag_input
    {
      if (is.na(sampling_flag_input))
      {
        warning ("No variable was provided as sampling flag, the function didn`t do anything")
        Out<-1
      }
      else if (!(sampling_flag_input %in% c("DATE_day_year", "DATE_week_year", "DATE_month_year")))
      {
        warning ("The sampling flag provided is not within the acceptable options ('day', 'week', 'month'), the function didn`t do anything")
        Out<-1 
      }
    }
    
    ## list_vars_input
    {
      if (!(sum(is.na(list_vars_input))<length(list_vars_input)))
      {
        warning ("No variable was provided as object to be inspected, the function didn`t do anything")
        Out<-1
      }
      # Verify if variable  names are within the dataframe
      else if (Out==0)
      {
        for (i in 1: length(list_vars_input))
        {
          if (!(list_vars_input[i] %in% names(DF_input)))
          {
            warning ("The variable provided as object to be inspected does not match any name in the dataframe, the function didn`t do anything")
            Out<-1
          }
        }
      }
      # Verify if variable is numeric
      else if (!is.numeric(DF_input[,which(Var_inspection_input==names(DF_input))]))
      {
        warning ("The variable provided as object to be inspected is not numeric, the function didn`t do anything")
        Out<-1
      }
    }
    
    # list_funs_input
    {
      if (!(sum(is.na(list_funs_input))<length(list_funs_input)))
      {
        warning ("No function list was provided for the segmentation, the function didn`t do anything")
        Out<-1
      }
      # Verify if functions are within possible options
      else
      {
        for (i in 1: length(list_funs_input))
        {
          if (!(sum(list_funs_input %in% c("max", "min", "mean", "sum"))==length(list_funs_input)))
          {
            warning ("The list of functions contains some not-accepted options ('max', 'min', 'mean', 'sum'), the function didn`t do anything")
            Out<-1 
          }
        }  
      }
    }
  }
  
  # Calculate all date stamps (just to ensure compatibility with other files)
  DF_input<-RG_Date_Marks (DF_input,
                           "DATE",
                           names(DF_input)[!names(DF_input) %in% c("DATE", "DATE_year", "DATE_month_year","DATE_week_year","DATE_day_year","DATE_day_month", "DATE_day_week", "DATE_hour_year", "DATE_hour_week", "DATE_hour_day", "DATE_weekday", "DATE_holiday" )])
  
  
  # manipulate date to adapt it to the new sampling period
  {
    if (sampling_flag_input=="DATE_day_year")
    {
      DF_input$DATE<-as.POSIXct(format(DF_input$DATE, format="%Y-%m-%d"))
    }
    else if (sampling_flag_input=="DATE_week_year")
    {
      DF_input$DATE<-as.POSIXct(cut(DF_input$DATE, "week"))
    }
    else if (sampling_flag_input=="DATE_month_year")
    {
      DF_input$DATE<-as.POSIXct(paste(format(DF_input$DATE, format="%Y-%m"),"-01",sep=""))
    }
  }
  
  
  # perform the operations
  attach(DF_input)
  DF_output<-as.data.frame(as.POSIXct(tapply(DATE, DATE, min),origin = '1970-01-01'))
  names(DF_output)<-"DATE"
  for (i in 1:length(list_vars_input))
  {
    a<-as.data.frame(tapply(get(list_vars_input[i]), DATE, list_funs_input[i]))
    names(a)<-list_vars_input[i]
    DF_output<-cbind (DF_output, a)
  }
  detach(DF_input)
  
  # define a tidy dataset
  # Calculate all date stamps (just to ensure compatibility with other files)
  DF_output<-RG_Date_Marks (DF_output,
                           "DATE",
                           names(DF_output)[!names(DF_output) %in% c("DATE", "DATE_year", "DATE_month_year","DATE_week_year","DATE_day_year","DATE_day_month", "DATE_day_week", "DATE_hour_year", "DATE_hour_week", "DATE_hour_day", "DATE_weekday", "DATE_holiday" )])
  
  
  
   rownames(DF_output) <- c()
  
  # return it
  return(DF_output)
}