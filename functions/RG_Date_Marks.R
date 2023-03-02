RG_Date_Marks <-function (DF_input,STRING_date,VEC_vars)
{
  ###
  # This function redefines the data frame to a standard format
  # input 1. Data Frame
  # input 2. name of the column with a posixct timestam
  # input 3. vector of columns to with data
  #
  # output. Data Frame
  # format.
  ## rows: observations
  ## cols:
  ##  DATE (Posixct)
  ##  DATE_year
  ##  DATE_month_year
  ##  DATE_week_year
  ##  DATE_day_year
  ##  DATE_day_month
  ##  DATE_day_week
  ##  DATE_hour_year
  ##  DATE_hour_week
  ##  DATE_hour_day
  ##  DATE_weekday
  ##  DATE_holiday
  ##  Var 1
  ##  Var 2
  ##  Var etc
  ###
  
  library(lubridate)
  
  
  Date<-DF_input[,names(DF_input) %in% STRING_date]
  
  
  ##  DATE (Posixct)
  DF_output <- as.data.frame(Date)
  names(DF_output)[1]<-"DATE"
  
  ##  DATE_year
  DF_output$DATE_year <- year(DF_output$DATE)
  
  ##  DATE_month_year
  DF_output$DATE_month_year <- month(DF_output$DATE)
  
  ##  DATE_week_year
  DF_output$DATE_week_year <- as.numeric(strftime(DF_output$DATE,format="%W"))
  
  ##  DATE_day_year
  DF_output$DATE_day_year <- yday(DF_output$DATE)
  
  ##  DATE_day_month
  DF_output$DATE_day_month <- day(DF_output$DATE)
  
  ##  DATE_day_week
  DF_output$DATE_day_week <- wday(DF_output$DATE)
  
  ##  DATE_hour_year
  DF_output$DATE_hour_year <- (DF_output$DATE_day_year-1)*24+hour(DF_output$DATE)
  
  ##  DATE_hour_week
  DF_output$DATE_hour_week <- hour(DF_output$DATE)+(DF_output$DATE_day_week-1)*24
  
  ##  DATE_hour_day
  DF_output$DATE_hour_day <- hour(DF_output$DATE)
  
  ##  DATE_weekday
  DF_output$DATE_weekday <- DF_output$DATE_day_week %in% 2:6

  ##  Var 1
  ##  Var 2
  ##  Var etc

  dim_vars <- length(VEC_vars)
  for (i in 1:dim_vars){
    var<-VEC_vars[i]
    attach(DF_input)
    DF_output[paste(var)] <- get(var)
    detach(DF_input)
  }
  return (DF_output)
}