RG_Repair_1h_double_side <-function (DF_input,STRING_date,VEC_vars, VEC_repair)
{
  ###
  # This function repairs 1h voids and outliers in the data frame
  # given that:
  # only 1h void
  # data from previous and following week is available for the preceding and following hours
  # input 1. Data Frame
  # input 2. name of the column with a posixct timestam
  # input 3. vector of columns to with data
  #
  # output. Data Frame
  ###
  
  Dat<-RG_Date_Marks (DF_input,
                      STRING_date,
                      VEC_vars)
  
  # Identification of missing data (& both missing or oulier data)
  {
    Dat$IS_Missing <- is.na(Dat$Temperature)+is.na(Dat$Solar.Irradiation)+is.na(Dat$Power)>0
    Dat$IS_Outlier[is.na(Dat$IS_Outlier)]<-TRUE
    
    Dat$IS_Missing_Outlier<- (Dat$IS_Missing + Dat$IS_Outlier)>0
    if ("IS_Repaired" %in% names(Dat))
    {
      Dat$IS_Repair_Pending<- as.logical(Dat$IS_Missing_Outlier * !Dat$IS_Repaired)  
    } else
    {
      Dat$IS_Repair_Pending<- Dat$IS_Missing_Outlier
    }
  }

  # repair of 1h voids with data available for interpollation
  ## Identification of missing spots
  {
    current_post1 <-filter(Dat$IS_Repair_Pending, c(0,1), sides=1)
    current_pre1  <-filter(Dat$IS_Repair_Pending, c(1,0), sides=2)
    
    Dat$IS_Repair_Pending_single<-Dat$IS_Repair_Pending*(!current_post1)*(!current_pre1)
    Dat$IS_Repair_Pending_single[is.na(Dat$IS_Repair_Pending_single)]<-0  
    
    Dat$IS_Repair_Pending_single<- as.logical(Dat$IS_Repair_Pending_single)
    
    rm(current_post1, current_pre1)
  }
  
  ## Revision of data for previous & following week
  ### Date marks
  {
    Dat$Date_currweek_prev_hour <- Dat$DATE-1*60*60
    Dat$Date_currweek_post_hour <- Dat$DATE+1*60*60
    
    Dat$Date_prevweek           <- Dat$DATE-1*60*60*24*7
    Dat$Date_prevweek_prev_hour <- Dat$Date_prevweek-1*60*60
    Dat$Date_prevweek_post_hour <- Dat$Date_prevweek+1*60*60
    
    Dat$Date_postweek           <- Dat$DATE+1*60*60*24*7
    Dat$Date_postweek_prev_hour <- Dat$Date_postweek-1*60*60
    Dat$Date_postweek_post_hour <- Dat$Date_postweek+1*60*60  
  }
  
  ### Are date marks available?
  {
    Dat$IS_1h_interpol_possible <- (Dat$Date_currweek_prev_hour %in% Dat$DATE[!Dat$IS_Repair_Pending]) *
      (Dat$Date_currweek_post_hour %in% Dat$DATE[!Dat$IS_Repair_Pending]) *
      (Dat$Date_prevweek %in% Dat$DATE[!Dat$IS_Repair_Pending])           *
      (Dat$Date_prevweek_prev_hour %in% Dat$DATE[!Dat$IS_Repair_Pending]) *
      (Dat$Date_prevweek_post_hour %in% Dat$DATE[!Dat$IS_Repair_Pending]) * 
      (Dat$Date_postweek %in% Dat$DATE[!Dat$IS_Repair_Pending])           *
      (Dat$Date_postweek_prev_hour %in% Dat$DATE[!Dat$IS_Repair_Pending]) *
      (Dat$Date_postweek_post_hour %in% Dat$DATE[!Dat$IS_Repair_Pending])
    
    Dat$IS_1h_interpol_possible <- as.logical(Dat$IS_1h_interpol_possible)  
  }
  
  ### Get smaller subset
  {
    Dat_sbs <- Dat[Dat$IS_1h_interpol_possible,]
    Dat_sbs <- Dat_sbs[Dat_sbs$IS_Repair_Pending_single,]
  }

  ## Linear interpolation
  {
    Dat_sbs_vardata_currweek           <- as.data.frame(Dat[Dat$DATE %in% Dat_sbs$DATE,
                                                            names(Dat) %in% VEC_repair])
                                          names(Dat_sbs_vardata_currweek)<-VEC_repair
    
    Dat_sbs_vardata_currweek_prev_hour <- as.data.frame(Dat[Dat$DATE %in% Dat_sbs$Date_currweek_prev_hour,
                                                            names(Dat) %in% VEC_repair])
                                          names(Dat_sbs_vardata_currweek_prev_hour)<-VEC_repair
    
    Dat_sbs_vardata_currweek_post_hour <- as.data.frame(Dat[Dat$DATE %in% Dat_sbs$Date_currweek_post_hour,
                                                            names(Dat) %in% VEC_repair])
                                          names(Dat_sbs_vardata_currweek_post_hour)<-VEC_repair
    
    Dat_sbs_vardata_prevweek           <- as.data.frame(Dat[Dat$DATE %in% Dat_sbs$Date_prevweek,
                                                            names(Dat) %in% VEC_repair])
                                          names(Dat_sbs_vardata_prevweek)<-VEC_repair
    
    Dat_sbs_vardata_prevweek_prev_hour <- as.data.frame(Dat[Dat$DATE %in% Dat_sbs$Date_prevweek_prev_hour,
                                                            names(Dat) %in% VEC_repair])
                                          names(Dat_sbs_vardata_prevweek_prev_hour)<-VEC_repair
    
    Dat_sbs_vardata_prevweek_post_hour <- as.data.frame(Dat[Dat$DATE %in% Dat_sbs$Date_prevweek_post_hour,
                                                            names(Dat) %in% VEC_repair])
                                          names(Dat_sbs_vardata_prevweek_post_hour)<-VEC_repair
    
    Dat_sbs_vardata_postweek           <- as.data.frame(Dat[Dat$DATE %in% Dat_sbs$Date_postweek,
                                                            names(Dat) %in% VEC_repair])
                                          names(Dat_sbs_vardata_postweek)<-VEC_repair
    
    Dat_sbs_vardata_postweek_prev_hour <- as.data.frame(Dat[Dat$DATE %in% Dat_sbs$Date_postweek_prev_hour,
                                                            names(Dat) %in% VEC_repair])
                                          names(Dat_sbs_vardata_postweek_prev_hour)<-VEC_repair
    
    Dat_sbs_vardata_postweek_post_hour <- as.data.frame(Dat[Dat$DATE %in% Dat_sbs$Date_postweek_post_hour,
                                                            names(Dat) %in% VEC_repair])
                                          names(Dat_sbs_vardata_postweek_post_hour)<-VEC_repair
    
    Dat_sbs_vardata_currweek <- ((Dat_sbs_vardata_prevweek + Dat_sbs_vardata_postweek)/2) +
                                ((Dat_sbs_vardata_currweek_prev_hour + Dat_sbs_vardata_currweek_post_hour)/2) -
                                ((Dat_sbs_vardata_prevweek_prev_hour + Dat_sbs_vardata_prevweek_post_hour +
                                  Dat_sbs_vardata_postweek_prev_hour + Dat_sbs_vardata_postweek_post_hour)/4)
    
    Dat_sbs <- cbind(Dat_sbs$DATE,Dat_sbs_vardata_currweek)  

    VEC_repair_corrected <-paste(VEC_repair, "_corrected" , sep="")
    
    names(Dat_sbs) <- c("DATE", VEC_repair_corrected)
  }
  
  # Writing the output data frame
  {
    
    VEC_names_Dat<-names(Dat)
    
    if (sum(names(Dat) %in% VEC_repair_corrected)>0)
    {
      Dat_cols_VEC_repair<-as.data.frame(Dat[, names(Dat) %in% VEC_repair_corrected])
      names(Dat_cols_VEC_repair)<-VEC_repair_corrected  
    } else
    {
      Dat_cols_VEC_repair<-as.data.frame(Dat[, names(Dat) %in% VEC_repair])
      names(Dat_cols_VEC_repair)<-VEC_repair_corrected  
    }
    
    Dat<-cbind(Dat, Dat_cols_VEC_repair)
    
    Dat[Dat$DATE %in% Dat_sbs$DATE, names(Dat) %in% VEC_repair_corrected]<-Dat_sbs_vardata_currweek
    
    
    if ("IS_Repaired" %in% names(Dat))
    {
      # Dat$IS_Repaired<-as.logical(pmin((as.numeric(Dat$IS_Repaired) + as.numeric(Dat$IS_Repair_Pending * Dat$IS_1h_interpol_possible)), 1))
      Dat$IS_Repaired<- Dat$IS_Repaired | (Dat$IS_Repair_Pending & Dat$IS_1h_interpol_possible)
    } else
    {
      Dat$IS_Repaired<-as.logical(Dat$IS_Repair_Pending * Dat$IS_1h_interpol_possible)  
    }
    
    varnames_out <- c(STRING_date,
                      "DATE_year", "DATE_month_year", "DATE_week_year", "DATE_day_year",
                      "DATE_day_month", "DATE_day_week", "DATE_hour_year", "DATE_hour_week",
                      "DATE_hour_day", "DATE_weekday",
                      VEC_vars,
                      "IS_Missing", "IS_Missing_Outlier", "IS_Repaired",
                      VEC_repair_corrected)
    
    Dat<-Dat[, names(Dat) %in% varnames_out]
    
    return (Dat)
  }
}
