RG_Summary <- function (DF_input, Var_inspection_input=NA, Var_segmentation_input=NA)
{
  ###
  # This function segments a variable from a dataset given a segmentation variable
  # The summary is calculated for each of the segments
  # This summary is ploted in a boxplot & returned as a data frame
  #
  ###
  
  
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
      if(dim(DF_input)[2]<2)
      {
        warning ("the provided object does not have sufficient variables (min=2), the function didn`t do anything")
        Out<-1
      }
    }
    
    ## Var_inspection_input
    {
      if (is.na(Var_inspection_input))
      {
        warning ("No variable was provided as object to be inspected, the function didn`t do anything")
        Out<-1
      }
      # Verify if variable is within the dataframe
      if (Out==0)
      {
        if (!(Var_inspection_input %in% names(DF_input)))
        {
          warning ("The variable provided as object to be inspected does not match any name in the dataframe, the function didn`t do anything")
          Out<-1
        }
      }
      # Verify if variable is numeric
      if (!is.numeric(DF_input[,which(Var_inspection_input==names(DF_input))]))
      {
        warning ("The variable provided as object to be inspected is not numeric, the function didn`t do anything")
        Out<-1
      }
    }
    
    # Var_segmentation_input
    {
      if (is.na(Var_segmentation_input))
      {
        warning ("No variable was provided as index for segmentation, the function didn`t do anything")
        Out<-1
      }
      # Verify if variable is within the dataframe
      if (Out==0)
      {
        if (!(Var_segmentation_input %in% names(DF_input)))
        {
          warning ("The variable provided as index for segmentation does not match any name in the dataframe, the function didn`t do anything")
          Out<-1
        }
      } 
      # else # Verify if variable is numeric
    }
  }
  
  # Execution
  if (Out==0)
  {
    attach (DF_input)
    output<-as.data.frame(as.vector(summary(get(Var_inspection_input))))
    row.names(output)<-names(summary(get(Var_inspection_input)))
    
    output_segement<-tapply(get(Var_inspection_input), get(Var_segmentation_input), summary)
    
    for (i in 1:length(output_segement))
    {
      a<-as.data.frame(as.vector(output_segement[[i]]))
      output<-cbind(output, a)
    }
    names(output)<-c("total",sort(unique(get(Var_segmentation_input))))
    
    # #reorder data
    # output<-output[,c(1,order(names(output[2:length(names(output))]))+1)]  
    
    boxplot(output,
            xlab=Var_segmentation_input,
            ylab=Var_inspection_input
    )
    
    
    detach (DF_input)
    
    return (output)
  }
}