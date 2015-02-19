Convert_discardLF_to_ss3_format_Fn <-
function(Length_Frequency_Table, LengthBins, season=1, fleet=NA, gender=0, partition=1, CheckForErrors=TRUE){
  # Years to generate
  Year_Set = sort(unique(Length_Frequency_Table[,'Year']))

  # Sanity check variable
  RowUsedTF = rep(FALSE, nrow(Length_Frequency_Table))
  
  # Define output list
  Matrix = matrix( 0, nrow=length(Year_Set), ncol=6+2*length(LengthBins), dimnames=list(NULL,c("Year","Season","Fleet","Gender","Partition","Nsamp",paste0("F_",LengthBins),paste0("M_",LengthBins))) )
  DF = data.frame( Matrix )
  
  # Counter for warnings
  ExcludedRows = NULL

  # Loop across years
  for(yI in 1:length(Year_Set)){
    # Add headers for the row
    DF[yI,c("Year","Season","Fleet","Gender","Partition")] = c(Year_Set[yI],season,fleet,gender,partition)
    for(lI in 1:length(LengthBins)){
      # Scan Length_Frequency_Table for matching row, and load in data
      RowNum = which(Length_Frequency_Table[,'Year']==Year_Set[yI] & Length_Frequency_Table[,'Lenbin']==LengthBins[lI])
      # Sanity check
      if( CheckForErrors==TRUE & length(RowNum)>=2 ) stop("Check Length_Frequency_Table for duplicated rows")
      if( CheckForErrors==FALSE & length(RowNum)>=2 ) ExcludedRows = rbind(ExcludedRows, Length_Frequency_Table[RowNum,])
      # Add to formatted data
      if( length(RowNum)==1 ){
        DF[yI,paste0("F_",LengthBins[lI])] = Length_Frequency_Table[RowNum,'Prop.numbers']
        RowUsedTF[RowNum] = TRUE
      }
    }
  }

  # Display warnings
  if( CheckForErrors==FALSE & !is.null(ExcludedRows) ){
    warning( paste("Number of duplicated rows =",nrow(ExcludedRows)) )
  }
  
  # check for evidence of problems
  if( CheckForErrors==TRUE & any(RowUsedTF==FALSE) ){
    stop( paste("Rows",which(RowUsedTF==FALSE),"not used") )
  }
  if( CheckForErrors==TRUE & sum(DF[,paste0("F_",LengthBins)])!=sum(Length_Frequency_Table[,'Prop.numbers']) ){
    stop( "totals don't match" )
  }

  # Return list
  Return = list("DF"=DF, "RowUsedTF"=RowUsedTF, "ExcludedRows"=ExcludedRows)
  return(Return)
}
