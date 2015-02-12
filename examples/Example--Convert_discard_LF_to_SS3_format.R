
# Load package
devtools::install_github("nwfsc-assess/nwfscDiscards")
library(nwfscDiscards) # This should reflect the latest formal release

# Read data
data( Simulated_example_Length_Freq ) # Simulated numbers (not proportions)
LF = Simulated_example_Length_Freq
LengthBins = seq(12, 66, by=2)

# Loop across gears and states
for(gI in 1:length(unique(LF[,'Gear']))){
for(sI in 1:length(unique(LF[,'State']))){
  State = unique(LF[,'State'])[sI]
  Gear = unique(LF[,'Gear'])[gI]

  # Restrict to single fleet
  LF_sub = LF[which(LF[,'Gear']==Gear & LF[,'State']==State),]

  # Length_Frequency_Table=LF; Sample_Size_Table=SampSize; LengthBins=LengthBins; season=1; fleet=NA; gender=0; partition=1
  SS3 = Convert_discardLF_to_ss3_format_Fn( Length_Frequency_Table=LF_sub, LengthBins=LengthBins, season=1, fleet=NA, gender=0, partition=1 )

  # Sample sizes
  SS3[,'Nsamp'] =   # Add your own code for input sample sizes here
  
  # save as CSV
  write.csv( file=paste0("Discard_comps_SS3format_State=",State,"_Gear=",Gear,".csv"), row.names=FALSE)
}}
