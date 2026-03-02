
# Runs either extr_PF_DataLP or extr_PF_ActNuit
# extr_PF_DataLP adds metadata associated with the participation (location, ...). Merges tables to create DataLP.
# extr_PF_ActNuit summarises bat activity per night

.libPaths(libpathR)
library(data.table)

# Which tables do you want to process?
What = "Count" # "Merge" to merge export tables with other variables, or "Count" to count bat passes/night

if (What == "Count"){
  EPA="/pbs/home/c/croemer/Scripts/extr_PF_ActNuit_BMRE.R" # Script using DataLP to count number of bat passes/night
}
if (What == "Merge"){
  EPA="/pbs/home/c/croemer/Scripts/extr_PF_DataLP.R" # Script merging tables to create DataLP
}

PP=c("55","56","57","58","59","5a","5b","5c","5d","5e","5f","60","61","62","63","64","65","66","67") # Groups of participations (prefix)
#PP=c("60","61","62","63","64","65","66","67")

Threshold="0" #score threshold
RandomResample=T # should raw files be resampled with weight on probability? (will be done during "Count")
if(RandomResample==T & Threshold != 0){
  stop("Incoherent choices for RandomResample")
}

args=""
args[10]="/sps/mnhn/vigiechiro/vigiechiro-prod-datastore/TempBMRE/VIGIE-CHIRO/Raw" # where raw files are stored
args[12]=""
args[13]=""
args[14]=""
args[15]=T #sort out doubtful data (probable hardware problems)
args[16]="mnt/VigieChiro/export_validtot210408.txt" # not useful here
args[17]=F #correct for validation or not
args[18] = "/sps/mnhn/vigiechiro/vigiechiro-prod-datastore/TempBMRE/VIGIE-CHIRO/Raw" # Where should results be written?
#args[18] = paste0("/sps/mnhn/vigiechiro/vigiechiro-prod-datastore/TempBMRE/VIGIE-CHIRO/SpNuit2_", Threshold) # Where should results be written?
FRaw="none"

for (i in 1:length(PP)) # Process for each group of participations
{
  print(paste(i,Sys.time()))
  args[1]="PrefPart"
  
  for (j in c(c(0:9),"a","b","c","d","e","f")) # For each subgroup of participations
  {
    args[2]=paste0(PP[i],j)
    print(args[2])
    if (What == "Merge"){
      args[4]=paste0(args[10],"/export_",args[2],".csv") #name of file to read
    }else{
      args[4]=paste0(args[10],"/DataLP_PF_export_",args[2],".csv") #name of file to read
    }
    if(file.exists(args[4])) 
    {
      # Launch script to Merge or to Count
      source(EPA) # 1e6 donnees/min 
      Sys.time()
    }
  }
}

#PrefDoubt=ifelse(args[15]==T, "", "DI")

# Bind all results in one table
if (RandomResample){
  Pattern=basename(paste0(args[18],"/SpNuit2_", PrefDoubt,"__","weighted_DataLP_PF_export"))
  
  SpToAgg=list.files(args[18],pattern=Pattern,full.names=T)
  
  my.data=list()
  for (k in 1:length(SpToAgg))
  {
    my.data[[k]]=fread(SpToAgg[k])
  }
  ActTot=rbindlist(my.data)
  fwrite(ActTot, paste0(args[18], "/SpNuit2_", PrefDoubt,"__", "weighted_DataLP_PF_exportTot.csv"))
}else{
  Pattern=basename(paste0(args[18], "/SpNuit2_", PrefDoubt,"__", Threshold, "_DataLP_PF_export"))
  
  SpToAgg=list.files(args[18], pattern=Pattern, full.names=T)
  
  my.data=list()
  for (k in 1:length(SpToAgg))
  {
    my.data[[k]]=fread(SpToAgg[k])
  }
  ActTot=rbindlist(my.data)
  fwrite(ActTot,paste0(args[18], "/SpNuit2_", PrefDoubt,"__", Threshold, "_DataLP_PF_exportTot.csv")) 
}



# test=data.frame(a=1)
# fwrite(test,paste0(args[10], "log/MAJ_VC/Succeed_SN50",Sys.Date(),".csv"))








