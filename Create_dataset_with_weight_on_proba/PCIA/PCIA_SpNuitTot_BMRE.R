library(data.table)

#run extr_PF_DataLP (1) or extr_PF_ActNuit (2) ?
What = 2

if (What == 1){
  EPA="/trinity/home/croemer/scripts_VC/PCIA_extr_PF_DataLP.R"
  #EPA="C:/Users/croemer01/Documents/R/bat-migration-europe/Brouillons/Create_dataset_with_weight_on_proba/extr_PF_DataLP.R" # Script merging tables to create DataLP
}else{
  EPA="/trinity/home/croemer/scripts_VC/PCIA_extr_PF_ActNuit_BMRE.R" # Script counting number of bat passes/night
}

PP=c("55","56","57","58","59","5a","5b","5c","5d","5e","5f","60","61","62","63","64") # Groups of participations (prefix)

#PP=c("5b")

args=""
args[3]="0" #score threshold
args[10]="/mnt/beegfs/ybas/VigieChiro/Raw" # where raw files are stored
args[12]=""
args[13]=""
args[14]=""
args[15]=F #sort out doubtful data (probable hardware problems)
args[16]="mnt/VigieChiro/export_validtot210408.txt" # not useful here
args[17]=F #correct for validation or not
args[18] = "/mnt/beegfs/croemer/VigieChiro/Raw"
FRaw="none"  #if you want to run just one (see args[4]) in EPA script
RandomResample=T # should raw files be resampled with weight on probability?


for (i in 1:length(PP)) # Process for each group of participations
{
  print(paste(i,Sys.time()))
  args[1]="PrefPart"
  
  for (j in c(c(0:9),"a","b","c","d","e","f")) # For each subgroup of participations
  {
    args[2]=paste0(PP[i],j)
    print(args[2])
    if (What == 1){
      args[4]=paste0(args[10],"/export_",args[2],".csv") #name of file to read
    }else{
      args[4]=paste0(args[10],"/DataLP_PF_export_",args[2],".csv") #name of file to read
    }
    if(file.exists(args[4])) 
    {
      # Count number of bat passes/night
      source(EPA) # 1e6 donnees/min 
      Sys.time()
    }
  }
}


# Bind all results in one table
if (RandomResample){
  Pattern=basename(paste0(args[18],"/SpNuit2_DI__","weighted_DataLP_PF_export"))
  
  SpToAgg=list.files(args[18],pattern=Pattern,full.names=T)
  
  my.data=list()
  for (k in 1:length(SpToAgg))
  {
    my.data[[k]]=fread(SpToAgg[k])
  }
  ActTot=rbindlist(my.data)
  fwrite(ActTot,paste0(args[18],"/SpNuit2_DI_","weighted_DataLP_PF_exportTot.csv"))
}else{
  Pattern=basename(paste0(args[10],"/SpNuit2_DI__",args[3],"_DataLP_PF_export"))
  
  SpToAgg=list.files(args[10],pattern=Pattern,full.names=T)
  
  my.data=list()
  for (k in 1:length(SpToAgg))
  {
    my.data[[k]]=fread(SpToAgg[k])
  }
  ActTot=rbindlist(my.data)
  fwrite(ActTot,paste0(args[18],"/SpNuit2_DI_",args[3],"_DataLP_PF_exportTot.csv")) 
}



# test=data.frame(a=1)
# fwrite(test,paste0(args[10], "log/MAJ_VC/Succeed_SN50",Sys.Date(),".csv"))








