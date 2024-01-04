
library(tidyverse)

Directory = "C:/Users/croemer01/Documents/Donnees vigie-chiro/"
Threshold = "weighted"

# Load bat ID
ID_table = read_delim(paste0(Directory, "SpNuit2_DI_", Threshold, "_DataLP_PF_exportTot.csv"))

# Load Mic diagnostic
Mic = read_delim(paste0(Directory, "DMTot.csv")) %>% 
  rename ("participation" = Group.1, "num_micro" = Group.2)

# Join and sort out bad mics
ID_table_Mic = left_join(ID_table, Mic) %>% 
  filter(probleme_micro == "non") %>% 
  select(-probleme_micro, -x)

# Save
write_csv(ID_table_Mic, paste0(Directory, "SpNuit2_", Threshold, "_DataLP_PF_exportTot.csv"))

