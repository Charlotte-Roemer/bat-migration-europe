
library(tidyverse)

# Directory
DirName = "/mnt/beegfs/croemer/VigieChiro/SIG"

# Load Grids
Grid.list <- unique(list.files(DirName, full.names = TRUE, 
                               pattern='GI_FR_SysGrid_500m_de_cote_part*.', 
                               recursive = TRUE))

for (i in 1:length(Grid.list)){
  Grid = read_csv(Grid.list[i], show_col_types = FALSE)
  print(i)
  print(ncol(Grid))
  #columnnames = data.frame(names(Grid) = "ok")
  columnnames <- names(Grid) %>% 
    map_dfc(setNames, object = list(character(1))) %>% 
    as.data.frame()
  columnnames[nrow(columnnames)+1,] <- "ok"
    
  if(exists("GridTot")){
    GridTot = bind_rows(Grid, GridTot)
    columnnamesTot = bind_rows(columnnamesTot, columnnames)
  }else{
    GridTot = Grid
    columnnamesTot = columnnames
  }
}

print(columnnamesTot)
write_delim(columnnamesTot,  paste0(DirName, "/", "columnnamesTot.csv"), delim = ";")
write_delim(GridTot,  paste0(DirName, "/", "SysGrid_500m_de_cote_FULL_with_habitat.csv"), delim = ";")






