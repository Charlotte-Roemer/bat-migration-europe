
library(tidyverse)

Directory = "/mnt/beegfs/croemer/VigieChiro/SequenceDuration/"

SeqLength = read_delim(paste0(Directory, "Sequence_length.csv"))

SeqLength2022 = subset(SeqLength, year(SeqLength$TempsDebutSequence) == 2022)

write_csv(SeqLength2022, paste0(Directory, "Sequence_length_2022only.csv"))

