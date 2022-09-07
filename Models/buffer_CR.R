# Selects points randomly in the dataset and puts them in the new dataset A or B
# To do this, it checks whether the selected point is close to points already put in A or B
# And puts it in the dataset where it closer to
# Some points are close to A and to B and will not be put in A nor in B
# The process is repeated "reps" times and then the biggest dataset (A+B) is chosen

# foo = dataset with columns x and y for coordinates (in L93)
# buffer = distance threshold between points (use the same unit as the coordinates)
# reps = repetitions of process x times to choose biggest dataset among x trials

buffer.f <- function(foo, buffer, reps){
  
  print(paste0 ("Sampling random points to create train and test datasets:"))
                
  # Make list of suitable tables
  suitable = list()
  for (k in 1:reps){
    
    print(paste0 (k, " out of ", reps, " iterations"))
    
    # Prepare output tables
    A <- data.frame(matrix(ncol = ncol(foo), nrow = 0))
    B <- data.frame(matrix(ncol = ncol(foo), nrow = 0))
    colnames(A) = colnames(foo)
    colnames(B) = colnames(foo)
    outdata = data.frame()
    
    # Set the rows to sample from
    for(i in 1:nrow(foo)){
      if(i>1){
        rowsleft <- (1:nrow(foo))[-c(as.numeric(rownames(A)), as.numeric(rownames(B)))]
      } else {
        rowsleft <- 1:nrow(foo)
      }
      
      # Randomly select point
      outpoint <- as.numeric(sample(as.character(rowsleft),1))
      outcoord <- foo[outpoint,c("x","y")]
      
      # TEST=rbind (TEST, outpoint)
      
      if(nrow(A) >0){
        if(nrow(B)>0){
          # If point is close to A points but far from B points, then goes to A
          if(TRUE %in%  (sqrt((A$x-outcoord$x)^2 + (A$y-outcoord$y)^2)<buffer) &
             !TRUE %in%  (sqrt((B$x-outcoord$x)^2 + (B$y-outcoord$y)^2)<buffer)){
            A = rbind(A,foo[which(rownames(foo)==outpoint),])
            # If point is close to B points but far from A points, then goes to B
          }else if(TRUE %in%  (sqrt((B$x-outcoord$x)^2 + (B$y-outcoord$y)^2)<buffer) &
                   !TRUE %in%  (sqrt((A$x-outcoord$x)^2 + (A$y-outcoord$y)^2)<buffer)){
            B = rbind(B,foo[which(rownames(foo)==outpoint),])
            # If point is far to A and B points ...
          }else if (!TRUE %in%  (sqrt((A$x-outcoord$x)^2 + (A$y-outcoord$y)^2)<buffer) &
                    !TRUE %in%  (sqrt((B$x-outcoord$x)^2 + (B$y-outcoord$y)^2)<buffer)){
            if(i %in% seq(1, nrow(foo), 3)){ # ... goes to A once every three cases to have approximately 33% A /66% B
              A = rbind(A,foo[which(rownames(foo)==outpoint),])
            }else{
              B = rbind(B,foo[which(rownames(foo)==outpoint),])
            }
          }
        }else{
          B = rbind(B,foo[which(rownames(foo)==outpoint),])
        }
      }else{
        A = rbind(A,foo[which(rownames(foo)==outpoint),])
      }
      
    }
    
    # Bind A and B
    A$type ="test"
    B$type ="train"
    outdata = rbind(A,B)
    
    # Populate the suitable points list
    suitable[[k]] <- outdata
  }
  
  # Go through the iterations and pick a list with the most data
  best <- suitable[[which.max(lapply(suitable, nrow))]]
  best
}




