# Selects points randomly in the dataset and puts them in the new dataset A (test) or B (train)
# To do this, it checks whether the selected point is in a buffer of max_dist from B
# If yes it puts it in B, else in A

# It then checks whether the selected point is close to points already put in A or B
# And puts it in the dataset where it is closer to
# Some points are close to A and to B and will not be put in A nor in B
# The process is repeated "reps" times and then the biggest dataset (A+B) is chosen

# foo = dataset with columns x and y for coordinates (in L93)
# buffer = distance threshold between points (use the same unit as the coordinates)
# reps = repetitions of process x times to choose biggest dataset among x trials

buffer.f <- function(foo, buffer, max_dist, reps){
  
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
      
      if(nrow(B) >0){
        if(nrow(A)>0){
          # If point is close to B points then goes to B
          if(TRUE %in%  (sqrt((Origin$x-outcoord$x)^2 + (Origin$y-outcoord$y)^2)<max_dist)){
            B = rbind(B,foo[which(rownames(foo)==outpoint),])
            # If point is close to B points but far from A points, then goes to B
          }else{
            A = rbind(A,foo[which(rownames(foo)==outpoint),])
          }
        }else{
          A = rbind(A,foo[which(rownames(foo)==outpoint),])
        }
      }else{
        B = rbind(B,foo[which(rownames(foo)==outpoint),])
        Origin = rbind(B,foo[which(rownames(foo)==outpoint),])
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




