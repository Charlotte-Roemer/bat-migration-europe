my_combine <- function (...) 
{
  pad0 <- function(x, len) c(x, rep(0, len - length(x)))
  padm0 <- function(x, len) rbind(x, matrix(0, nrow = len - 
                                              nrow(x), ncol = ncol(x)))
  rflist <- list(...)
  areForest <- sapply(rflist, function(x) inherits(x, "randomForest"))
  if (any(!areForest)) 
    stop("Argument must be a list of randomForest objects")
  rf <- rflist[[1]]
  classRF <- rf$type == "classification"
  trees <- sapply(rflist, function(x) x$ntree)
  ntree <- sum(trees)
  rf$ntree <- ntree
  nforest <- length(rflist)
  haveTest <- !any(sapply(rflist, function(x) is.null(x$test)))
  vlist <- lapply(rflist, function(x) rownames(importance(x)))
  numvars <- sapply(vlist, length)
  if (!all(numvars[1] == numvars[-1])) 
    stop("Unequal number of predictor variables in the randomForest objects.")
  for (i in seq_along(vlist)) {
    if (!all(vlist[[i]] == vlist[[1]])) 
      stop("Predictor variables are different in the randomForest objects.")
  }
  haveForest <- sapply(rflist, function(x) !is.null(x$forest))
  if (all(haveForest)) {
    nrnodes <- max(sapply(rflist, function(x) x$forest$nrnodes))
    rf$forest$nrnodes <- nrnodes
    rf$forest$ndbigtree <- unlist(sapply(rflist, function(x) x$forest$ndbigtree))
    rf$forest$nodestatus <- do.call("cbind", lapply(rflist, 
                                                    function(x) padm0(x$forest$nodestatus, nrnodes)))
    rf$forest$bestvar <- do.call("cbind", lapply(rflist, 
                                                 function(x) padm0(x$forest$bestvar, nrnodes)))
    rf$forest$xbestsplit <- do.call("cbind", lapply(rflist, 
                                                    function(x) padm0(x$forest$xbestsplit, nrnodes)))
    rf$forest$nodepred <- do.call("cbind", lapply(rflist, 
                                                  function(x) padm0(x$forest$nodepred, nrnodes)))
    tree.dim <- dim(rf$forest$treemap)
    if (classRF) {
      rf$forest$treemap <- array(unlist(lapply(rflist, 
                                               function(x) apply(x$forest$treemap, 2:3, pad0, 
                                                                 nrnodes))), c(nrnodes, 2, ntree))
    }
    else {
      rf$forest$leftDaughter <- do.call("cbind", lapply(rflist, 
                                                        function(x) padm0(x$forest$leftDaughter, nrnodes)))
      rf$forest$rightDaughter <- do.call("cbind", lapply(rflist, 
                                                         function(x) padm0(x$forest$rightDaughter, nrnodes)))
    }
    rf$forest$ntree <- ntree
    if (classRF) 
      rf$forest$cutoff <- rflist[[1]]$forest$cutoff
  }
  else {
    rf$forest <- NULL
  }
  #
  #Tons of stuff removed here...
  #
  if (classRF) {
    rf$confusion <- NULL
    rf$err.rate <- NULL
    if (haveTest) {
      rf$test$confusion <- NULL
      rf$err.rate <- NULL
    }
  }
  else {
    rf$mse <- rf$rsq <- NULL
    if (haveTest) 
      rf$test$mse <- rf$test$rsq <- NULL
  }
  rf
}