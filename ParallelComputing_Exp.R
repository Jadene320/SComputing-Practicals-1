aComplexFunction <- function(x) {
  Sys.sleep(1)
  return(sqrt(x))
}  
system.time({
  result <- lapply(1:5, aComplexFunction)
})

#Now try the same thing using foreach
library(foreach)
system.time({
  result <- foreach(i = 1:5, .combine = c) %do% {
    aComplexFunction(i)
  }
})


#####Parallel Execution#####
library(doParallel)
cl <- makeCluster(3) #creates 3 clusters 

registerDoParallel(cl)

#Do the exact samw task but now in parallel
system.time({
  result <- foreach(i = 1:5, .combine = c) %dopar% {
    aComplexFunction(i)
  }
})
##Look at elapsed times. Parallel is way better.
stopCluster(cl)


#What if each task requires the data? it needs to copy the data, which could be long and inefficient
some_data_which_is_shared <- numeric(6L)
my_data <- some_data_which_is_shared

cl <- makeCluster(2)
clusterExport(cl, "my_data")  # Copying data to workers
stopCluster(cl)

system.time({
  cl <- makeCluster(2)
  stopCluster(cl)
})

#Setting seeds in parallel doesn't work, use irnorm for that
counter <- 0  # Global variable
hi <- function(i) {
  counter <<- counter + 1  # This modification won't work properly
  return(counter)
}

cl <- makeCluster(3)
registerDoParallel(cl)

#each cluster has a different workflow, you'd assume your results are numbers from 2-11
result <- foreach(i = 1:10, .combine = c) %dopar%{
  hi(i)
}

print(result)

print(counter)
