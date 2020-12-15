input <- c(18,11,9,0,5,1)

occs <- vector("list", length=length(input))
names(occs) <- input
for (i in 1:length(input)){
  occs[[i]] <- i
}

prevval <- input[length(input)]

for (i in as.integer((length(input)+1):30000000)){

  prevtimes <- occs[[as.character(prevval)]]
  
  if (length(prevtimes)>1){
    prevval <- as.integer(abs(diff(prevtimes)))
    if (is.null(occs[[as.character(prevval)]])){
      occs[[as.character(prevval)]] <- i
    } else{
      occs[[as.character(prevval)]] <- c(occs[[as.character(prevval)]][length(occs[[as.character(prevval)]])], i)  
    }
    
    #next val
  } else{
    prevval <- 0
    occs[["0"]] <- c(occs[["0"]][length(occs[["0"]])], i)
  }
}

# 
prevval
