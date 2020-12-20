library(hash)


input <- c(18,11,9,0,5,1)

occs <- hash(values=1:length(input), keys=input)

prevval <- input[length(input)]

for (i in (length(input)+1):30000000){
  if (i %% 10000==0) print(i)
  prevtimes <- occs[[as.character(prevval)]]
  
  if (length(prevtimes)>1){
    prevval <- as.integer(abs(diff(prevtimes)))
    if (!has.key(as.character(prevval), occs)){
      occs[[as.character(prevval)]] <- i
    } else{
      occsi <- occs[[as.character(prevval)]]
      occs[[as.character(prevval)]] <- c(occsi[length(occsi)], i)  
    }
    
    #next val
  } else{
    prevval <- 0
    occs0 <- occs[["0"]]
    occs[["0"]] <- c(occs0[length(occs0)], i)
  }
}

# 
prevval
