input <- c(18,11,9,0,5,1)

ninit <- length(input)

out <- c(as.integer(input), rep(NA, 2020-ninit))

for (i in (ninit+1):2020){
  prevval <- out[i-1]
  if (prevval %in% out[1:(i-2)]){
    out[i] <- as.integer(abs(diff(rev(which(out==prevval))[1:2])))
  } else{
    out[i] <- 0L
  }
}

out[2020]
