CokoRek <- function(M,r,s){ # M = matice, s = 1, r = 1
  if(r == nrow(M)){
    return(M[r,s]) # ukonceni
  } else {
    C <- M[r, s]
    Cdolu <- CokoRek(M,r+1,s) # volani funkce pro vypocet dolù
    Csikmo <- CokoRek(M,r+1,s+1) # volani funkce pro vypocet šikmo
    M[r+1, s] <- max(Cdolu,Csikmo) + C
  }

}

CokoIter <- function(M){
  s <- dim(M)
  k1<- seq(from=s[1]-1,to=1,by=-1)
  for(r in k1){
    k2 <- seq(from=r,to=1,by=-1)
    for(s in k2){
      Cdolu <- M[r+1,s]+M[r,s]
      Csikmo <- M[r+1,s+1]+M[r,s]
      M[r,s] <- max(c(Cdolu,Csikmo))
    }
  }
  return(M[1,1])
}