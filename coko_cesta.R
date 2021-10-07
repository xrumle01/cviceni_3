CokoRek <- function(M,r,s){ # M = matice, s = 1, r = 1
  if(r == nrow(M)){
    return(M[r,s]) # ukonceni
  } else {
    C <- M[r, s]
    Cdolu <- CokoRek(M,r+1,s) # volani funkce pro vypocet dolù
    Csikmo <- CokoRek(M,r+1,s+1) # volani funkce pro vypocet šikmo
  }
}