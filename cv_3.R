VratitMince <- function(M){
  padesatek <- floor(M/50)
  zbytek <- M - 50*padesatek
  dvacek <- floor(zbytek/20)
  zbytek <- zbytek - 20*dvacek
  desitek <- floor(zbytek/10)
  zbytek <- zbytek - 10*desitek
  petek <- floor(zbytek/5)
  zbytek <- zbytek - 5*petek
  dvojek <- floor(zbytek/2)
  zbytek <- zbytek - 2*dvojek
  jednicek <- zbytek
  vysledek <- c('padesatek:', padesatek, 'dvacek:', dvacek, 'desitek:', desitek, 'petek:', petek, 'dvojek:', dvojek, 'jednicek:', jednicek)
  return(vysledek)
}

VratitMince_optimal <- function(M,mince){
  vratit <- c()
  while(M > 0){
    for(i in 1:length(mince)){
      if (M > mince[i]){
        x <- floor(M/mince[i])
        y <- mince[i] * x
        vratit <- c(vratit, y)
        M <- M - vratit[length(vratit)]
      }
    }
  }
  return(vratit)
}

JePotrebaVratit <- VratitMince_optimal(56,c(50, 20, 10, 5, 2, 1))
