HanojVez <- function(n,zKoliku,naKolik){
  if(n == 1){
    hlaska <- c('Pøesuò disk z kolíku',zKoliku, 'na kolík', naKolik)
    print(hlaska)
    return
  }else{
    volnyKolik <- 6 - zKoliku - naKolik
    HanojVez(n-1,zKoliku,volnyKolik)
    hlaska <- c('Pøesuò disk z kolíku',zKoliku, 'na kolík', naKolik)
    print(hlaska)
    HanojVez(n-1,volnyKolik,naKolik)
  }
}