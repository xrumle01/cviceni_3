HanojVez <- function(n,zKoliku,naKolik){
  if(n == 1){
    hlaska <- c('P�esu� disk z kol�ku',zKoliku, 'na kol�k', naKolik)
    print(hlaska)
    return
  }else{
    volnyKolik <- 6 - zKoliku - naKolik
    HanojVez(n-1,zKoliku,volnyKolik)
    hlaska <- c('P�esu� disk z kol�ku',zKoliku, 'na kol�k', naKolik)
    print(hlaska)
    HanojVez(n-1,volnyKolik,naKolik)
  }
}