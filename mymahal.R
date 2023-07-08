## ---- myrcode3
#libary(DOS)

# vstup: df - datovy soubor pro parovani, z - indikator prirazeni lecby (0,1),
# covariates - data frame, kde sloupce jsou klicove promenne (numeric) a propensity skore
# vystup: dataframe - datovy soubor po provedenem parovani na zaklade 
# propensity skore a promennych obsazenych v covariates
 
parovani_mahal <- function(df,covariates,z)
{
  
  X = mahal(z,covariates)
  N = nrow(X); shluky = 1:length(z)
  
  
  for(i in 1:N)
  {
    # index nejblizsich hodnot
    X = rbind(X,rep(Inf,ncol(X)))
    nejbliz = which(X == min(X), arr.ind = TRUE)[1, ]
    
    nejblizsi_leceny = rownames(X)[nejbliz[1]]
    nejblizsi_kontrolni =  colnames(X)[nejbliz[2]]
    # Vytvoreni paru
    #nazvy = as.numeric(row.names(X)) # 1:N
    shluky[which(shluky == nejblizsi_kontrolni)] = nejblizsi_leceny
    
    # Vyhozeni vzdalenosti paru z matice
    X = X[-nejbliz[1], -nejbliz[2]]
  }
  
  df$shluky = shluky
  
  matched_muj = subset(df,duplicated(shluky) | duplicated(shluky, fromLast=TRUE))
  return(matched_muj)
}