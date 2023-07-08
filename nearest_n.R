Sys.setlocale("LC_CTYPE", "czech")
df <-read.csv("df.csv", encoding="latin1", stringsAsFactors=TRUE)

m_ps <- glm(skupina ~ vek+pohlaví+stadium+BMI_kat1,
            family = binomial(), data = df)
summary(m_ps)

prs_df <- data.frame(pr_score = predict(m_ps, type = "response"),
                     skupina = m_ps$model$skupina)

rownames(df) <- 1:nrow(df)
rownames(prs_df) <- 1:nrow(prs_df)

prs_df$skupina = as.integer(prs_df$skupina)
## ---- myrcode2

### df: datovy soubor
### prs_df: datovy soubor s odhadnutym propensity skore a skupina jako integer
### pro vypocet matice vzdalenosti
### metrika: zpusob vypocteni vzdalenosti ("euclidean", "maximum", "manhattan",
### "canberra","binary", "minkowski", "pearson", "spearman" or "kendall")
parovani <- function(df,skupina,pr_score,metrika)
{
  prs_df = data.frame(df$skupina,df$pr_score)
  prs_df$skupina = as.integer(prs_df$skupina)

  X = as.matrix(dist(prs_df,method=metrika,diag=T,upper=T))
  X[X<1] <- 2
  diag(X) = Inf; N = nrow(X); shluky = 1:N

  t = table(df$skupina)
  k = unname(t[2])

  for(i in 1:k)
  {
    # index nejblizsich hodnot
  
    nejbliz = which(X == min(X), arr.ind = TRUE)[1, ]
  
    # Vytvoreni paru
    nazvy = as.numeric(row.names(X)) # 1:N
    shluky[which(shluky == nazvy[nejbliz[2]])] = nazvy[nejbliz[1]]
  
    # Vyhozeni vzdalenosti paru z matice
    X = X[-nejbliz[2], -nejbliz[2]]
    X = X[-(nejbliz[1]-1), -(nejbliz[1]-1)]
  }

  df$shluky = shluky

  matched_muj = subset(df,duplicated(shluky) | duplicated(shluky, fromLast=TRUE))
  return(matched_muj)
}
## vystup: matched datovy soubor podle propensity skore

# priklad pouziti funkce: df_matched = parovani(df,prs_df,"euclidean")


