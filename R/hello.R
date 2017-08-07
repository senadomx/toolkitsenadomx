# devtools::install_github("senadomx/datasenadomx")
library(datasenadomx)
library(FactoMineR)

data(l62votes)
data(l62rollcalls)


# datamatrix
X <- l62votes[!duplicated(l62votes), ] %>%
  select(senador, rollcall_id, voto) %>%
  spread(key = rollcall_id, value = voto)

for (c in 1:ncol(X)) {
  X[ ,c] <- factor(X[ ,c])
}


models <- MCA(X)
summary(models)
plot(models,invisible=c("var","quali.sup","quanti.sup"),cex=0.7)
