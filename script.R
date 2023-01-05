setwd("/home/uff/Área de Trabalho/arquivos/lorena/dados_solo")

df <- read.csv("dados_solo.txt", h=T, sep="\t")

head(df)

par(mfrow=c(4,5))

for (col in 4:ncol(df)) {
  print(paste(names(df)[col]))
  print(shapiro.test(df[,col]))
  qqnorm(df[,col], main=paste(names(df)[col]))
  qqline(df[,col], col="red")
}

library(GFD)

for (col in 4:ncol(df)) {
  if (col == 4) {
    boxplot(df[,col] ~ df[,"Áreas"] * df[,"Prof"] * df[,"Coleta"], main=paste(names(df)[col]))
    print(paste(names(df)[col]))
    print(kruskal.test(df[,col]~df[,"Áreas"]))
    print(kruskal.test(df[,col]~df[,"Prof"]))
    print(kruskal.test(df[,col]~df[,"Coleta"]))
    print(wilcox.test(df[,col]~df[,"Prof"]))
    print(wilcox.test(df[,col]~df[,"Coleta"]))
    print(GFD(df[,col] ~ df[,"Áreas"] * df[,"Prof"] * df[,"Coleta"]))
  } else {
    boxplot(df[,col] ~ df[,"Áreas"] * df[,"Prof"], main=paste(names(df)[col]))
    print(paste(names(df)[col]))
    print(kruskal.test(df[,col]~df[,"Áreas"]))
    print(kruskal.test(df[,col]~df[,"Prof"]))
    print(wilcox.test(df[,col]~df[,"Prof"]))
    #print(GFD(df[,col][1:24] ~ df[,"Áreas"][1:24] * df[,"Prof"][1:24]))
  }
}

fatorial <- function(x) {
  if (x == 0 || x == 1) {
    return(1)
  } else {
    return(x * fatorial(x-1))
  }
}

fatorial <- function(x) {
  x <- x-1
  v <- x:1
  for (i in v) {
    x <- x * i
  }
  return(x)
}
