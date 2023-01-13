setwd("/home/uff/Área de Trabalho/arquivos/lorena/dados_solo")
df <- read.csv("dados_solo.txt", h=T, sep="\t")
df<-as_tibble(df)

library(ggstatsplot)

names(df)

ggbetweenstats(
              data = df,
               x = Prof,
               y = argila,
               type = "nonparametric"
)
