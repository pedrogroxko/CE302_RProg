library(ggplot2)
data("diamonds")
str(diamonds)
 library(dplyr)
View(starwars)
View(diamonds)
?diamonds
df <- head(diamonds,n = 1000)

plot(df$carat , df$price,
     main = "Relação entre Preço e Peso dos diamantes",
     xlab = "Peso em Quilates",
     ylab = "Preço em Dólar",
     pch = 19, col = "darkgreen")

abline(lm(df$price ~ df$carat, data = df), col = "red", lwd = 2)
cor(df$price, df$carat, use = "complete.obs")
View(df)
summary(df)
lm
mean(df$price)
summary(df$price)

library(ggplot2)

# correlação numérica
cor(df$carat, df$price)
summary(df$carat[df$carat <1])
geom_smooth(method = "loess", color = "blue", se = FALSE)
library(ggplot2)

# gráfico dedisperção entre o preço e peso dos diamantes
ggplot(df, aes(x = carat, y = price)) +
  geom_point(alpha = 0.3) +  # pontos
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(
    title = "Relação entre preço e peso dos diamantes",
    x = "Peso (Quilates)",
    y = "Preço (Dólar)"
  )

library(ggplot2)

#Densidade dos preços
ggplot(df, aes(x = price)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(
    title = "Distribuição dos preços dos diamantes",
    x = "Preço (USD)",
    y = "Densidade"
  )

ggplot(df, aes(x = price, fill = cut)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Distribuição dos preços por qualidade de corte",
    x = "Preço (USD)",
    y = "Densidade"
  )

#Gráfico da distribuição dos pesos por qualidade de corte (interativo)
p <- ggplot(df_teste, aes(x = price, fill = cut)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Distribuição dos preços por qualidade de corte",
    x = "Preço (USD)",
    y = "Densidade"
  )

ggplotly(p)

summary(diamonds$price)
df_teste <- diamonds[diamonds$price < 2500,]
View(df_teste)
