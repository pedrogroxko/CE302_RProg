require(read.dbc)

#2022
url <- "http://www.leg.ufpr.br/~ara/data/TUBEBR22.dbc"
download.file(url, destfile = "TUBEBR22.dbc", mode = "wb")
dados22 <- read.dbc("TUBEBR22.dbc")
nrow(dados22)
col(dados22)

#2023
url <- "http://www.leg.ufpr.br/~ara/data/TUBEBR23.dbc"
download.file(url, destfile = "TUBEBR23.dbc", mode = "wb")
dados23 <- read.dbc("TUBEBR23.dbc")
nrow(dados23)
col(dados23)

#2024
url <- "http://www.leg.ufpr.br/~ara/data/TUBEBR24.dbc"
download.file(url, destfile = "TUBEBR24.dbc", mode = "wb")
dados24 <- read.dbc("TUBEBR24.dbc")
nrow(dados24)
col(dados24)

summary(dados22)

sum(names(dados22)==names(dados23))
sum(names(dados23)==names(dados24))

names(dados22)

data.frame(dados22$DT_NOTIFIC[1:30],
dados22$NU_ANO[1:30])

dadosT=rbind(dados22,
             dados22,
             dados24)
nrow(dadosT)
nas.por.variavel <- apply(is.na.data.frame(dadosT),2, sum)
prop.mis <- round(nas.por.variavel/nrow(dadosT), 2)
prop.mis
par(mar=c(4,6,1,1))
CEX = 0.85
barplot(prop.mis[1:25],
        horiz = T,
        las=1,
        cex.names = CEX,
        xlab="NA prop.")
barplot(prop.mis[26:50],
        horiz = T,
        las=1,
        cex.names = CEX,
        xlab="NA prop.")
barplot(prop.mis[51:75],
        horiz = T,
        las=1,
        cex.names = CEX,
        xlab="NA prop.")
barplot(prop.mis[76:97],
        horiz = T,
        las=1,
        cex.names = CEX,
        xlab="NA prop.")



nosso_bar <- function(x){
  barplot(x,
          horiz = T,
          las=1,
          cex.names=CEX,
          xlab="NA prop.",
          border=NA,
          col="tomato")
}

nosso_bar(prop.mis[1:25])
nosso_bar(prop.mis[26:50])
nosso_bar(prop.mis[51:75])
nosso_bar(prop.mis[76:97])

sort(prop.mis)

names(dadosT)[prop.mis<0.10]
summary

REGIAO <- ifelse(startsWith(as.character(dadosT$SG_UF), "1"), "N",
                 ifelse(startsWith(as.character(dadosT$SG_UF), "2"), "ND",
                        ifelse(startsWith(as.character(dadosT$SG_UF), "3"), "SD",
                               ifelse(startsWith(as.character(dadosT$SG_UF), "4"), "S", "CO"))))
POP22REG <- c(29937706, 84840113, 16289538, 54658515, 17354884)
casos.regiao <- table(REGIAO)
REGIAO <- factor(REGIAO, levels=c("S", "SD", "CO", "ND", "N"))
summary(REGIAO)
INCIDENCIA <- round((casos.regiao/POP22REG)*10000,2)
INCIDENCIA
barplot(INCIDENCIA)

inc.norte <- 23.89
inc.nd <- 15.01
inc.co <- 9.49
inc.sd <- 16.69
inc.s <- 12.58

barplot

incidencias <- c(inc.s, inc.sd, inc.co, inc.nd, inc.norte)
names(incidencias) <- c("Sul", "Sudeste", "Centro-Oeste", "Nordeste", "Norte")
barplot(incidencias,
        col = "lightblue",
        main = "Incidência de Tuberculose por Região",
        ylab = "Incidência")
