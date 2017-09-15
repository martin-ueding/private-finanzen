library(privatefinanzen)
source('plot-setup.R')

# Jährlicher VL Betrag des Arbeitgebers.
vl <- 3.33 * 12

# Jährliche Kosten des VL Depots.
kosten <- 12

jahre <- 1:7
rendite <- c(-100, 0, 3, 6, 9)
rendite.txt <- sapply(rendite, function (x) paste(x, ' %'))
faktor <- 1 + rendite/100
entwicklung <- matrix(rep(faktor, each=length(jahre)), nrow=length(jahre))^rev(jahre)
print(entwicklung)
entwicklung <- data.frame(t(entwicklung), row.names=rendite)
colnames(entwicklung) <- jahre
print(entwicklung)

#barplot(t(as.matrix(entwicklung)), beside=TRUE, names.arg=rendite.txt, legend.text=jahre)

wert <- apply(vl * entwicklung, 1, function (row) sum(row[1:3]))
print(wert)

barplot(t(as.matrix(wert - kosten * 7)),
        beside=TRUE, names.arg=rendite.txt,
        col=c('red', rep('black', 6)),
        main='Gesamtgewinn nach 7 Jahren bei 3 Jahren VL (3,33 EUR/Monat)\nund 12 EUR/Jahr Depotkosten',
        xlab='Jährliche Fondsentwicklung',
        ylab='Gesamtgewinn / EUR')

library(ggplot2)
library(reshape2)

theme_set(theme_bw())

gewinn <- wert - kosten * 7

gg.gewinn <- data.frame(rendite=rendite.txt, gewinn=as.vector(gewinn))
print(gg.gewinn)

ggplot(gg.gewinn, aes(x=rendite, y=gewinn)) +
    geom_bar(stat='identity') +
    labs(x='Rendite der Fonds',
         y='Gewinn',
         title='Gesamtgewinn nach 7 Jahren bei 3 Jahren VL',
         subtitle='VL = 3,33 EUR/Monat, Depotkosten = 12 EUR/Jahr')
my.ggsave('plot-gesamtgewinn-doktorand.svg')

v <- function(p, k, n) {
    p1 <- 1 + p
    r <- 7 * k * p1^(n - 7) * (-1 + p1^(1/12)) / (-1 + p1^n)
    r[is.nan(r)] <- 7 * k / (12 * n)
    r
}

p <- seq(-0.10, 0.15, 0.01)
y <- v(p, 12, 2)

df <- data.frame(Rendite=p * 100, VL=y)

print(df)

ggplot(df, aes(x=Rendite, y=VL)) +
    geom_line() +
    geom_vline(xintercept=6, color='red') +
    geom_hline(yintercept=v(0.06, 12, 2), color='red') +
    labs(title='Gewinnschwelle',
         subtitle='Depotkosten = 12 EUR/Jahr, drei Jahre Einzahlungen',
         x='Rendite des Fonds [%]',
         y='VL [EUR/Monat]')
my.ggsave('vl-gewinnschwelle.svg')
