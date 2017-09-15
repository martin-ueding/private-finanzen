#!/usr/bin/Rscript

taxe <- read.table('gliedertaxe.txt')
taxe.rente <- read.table('gliedertaxe-rente.txt')

inv <- c(1:100)

colors <- c('#e41a1c', '#377eb8', '#4daf4a', '#984ea3')

inv.225 <- inv
inv.225[inv > 25] = inv.225[inv > 25] + c(1:75)
inv.225[inv > 50] = inv.225[inv > 50] + c(1:50)

inv.300 <- inv
inv.300[inv > 25] = inv.300[inv > 25] + c(1:75) * 2
inv.300[inv > 50] = inv.300[inv > 50] + c(1:50)

inv.350 <- inv
inv.350[inv > 25] = inv.350[inv > 25] + c(1:75) * 2
inv.350[inv > 50] = inv.350[inv > 50] + c(1:50) * 2

invs <- matrix(c(inv.225, inv.225, inv.225, inv.350), ncol=4)

progressed <- taxe

for (col in c(1:length(colnames(taxe)))) {
    for (row in c(1:length(rownames(taxe)))) {
        progressed[row, col] <- invs[taxe[row, col], col]
    }
}


inval.2480 <- c(200, 100, 140, 82)

grundbetrag <- list(A=200, B=100, C=140, D=82)

betrag <- t(apply(progressed, 1, function(x) x * unlist(grundbetrag) / 100))


svg('plot-grundbetrag.svg')
barplot(unlist(grundbetrag), beside=TRUE, horiz=TRUE, col=colors,
        main='Grundbetrag', xlab='Grundbetrag / 1000 EUR')

svg('plot-progression.svg')
plot(inv.350, type='l', main='Progression: 225% und 350%',
     xlab='Invalidität / %', ylab='Invalidität nach Progression / %', col='blue')
lines(inv.225, col='red')
grid(col='black')

svg('plot-taxe-schlecht.svg')
par(las=2)
par(mar=c(5,12,4,2))
barplot(t(as.matrix(taxe.rente)), col=colors,
        beside=TRUE, legend=colnames(taxe.rente), horiz=TRUE,
        main='Festsetzung der Rente (schlechte Gliedertaxe)',
        xlab='Invaliditätsgrad / %')
grid(ny=NA, col='black')
abline(v=50, col='red')

svg('plot-taxe-gut.svg')
par(las=2)
par(mar=c(5,12,4,2))
barplot(t(as.matrix(taxe)), col=colors,
        beside=TRUE, legend=colnames(taxe), horiz=TRUE,
        main='Invalidität ohne Progression (gute Gliedertaxe)',
        xlab='Invaliditätsgrad / %')
grid(ny=NA, col='black')

svg('plot-auszahlung.svg')
par(las=2)
par(mar=c(5,12,4,2))
barplot(t(as.matrix(betrag)), col=colors,
        beside=TRUE, legend=colnames(betrag), horiz=TRUE,
        main='Ausgeschütteter Betrag',
        xlab='Betrag / 1000 EUR')
grid(ny=NA, col='black')

cat("\nGute Gliedertaxe in %\n")
print(taxe)

cat("\nSchlechte Gliedertaxe in %\n")
print(taxe.rente)

cat("\nGliedertaxe nach Progression in %\n")
print(progressed)

cat("\nAusgeschütteter Betrag in 1000 EUR\n")
print(betrag)
