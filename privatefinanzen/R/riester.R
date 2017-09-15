#' Jährliche Zulage durch Riester-Förderung.
#'
#' @param einzahlung Eingezahlter Beitrag inklusive Zulage
#' @param bruttoeinkommen Bruttoeinkommen
#' @param kinder Anzahl der Kinder, geboren nach 2008
#'
#' @return
#' @export
#'
#' @examples
riester.zulage <- Vectorize(function(einzahlung, bruttoeinkommen, kinder) {
    max.zulage <- 175 + kinder * 300

    vier.prozent <- bruttoeinkommen * 0.04

    if (einzahlung < 60) {
        zulage <- 0
    }
    else {
        zulage <- min(einzahlung / vier.prozent, 1) * max.zulage
    }

    return (zulage)
}, vectorize.args = c('einzahlung'))

riester.steuererlass <- Vectorize(function(einzahlung, bruttoeinkommen, kinder) {
    maximal.absetzbar <- 2100
    zulage <- get.zulage(einzahlung, bruttoeinkommen, kinder)
    erlass <- get.grenzsteuersatz(bruttoeinkommen) * min(einzahlung, maximal.absetzbar)
    max(erlass - zulage, 0)
}, vectorize.args = c('einzahlung'))
