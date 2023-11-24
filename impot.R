library(data.table)
d <- function(percent, max.CAD){
  data.table(percent, max.CAD)
}
CAD.brut <- 134375
positive.part <- function(x)ifelse(x>0, x, 0)
rate.dt <- rbind(
  ## https://www.revenuquebec.ca/fr/citoyens/declaration-de-revenus/produire-votre-declaration-de-revenus/taux-dimposition/
  data.table(revenu="Quebec", rbind(
    d(14, 49275), 
    d(19, 98540),
    d(24, 119910),
    d(25.75, Inf)
  )),
  ## https://www.canada.ca/fr/agence-revenu/services/impot/particuliers/foire-questions-particuliers/taux-imposition-canadiens-particuliers-annee-courante-annees-passees.html#toc0
  data.table(revenu="Canada", rbind(
    d(15, 53359), #inclusive
    d(20.5, 106717),
    d(26, 165430),
    d(29, 235675),
    d(33, Inf)
  ))
)[
, diff := c(max.CAD[1],diff(max.CAD)), by=revenu
][
, prev.max := c(0,max.CAD[-.N]), by=revenu
][
, CAD.in.rate := ifelse(
  CAD.brut>max.CAD, diff, positive.part(CAD.brut-prev.max))
][
, impot := CAD.in.rate*percent/100
][]
imposable <- rate.dt[, .(
  CAD.imposable=sum(CAD.in.rate),
  impot=sum(impot)
), keyby=revenu
][
, cum.impot := cumsum(impot)
][
, CAD.net := CAD.brut-cum.impot
][
, CAD.net.mensuel := CAD.net/12
][]
