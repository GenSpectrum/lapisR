library(devtools)
install_github("GenSpectrum/lapisR@dev")
library(lapisR)

session <- initialize("https://lapis.cov-spectrum.org/gisaid/v2", expireOnUpdate = TRUE)

getAggregated(session, country = "Switzerland", dateDay =20)
getDetails(session, country = "Switzerland", limit = 10, fields=c("country"))
getNucleotideMutations(session, region = "Europe", minProportion = 0.1, limit=10)

