library(devtools)
install_github("GenSpectrum/lapisR")

library(lapisR)
library(roxygen2)
library(styler)

roxygenise()

style_dir(exclude_dirs = c("tests/testthat/endpoints"))

session <- lapisR::initialize("https://lapis.cov-spectrum.org/gisaid/v2", expireOnUpdate = TRUE, accessKey = "")

getAggregated(session, country = "Switzerland", dateDay = 20)
getDetails(session, country = "Switzerland", limit = 10, fields = "country")
getNucleotideMutations(session, region = "Europe", minProportion = 0.1, limit = 10, orderBy = "count")
getNucleotideInsertions(session, region = "Europe", limit = 5)
getAminoAcidMutations(session, region = "Europe", minProportion = 0.1, limit = 10)
getAminoAcidInsertions(session, region = "Europe", limit = 10)
getNucleotideAlignment(session, country = "Poland", dateDay = 5, dateMonth = 5, limit = 3)
getAminoAcidAlignment(session, "ORF1a", country = "Poland", dateDay = 5, dateMonth = 5, limit = 3)
getNucleotideSequences(session, country = "Poland", dateDay = 5, dateMonth = 5, limit = 3)

alignment <- getNucleotideAlignment(session, country = "Poland", dateDay = 5, dateMonth = 5, limit = 3, downloadAsFile = F)
write(alignment, "alignment.fasta")
alignment_aa <- getAminoAcidAlignment(session, "ORF1a", country = "Poland", dateDay = 5, dateMonth = 5, limit = 3, downloadAsFile = F)
write(alignment_aa, "aa_alignment.fasta")
seqs <- getNucleotideSequences(session, country = "Poland", dateDay = 5, dateMonth = 5, limit = 3, downloadAsFile = F)
write(seqs, "sequences.fasta")
