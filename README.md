# lapisR - Retrieving data from LAPIS

lapisR provides functions for retrieving data from various LAPIS endpoints. These include functions requesting database statistics, sets of sequences and their metadata, sets of mutations or insertions, and sequence alignments.

## Installation

```         
library(devtools)
install_github("GenSpectrum/lapisR")
```
## Usage
### Session initialization
Sessions can be initialized with or without an access key. They can also be set to expire in case the database receives an update.

Example:
```
session <- lapisR::initialize("https://lapis.cov-spectrum.org/gisaid/v2", expireOnUpdate = TRUE, accessKey="myKey")
```
### Database statistics
Aggregated data can be retrieved using the `getAgregated` function. Various filters can be applied. Valid filter keys for a particular session can be found with `getFilters(session)`.

Example:
```
getAggregated(session, fields=c("country"), date = '2020-05-05')
```
### Sequences
Nucleotide sequences can be obtained in FASTA format with the `getNucleotideSequence` function, either as string or as compressed or uncompressed file.

Example:
```
getNucleotideSequences(session, country = "Switzerland", date = '2020-05-05', limit = 10, downloadAsFile = TRUE, compression = 'gzip', out = 'mySequences')
```
### Metadata
Sequence metadata can be retreived with `getDetails`.

Example:
```
getDetails(session, country = "Switzerland", date = '2020-05-05', limit = 10)
```
### Mutations / Insertions
Nucleotide and amino acid mutations or insertions can be retreived with `getNucleotideMutations`, `getNucleotideInsertions`, `getAminoAcidMutations`, and `getAminoAcidInsertions`.

Example:
```
getNucleotideMutations(session, region = "Europe", minProportion = 0.1, limit = 100)
```
### Alignments
Nucleotide and amino acid sequence alignments can be obtained with the `getNucleotideAlignment` and `getAminoAcidAlignment` functions, as string or as compressed or uncompressed FASTA file.

Example:
```
getAminoAcidAlignment(session, country = "Switzerland", date = '2020-05-05', limit = 10, downloadAsFile = TRUE, out = 'myAlignment')
```