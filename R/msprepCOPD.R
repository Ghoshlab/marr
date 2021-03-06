#' Example of processed mass spectrometry dataset
#'
#' Data contains LC-MS metabolite analysis for samples from 20 subjects.
#' and 662 metabolites. The raw data was pre-processed using MSPrep method.
#' The raw data pre- processing include 3 steps- Filtering, Missing Value
#' Imputation and Normalization. Filtering- the metabolites(columns)
#' in the raw data were removed if they were missing more than 80 percent
#' of the samples. Missing Value Imputation- The Bayesian
#' Principal Component Analysis (BPCA) was applied to impute the missing values.
#' Normalization- median normalization was applied to remove unwanted
#' variation appears from various sources in metabolomics studies.
#' The first three columns indicate "Mass" indicating the mass-to-charge ratio,
#' "Retention.Time", and "Compound.Name" for each present metabolite.
#' The remaining columns indicate abundance for each of the 645
#' mass/retention-time combination for each subject combination.
#'
#' @docType data
#' @format SummarizedExperiment assay object containing
#' 645 metabolites (features) of 20 subjects (samples).
#' \describe{
#' \item{Mass}{Mass-to-charge ratio}
#' \item{Retention.Time}{Retention-time}
#' \item{Compound.Name}{Compound name for each mass/retention time
#'   combination}
#' \item{X10062C}{The columns indicate metabolite abundances
#'   found in each subject combination. Each column begins with an
#'   'X', followed by the subject ID.}
#' }
#' @source \url{https://www.metabolomicsworkbench.org/data/
#' DRCCMetadata.php?Mode=Project&ProjectID=PR000438}
#'
#' The raw data is available at the NIH Common Fund's National Metabolomics Data
#' Repository (NMDR) website, the Metabolomics Workbench,
#' https://www.metabolomicsworkbench.org, where it has been assigned Project
#' ID PR000438. The raw data can be accessed directly via it's Project DOI:
#' 10.21228/M8FC7C This work is supported by NIH grant, U2C- DK119886.
#'
#' @references
#' Nichole Reisdorph. Untargeted LC-MS metabolomics analysis of human
#' COPD plasma, HILIC & C18, metabolomics_workbench, V1.
#'
#' Hughes, G., Cruickshank-Quinn, C., Reisdorph, R., Lutz, S.,
#' Petrache, I., Reisdorph, N., Bowler, R. and Kechris, K., 2014.
#' MSPrep—Summarization, normalization and diagnostics for processing
#' of mass spectrometry–based metabolomic data.
#' Bioinformatics, 30(1), pp.133-134.
#' @keywords datasets
#' @examples
#' data(msprepCOPD)
#' @usage data(msprepCOPD)
"msprepCOPD"

