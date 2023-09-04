#' Chemotherapy for Stage B/C colon cancer
#'
#' These are data from one of the first successful trials of adjuvant chemotherapy for colon cancer.
#' Levamisole is a low-toxicity compound previously used to treat worm infestations in animals;
#' 5-FU is a moderately toxic (as these things go) chemotherapy agent.
#' There are two records per person, one for recurrence and one for death.
#' Copied from the survival package
#'
#'
#'
#'
#' @format ## `colon`
#' A data frame with 1858 rows and 16 variables:
#' \describe{
#'   \item{id}{Patient identifier}
#'   \item{study}{1 for all patients}
#'   \item{rx}{Treatment, categorical, "Obs" = Obervation, "Lev" = Levamisole, "Lev+5FU" = Levamisole + 5-FU}
#'   \item{sex}{Patient sex, 1 = male, 0 = female}
#'   \item{age}{Patient age, years}
#'   \item{obstruct}{Colon obstruction by tumour, 0 = no obstruction, 1 = obstruction}
#'   \item{perfor}{Perforation of colon, 0 = no perforation, 1 = perforation}
#'   \item{adhere}{Tumour adherence to nearby organs, 0 = no adherence, 1 = adherence}
#'   \item{nodes}{Number of positive lymph nodes}
#'   \item{status}{Censoring status, 0 = no event, 1 = event}
#'   \item{differ}{Differentiation of tumour, 1 = well differentiated, 2 = moderate, 3 = poor}
#'   \item{extent}{Extent of local spread, 1 = submucosa, 2 = muscle, 3 = serosa, 4 = continious}
#'   \item{surg}{Time from surgery to registration, 0 = short, 1 = long}
#'   \item{node4}{More than 4 positive lymphnodes}
#'   \item{time}{Follow-up time in days until event or end of follow-up}
#'   \item{etype}{Event type, 1 = recurrence, 2 = death}
#'   ...
#' }
#' @source JA Laurie, CG Moertel, TR Fleming, HS Wieand, JE Leigh, J Rubin, GW McCormack, JB Gerstner, JE Krook and J Malliard.
#' Surgical adjuvant therapy of large-bowel carcinoma: An evaluation of levamisole and the combination of levamisole and fluorouracil:
#' The North Central Cancer Treatment Group and the Mayo Clinic. J Clinical Oncology, 7:1447-1456, 1989.
"colon"
