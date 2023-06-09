#' A function that finds which phenotypes from a collection of phenotypes (openGWAS for this package)
#' map to specific terms in an ontology (EFO for this package).
#' @description
#' This function relies on a SQLite database that contains mappings from a set of phenotypes to one or more
#' ontologies.  The database contains information about the organization of the ontology and hence can find
#' phenotypes that map directly to a term or those that are inherited at a term by virtue of having mapped
#' directly to one of the chilren or other more specific descriptions.
#' @param search_terms character() A label for the term (EFO in this case) that is of interest.
#' @param include_subclasses logical(1) defaults to TRUE; if `TRUE` then all terms mapped to subclasses (children) of the term are included.
#' @param direct_subclasses_only logical(1) defaults to FALSE; If `TRUE` then only terms that map to direct children (first step only) are included.
#' @details The mapping of openGWAS traits/phenotypes to EFO ontology terms was performed by `text2term`. The results were
#' then organized, together with information on the sturcture of the ontology (EFO in this case) to facilitate retrieval.
#' Included in the return values is the confidence score provided by `text2term` which gives some indication of how syntactically
#' similar the phenotype description was to the ontology term label.
#' @return
#' A `data.frame` with columns:
#' \describe{
#'   \item{OpenGWAS ID}{The openGWAS ID for the trait.}
#'   \item{OpenGWAS Trait}{The openGWAS text label for that trait.}
#'   \item{Ontology Term}{The text description/label of the ontology term.}
#'   \item{Ontology Term ID}{The label for the ontology term.}
#'   \item{Mapping Confidence}{The confidence score from mapping the phenotype label to the ontology label.}
#' }
#' @references `text2term`
#' @author Robert Gentleman
#' @examples
#' ematch <- resources_annotated_with_term("EFO:0005297", include_subclasses = FALSE)
#' dim(ematch)
#' ematch[1, ]
#' @export
resources_annotated_with_term <- function(search_terms, include_subclasses = TRUE, direct_subclasses_only = FALSE) {
  ontology_name <- "efo"
  con <- gwasCatSearch_dbconn()
  if (include_subclasses) {
    if (direct_subclasses_only) {
      ontology_table <- paste0(ontology_name, "_edges")
    } else {
      ontology_table <- paste0(ontology_name, "_entailed_edges")
    }
  } else {
    ontology_table <- paste0(ontology_name, "_edges")
  }

  query <- paste0("SELECT DISTINCT
                    m.`STUDY.ACCESSION`,
                    m.`DISEASE.TRAIT`,
                    m.MAPPED_TRAIT,
                    m.PUBMEDID,
                    m.MAPPED_TRAIT_URI,
                    m.MAPPED_TRAIT_CURIE
                  FROM `gwascatalog_metadata` m
                LEFT JOIN ", ontology_table, " ee ON (m.MAPPED_TRAIT_CURIE = ee.Subject)")

  index <- 0
  where_clause <- "\nWHERE ("
  for (term in search_terms) {
    if (index == 0) {
      where_clause <- paste0(where_clause, "m.MAPPED_TRAIT_CURIE = \'", term, "\'")
    } else {
      where_clause <- paste0(where_clause, " OR m.MAPPED_TRAIT_CURIE = \'", term, "\'")
    }
    if (include_subclasses) {
      where_clause <- paste0(where_clause, " OR ee.Object = \'", term, "\'")
    }
    index <- index + 1
  }
  query <- paste0(query, where_clause, ")")
  results <- dbGetQuery(con, query)
  ## results$MappingConfidence = round(results$MappingConfidence, digits=3)
  return(results)
}
