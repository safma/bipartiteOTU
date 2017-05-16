#' @title Get metadata
#'
#' @description Funkce get_metadata vypisuje dostupná metadata a počet levlů v nich.
#'
#' @param biom_file již načtený biom soubor
#'
#' @export
#' @return dostupná metadata a počet levlů v nich
#'
#'
get_metadata <- function(biom_file){

  # nacteni metadat
  k <- biom::sample_metadata(biom_file)

  # overeni podminky ze existuji metadata k souboru
  if (is.null(k)){
    print("Zadna metadata")
  } else{

    a <- matrix(nrow = length(names(k)), ncol = 2)
    a[, 1] <- dQuote(names(k))
    x <- vector()

    # vypsani metadat vcetne poctu levelu
    for (i in 1:length(names(k))) {
      pom <- length(levels(as.factor(k[, i])))
      if (pom > 1) {
        a[i, 2] <- paste("[", length(levels(as.factor(k[, i]))), "]")
      } else{
        x <- c(x, i)
      }
    }

    # odstraneni prazdnych radku nebo radku s jedinym levelem
    if (length(x) > 0) {
      a <- a[-x, ]
    }

    # vypsani vysledku
    a <- noquote(a)
    colnames(a) <- c("metadata", "levels")
    return(a)
  }
}
