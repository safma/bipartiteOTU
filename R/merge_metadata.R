#' @title Merge metadata
#'
#' @description Funkce merge_metadata redukuje OTU tabuku na základě dostupných metadat. OTU shodující se v metadatech jsou
#' sloučena do jednoho OTU nazvaného po hodnotě metadat.
#'
#' @param biom_file již načtený biom soubor
#' @param OTUtab maticový zápis OTU tabulky
#' @param metadata textová proměnná. Název metadat na základě kterých mají být data porovnávána a redukována
#'
#'
#' @export
#' @return maticový zápis redukované OTU tabulky
#'

merge_metadata <- function(biom_file, OTUtab, metadata){

  # nacteni metadat
  k <- biom::sample_metadata(biom_file)

  # slouceni na zaklade informace obsazene v metadatech
  if (length(k)>1){
    vyber <- which(names(k)== metadata)

    pom <- levels(as.factor(k[,vyber]))
    redmat <- matrix(nrow=dim(OTUtab)[1], ncol = length(pom))

    for (i in 1:length(pom)){
      a <- which(k[,vyber] == pom[i])
      if (length(a)>1){
        redmat[,i] <- rowSums(OTUtab[,a])
      } else{
        redmat[,i] <- OTUtab[,a]
        }
    }

    # rownames & colnames
    rownames(redmat) <- rownames(OTUtab)
    colnames(redmat) <- as.factor(pom)


  } else {
    print("Zadna metadata nejsou k dispozici")

  }

  OTUtab <- redmat
  return(OTUtab)
}
