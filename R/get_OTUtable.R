#' @title Get OTUtable
#'
#' @description Funkce get_OTUtable využívá tři přístupy k redukci OTU tabulky: Redukce podle taxonomie, redukce podle metadat (pokud jsou k dispozici)
#' a redukce nastavením prahové hodnoty.
#'
#' @param biom_file již načtený biom soubor
#' @param taxonomy textová proměnná. Taxonomická kategorie, na základě, které mají být data redukována. Možné volby jsou:
#' kingdom, phylum, class, order, family, genus a species.
#' @param NewOTU logická proměnná. Nastavení NewOTU = TRUE iniciuje zachování všech OTU včetně těch, u nichž není známá požadovaná taxonomická kategorie.
#' Přednastavená volba NewOTU = FALSE vede k odstranění OTU bez požadované taxonomické kategorie z dalšího zpracování.
#' @param metadata textová proměnná. Název metadat na základě kterých mají být data porovnávána a redukována
#' @param threshold číselná proměnná. Prahová hodnota se kterou jsou pozorování porovnávána.
#'
#'
#' @export
#' @return maticový zápis redukované OTU tabulky
#'
#'

get_OTUtable <- function(biom_file, taxonomy = "class", NewOTU = FALSE, metadata = NULL, threshold = 10){

  # nacteni dat
  OTUtab <- methods::as(biom::biom_data(biom_file), "matrix")

  # slouceni OTU na zaklade pozadovane taxonomie
  OTUtab <- merge_taxonomy(biom_file, OTUtab, taxonomy, NewOTU)

  # slouceni OTU na zaklade sample_metadata
  if ((is.null(metadata))==FALSE){
    OTUtab <- merge_metadata(biom_file, OTUtab, metadata)
  }

  # odstraneni pozorovani pod prahem cetnosti pozorovani
  OTUtab <- threshold_drop(OTUtab, threshold)

  return(OTUtab)
}
