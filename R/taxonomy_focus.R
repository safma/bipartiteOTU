#' @title Taxonomy focus
#'
#' @description Funkce taxonomy_focus umožňuje zaměřit se pouze na určitou taxonomickou kategorii. Všechny OTU nenáležející
#' do této specifické kategorie jsou z OTU tabulky odstraněny.
#'
#' @param biom_file již načtený biom soubor
#' @param OTUtab maticový zápis OTU tabulky
#' @param taxlevel textová proměnná. Taxonomická kategorie, na základě, které mají být data redukována. Možné volby jsou:
#' kingdom, phylum, class, order, family, genus a species.
#' @param value textová proměnná. Specifická hodnota taxonomie jako požadovaná vlastnost všech OTU ve výstupní OTU tabulce.
#'
#' @export
#' @return maticový zápis redukované OTU tabulky
#'
#'


taxonomy_focus <- function(biom_file, OTUtab, taxlevel, value){

  # načtení taxonomie do pomocné tabulky
  a <-biom::observation_metadata(biom_file)
  c <- vector()

  for (i in 1:length(a)){
    c[i] <- length(a[[i]])
  }

  d <- cumsum(c)
  d <- append(d, 0, 0)
  b <- as.matrix(unlist(a))
  pom <- matrix(nrow = length(c), ncol=7)

  for (i in 1:(length(c))){
    pom[i,1:c[i]] <- b[(d[i]+1):d[i+1]]
  }

  rownames(pom)<-names(a)

  # kdyz nektere OTU uz odstraneny
  if(dim(pom)[1]!=dim(OTUtab)[1]){
    pom <- pom[row.names(pom) %in% row.names(OTUtab),]
  }

  predpona <- character()
  # nahrada taxonomicke kategorie ciselnym ukazatelem
  if (taxlevel == "kingdom" | taxlevel == "Kingdom"){
    taxlevel <- 1
    predpona <- "k__"
  } else if (taxlevel == "phylum" | taxlevel == "Phylum"){
    taxlevel <- 2
    predpona <- "p__"
  } else if (taxlevel == "class" | taxlevel == "Class"){
    taxlevel <- 3
    predpona <- "c__"
  } else if (taxlevel == "order" | taxlevel == "Order"){
    taxlevel <- 4
    predpona <- "o__"
  } else if (taxlevel == "family" | taxlevel == "Family"){
    taxlevel <- 5
    predpona <- "f__"
  } else if (taxlevel == "genus" | taxlevel == "Genus"){
    taxlevel <- 6
    predpona <- "g__"
  } else if (taxlevel == "species" | taxlevel == "Species"){
    taxlevel <- 7
    predpona <- "s__"
  }

  # v?b?r
  predpona <- paste(predpona,value, sep="")
  if (any(pom[,taxlevel]==value | pom[,taxlevel]==predpona, na.rm = TRUE)){
    z <- (pom[,taxlevel]==value | pom[,taxlevel]==predpona)
    z[is.na(z)] <- FALSE
    pom <- pom[z==TRUE,]
    OTUtab <- OTUtab[rownames(OTUtab) %in% rownames(pom),]
  }

  return(OTUtab)

}
