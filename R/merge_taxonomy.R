#' @title Merge taxonomy
#'
#' @description Na základě požadované taxonomické úrovně jsou porovnávány OTU mezi sebou
#' a OTU se stejnou taxonomickou kategorií jsou sečtena a přiřazena novému OTU nazvanému podle taxonomického určení,
#' které je spojuje
#'
#' @param biom_file již načtený biom soubor
#' @param OTUtab maticový zápis OTU tabulky
#' @param taxonomy textová proměnná. Taxonomická kategorie, na základě, které mají být data redukována. Možné volby jsou:
#' kingdom, phylum, class, order, family, genus a species.
#'
#' @param NewOTU logická proměnná. Nastavení NewOTU = TRUE iniciuje zachování všech OTU včetně těch, u nichž není známá požadovaná taxonomická kategorie.
#' Přednastavená volba NewOTU = FALSE vede k odstranění OTU bez požadované taxonomické kategorie z dalšího zpracování.
#'
#' @export
#' @return maticový zápis redukované OTU tabulky
#'
#'

merge_taxonomy <- function(biom_file, OTUtab, taxonomy, NewOTU=FALSE){

  #nacteni taxonomie do pomocne tabulky
  a <- biom::observation_metadata(biom_file)
  if (methods::is(a)[1]=="list"){
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

    if(dim(pom)[1]!=dim(OTUtab)[1]){
      rownames(pom)<-names(a)
      pom <- pom[rownames(pom) %in% rownames(OTUtab),]
    }
  } else {
    pom <- a
  }


  #nahrada taxonomicke kategorie ciselnym ukazatelem
  if (taxonomy == "kingdom" | taxonomy == "Kingdom"){
    taxonomy <- 1
  } else if (taxonomy == "phylum" | taxonomy == "Phylum"){
    taxonomy <- 2
  } else if (taxonomy == "class" | taxonomy == "Class"){
    taxonomy <- 3
  } else if (taxonomy == "order" | taxonomy == "Order"){
    taxonomy <- 4
  } else if (taxonomy == "family" | taxonomy == "Family"){
    taxonomy <- 5
  } else if (taxonomy == "genus" | taxonomy == "Genus"){
    taxonomy <- 6
  } else if (taxonomy == "species" | taxonomy == "Species"){
    taxonomy <- 7
  }

  # nahrada taxonomie "k__", "p__", "c__", "o__", "f__", "g__", "s__" za NA
  pom <- as.data.frame(pom)
  vek <- sort(levels(as.factor(pom[,taxonomy])))

  if (any(nchar(vek)==3)){
    z <- (pom[,taxonomy]==vek[1])
    z[is.na(z)] <- FALSE
    pom[z==TRUE,taxonomy] <- NA
    pom[,taxonomy] <- factor(pom[,taxonomy])
    vek <- sort(levels(pom[,taxonomy]))
  }

  # nahrada "Unassigned" za NA
  if (any(pom[,taxonomy]=="Unassigned", na.rm = TRUE)){
    z <- (pom[,taxonomy]=="Unassigned")
    z[is.na(z)] <- FALSE
    pom[z==TRUE,taxonomy] <- NA
    pom[,taxonomy] <- factor(pom[,taxonomy])
    vek <- sort(levels(pom[,taxonomy]))
  }

  # rownames & colnames
  tax <- length(vek)
  redmat <- matrix(data = 0, nrow= (tax), ncol = dim(OTUtab)[2])
  rownames(redmat) <- vek
  colnames(redmat) <- colnames(OTUtab)

  # sumace
  k_NewOTU <- vector()
  for (i in 1:length(vek)){
    k <- which(pom[,taxonomy]==vek[i])
    k_NewOTU <- c(k_NewOTU,k)
    if (length(k)>1){
      redmat[i, ] <- colSums(OTUtab[k,])
    } else{
      redmat[i,] <- OTUtab[k,]
    }
  }

  # pridani OTU, ktera nebylo mozno sloucit s jinymi OTU
  if (NewOTU == TRUE){
    redmatt <- OTUtab[-k_NewOTU,]
    redmat <- rbind(redmat,redmatt)
  }

  OTUtab <- redmat
  return(OTUtab)
}
