#' @title Name adjust
#'
#' @description Funkce name_adjust slouží ke zkrácení automaticky QIIME vygenerovaného názvu taxonomických kategorií a OTU.
#'
#' @param object igraph object, maticový zápis OTU tabulky nebo adjacenční matice
#'
#' @export
#' @return igraph object, maticový zápis OTU tabulky nebo adjacenční matice se zkrácenými názvy
#'

name_adjust <- function(object){

  # objekt = graf
  if (igraph::is.igraph(object)==TRUE){
    radky <- igraph::vertex_attr(object)$name
    for (i in 1:length(radky)){
      if (sum(charmatch(c("k__", "p__", "c__", "o__", "f__", "g__", "s__"), substring(radky[i],1,3), nomatch = FALSE)) > 0){
        radky[i] <- substring(radky[i], 4)
      } else {
        break
      }
    }
    for (i in 1:length(radky)){
      if (sum(charmatch(c("D_0__", "D_1__", "D_2__", "D_3__", "D_4__", "D_5__", "D_6__"), substring(radky[i],1,5), nomatch = FALSE)) > 0){
        radky[i] <- substring(radky[i], 6)
      } else {
        break
      }
    }
    pom <- radky[substring(radky, 1, 16) == "New.ReferenceOTU"]
    radky[substring(radky, 1, 16) == "New.ReferenceOTU"] <- paste("New", substring(pom, 17))

    pom <- radky[substring(radky, 1, 24) == "New.CleanUp.ReferenceOTU"]
    radky[substring(radky, 1, 24) == "New.CleanUp.ReferenceOTU"] <- paste("CleanUp", substring(pom, 25))

    igraph::vertex_attr(object, "name") <- radky

    # objekt = adjacencni matice, OTU tabulka
  } else {

    pom <- identical(colnames(object), rownames(object))
    radky <- rownames(object)

    for (i in 1:length(radky)){
      if (sum(charmatch(c("k__", "p__", "c__", "o__", "f__", "g__", "s__"), substring(radky[i],1,3), nomatch = FALSE)) > 0){
        rownames(object)[i] <- substring(rownames(object)[i], 4)
      } else {
        break
      }
    }

    for (i in 1:length(radky)){
      if (sum(charmatch(c("D_0__", "D_1__", "D_2__", "D_3__", "D_4__", "D_5__", "D_6__"), substring(radky[i],1,5), nomatch = FALSE)) > 0){
        rownames(object)[i] <- substring(rownames(object)[i], 6)
      } else {
      break
      }
    }

    radky <- rownames(object)[substring(rownames(object), 1, 16) == "New.ReferenceOTU"]
    rownames(object)[substring(rownames(object), 1, 16) == "New.ReferenceOTU"] <- paste("New", substring(radky, 17))

    radky <- rownames(object)[substring(rownames(object), 1, 24) == "New.CleanUp.ReferenceOTU"]
    rownames(object)[substring(rownames(object), 1, 24) == "New.CleanUp.ReferenceOTU"] <- paste("CleanUp", substring(radky, 25))

    if (pom == TRUE){
      colnames(object) <- rownames(object)
    }

  }

return(object)

}
