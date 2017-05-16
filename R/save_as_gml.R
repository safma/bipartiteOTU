#' @title Save as gml
#'
#' @description Funkce save_as_gml umožňuje převedení OTU tabulek, adjacenčních matic nebo igraph objektů do bipartitních grafů v gml formátu.
#'
#' @param object igraph object, maticový zápis OTU tabulky nebo adjacenční matice
#' @param name textová proměnná. Název nového gml souboru.
#'
#' @export
#'
#'

save_as_gml <- function(object, name = "graf.gml"){

  # objekt = igraph
  if (igraph::is.igraph(object)){
    igraph::V(object)$label <- igraph::V(object)$name
    object <- igraph::remove.vertex.attribute(object, "name")
    igraph::write_graph(object, file = name, format = c("gml"))

  # objekt = OTUtab, adjmatrix
  } else {
    pom <- identical(colnames(object), rownames(object))
    if (pom == FALSE){
      object <- adjacency_matrix(object)
    }

    # nastaveni jednoducheho stylu zavorek
    options(useFancyQuotes = FALSE)

    # vytvoreni pomocne promenne charakterizujici pocet OTU a vzorku
    for (i in 1:dim(object)[1]){
      if (sum(object[1:i,1:i]>0)){
        pom <- i
        break
      }
    }

    # zapis uzlu
    final_graph <- vector()
    final_graph <- paste("graph [")
    for (i in 1:(pom-1)){
      final_graph <- paste(final_graph, "\n", "node [", "\n", "id", i, "\n", "label", dQuote(colnames(object)[i]), "\n", "type 1", "\n", "]")
    }
    for (i in pom:dim(object)[1]){
      final_graph <- paste(final_graph, "\n", "node [", "\n", "id", i, "\n", "label", dQuote(colnames(object)[i]), "\n", "type 2", "\n", "]")
    }


    # z?pis hran
    for (i in pom:dim(object)[1]){
      a<-which(object[i,1:(pom-1)]>0)

      for (j in 1:length(a)){
        final_graph <- paste(final_graph, "\n", "edge [", "\n", "source", i, "\n", "target", a[j], "]")
      }
    }
    final_graph <- paste(final_graph, "\n", "]")
    final_graph <- noquote(strsplit(final_graph, "\n"))

    a <- noquote(unlist(final_graph))
    write(a, file=name)
  }
}
