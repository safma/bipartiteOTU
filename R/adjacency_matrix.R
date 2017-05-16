#' @title Adjacency matrix
#'
#' @description Funkce adjacency_matrix převede igraph objekt nebo OTU tabulku do adjacenční matice.
#'
#' @param object igraph object nebo maticový zápis OTU tabulky
#'
#' @export
#' @return maticový zápis adjacenční matice
#'
#'


adjacency_matrix <- function(object){

  if (igraph::is.igraph(object)){
    adjmatrix <- as.matrix(igraph::get.adjacency(object, attr = igraph::E(object)$weight))

  } else {
    adjmatrix <- methods::rbind2(methods::cbind2(matrix(0, nrow = dim(object)[1], ncol = dim(object)[1]), object),
                        methods::cbind2(t(object), matrix(0, nrow = dim(object)[2], ncol = dim(object)[2])))

    colnames(adjmatrix) <- c(rownames(object), colnames(object))
  }

  return(adjmatrix)

}
