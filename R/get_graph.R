#' @title Get graph
#'
#' @description Funkce get_graph vytváří z OTU tabulek nebo adjacenčních matic igraph objekty, které jsou také rovnou vykresleny.
#'
#' @param object maticový zápis OTU tabulky nebo adjacenční matice
#' @param weighted logická proměnná. Přednastavená volba weighted = TRUE iniciuje váhování hran. Nastavení weighted = FALSE odstraní váhování hran.
#' @param community textová proměnná. Metoda detekce komunit. Možné volby jsou: walktrap, leading_eigen, label_prop,
#' edge_betweennes, multilevel, optimal
#' @param bipartite_projection logická proměnná. Umožňuje využití projekce bipartitního grafu pro detekci komunit. Volby bipartite_projection = 1 a bipartite_projection = 2
#' vedou k využití jedné ze dvou bipartitních projekcí, volby bipartite_projection = FALSE zakáže tvorbu bipartitních projekcí.
#'
#'
#' @export
#' @return objekt igraph
#'

get_graph <- function(object, weighted = TRUE, community = "walktrap", bipartite_projection = FALSE){

  #sjednoceni objektu na adjacencni matici
  pom <- identical(colnames(object), rownames(object))

  if (pom == FALSE){
    object <- adjacency_matrix(object)
  }

  # vytvoreni grafu
  graf <- igraph::graph_from_adjacency_matrix(object, mode = "undirected", weighted = TRUE)

  # prirazeni partity
  partity <- igraph::bipartite_mapping(graf)
  igraph::vertex_attr(graf, "type") <- as.numeric(partity$type)

  # vahovani
  if (weighted == TRUE){
    igraph::edge_attr(graf, "weight") <- 10*igraph::E(graf)$weight/(max(igraph::E(graf)$weight))+2
  } else {
    graf <- igraph::delete_edge_attr(graf, "weight")
  }

  if (!is.null(community)){

    # projekce P
    if (bipartite_projection == 1){
      grafi <- igraph::bipartite_projection(graf)
      grafi <- grafi$proj1
      
      if (weighted == FALSE){
        grafi <- igraph::delete_edge_attr(grafi, "weight")
      }
      
    } else if (bipartite_projection == 2){
      grafi <- igraph::bipartite_projection(graf)
      grafi <- grafi$proj2
      
      if (weighted == FALSE){
        grafi <- igraph::delete_edge_attr(grafi, "weight")
      }
      
    } else {
      grafi <- graf
    }

    # detekce komunit
    if (community == "walktrap" | community == "Walktrap"){ ##
      komunity <- igraph::walktrap.community(grafi, weights = igraph::E(grafi)$weight)
      print(max(komunity$modularity))

    } else if (community == "leading_eigen" | community == "Leading_eigen"){ ##
      komunity <- igraph::leading.eigenvector.community(grafi, weights = igraph::E(grafi)$weight)
      print(max(komunity$modularity))

    } else if (community == "label_prop" | community == "Label_prop"){ ##
      komunity <- igraph::label.propagation.community(grafi, weights = igraph::E(grafi)$weight)
      print(max(komunity$modularity))

    } else if (community == "edge_betweennes" | community == "Edge_betweennes"){
      komunity <- igraph::edge.betweenness.community(grafi, weights = igraph::E(grafi)$weight)
      print(max(komunity$modularity))

    } else if (community == "multilevel" | community == "Multilevel"){
      komunity <- igraph::multilevel.community(grafi, weights = igraph::E(grafi)$weight)
      print(max(komunity$modularity))

    } else if (community == "optimal" | community == "Optimal"){
      komunity <- igraph::optimal.community(grafi, weights = igraph::E(grafi)$weight)
      print(max(komunity$modularity))
    }


    # spojeni projekce P a S
    if (bipartite_projection == 1){
      pom <- igraph::V(graf)$name[igraph::vertex_attr(graf, "type")==1]
      komunity <- igraph::membership(komunity)
      a <- length(komunity)
      komunity[(1+a):(length(pom)+a)] <- 0
      names(komunity)[(1+a):(length(pom)+a)] <- pom
      igraph::vertex_attr(graf, "community") <- komunity

    } else if (bipartite_projection == 2){
      pom <- igraph::V(graf)$name[igraph::vertex_attr(graf, "type")==0]
      komunity <- igraph::membership(komunity)
      a <- length(komunity)
      komunity[(1+a):(length(pom)+a)] <- 0
      names(komunity)[(1+a):(length(pom)+a)] <- pom
      igraph::vertex_attr(graf, "community") <- c(komunity[(a+1):length(komunity)], komunity[1:a])

    } else {
      igraph::vertex_attr(graf, "community") <- igraph::membership(komunity)
    }
  }

  # valence vrcholu
  igraph::vertex_attr(graf, "degree") <- igraph::degree(graf)

  # vykresleni grafu
  if (is.null(community)){
    barva <- igraph::V(graf)$type
  } else {
    barva <- igraph::V(graf)$community
  }

  if (igraph::vcount(graf)>=500){
    graphics::plot(graf, vertex.color = barva, edge.width=igraph::E(graf)$weight, vertex.size = 45*igraph::V(graf)$degree/max(igraph::V(graf)$degree)+5)
  } else {
    igraph::tkplot(graf, vertex.color = barva, edge.width=igraph::E(graf)$weight, vertex.size = 45*igraph::V(graf)$degree/max(igraph::V(graf)$degree)+5)
  }

  return(graf)
}
