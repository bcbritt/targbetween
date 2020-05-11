#' Retrieve Shortest Paths from One Source to Multiple Destinations
#'
#' This helper function retrieves the set of shortest paths connecting all
#' possible dyads that can be formed from a single \code{source} vertex to a
#' vector of \code{destination} vertices, for subsequent use by
#' \code{\link{compute_targbetween}}.
#'
#' This function should not be called directly. Instead, provide the network
#' and the set of dyads that you want to analyze to
#' \code{\link{targbetween_alldyads}}. That function calls
#' \code{\link{one_to_many}} as needed.
#'
#' Note that other functions that call \code{\link{one_to_many}} may reverse
#' \code{source} and \code{destination} in order to identify the shortest paths
#' from many source vertices to a single destination vertex. When this is done
#' for directed networks, the \code{mode} argument is set to \code{"in"} rather
#' than \code{"out"} as its value. (If the network is treated as undirected in
#' the analysis, \code{mode = "all"} regardless of whether \code{source} and
#' \code{destination} are reversed.)
#'
#' @param network An \code{igraph} network graph
#' @param source A single vertex or a vector of vertices in \code{network}
#' @param destination A vector of one or more vertices in \code{network}
#' @param mode A character value indicating the \code{mode} for
#'   \code{\link[igraph]{all_shortest_paths}} (\code{"in"}, \code{"out"}, or
#'   \code{"all"})
#' @return A list of shortest paths from the \code{source} vertex to each of the
#'   \code{destination} vertices.
#' @export
#' one_to_many

one_to_many <- function(network, source, destination, mode) {
   if(!igraph::is_igraph(network)) {
      stop("The 'network' argument that you provided is not a valid igraph network graph.")
   }
   if(!(source %in% igraph::V(network))) {
      stop("Not all vertices that you provided in the 'source' and 'destination' arguments are valid vertices in the network.")
   }
   if(length(destination) - sum(destination %in% igraph::V(network)) > 0) {
      stop("Not all vertices that you provided in the 'source' and 'destination' arguments are valid vertices in the network.")
   }

   paths <- igraph::all_shortest_paths(network, from = source, to = destination, mode = mode, weights = NULL) #This finds all shortest paths in the network from the specified source vertex to the specified destination vertex (or vertices).
   return(paths$`res`)
}

#' Retrieve Shortest Paths for Assorted Dyads
#'
#' This helper function retrieves the set of shortest paths connecting each dyad
#' of vertices specified in a two-column data.frame (column 1 = source,
#' column 2 = destination), for subsequent use by
#' \code{\link{compute_targbetween}}.
#'
#' This function should not be called directly. Instead, provide the
#' \code{network} and the set of \code{dyads} that you want to analyze to
#' \code{\link{targbetween}}. That function calls \code{\link{assorted_dyads}}
#' as needed.
#'
#' WARNING: This function may be extremely slow when the number of rows in
#' \code{dyads} is large, as \code{\link[igraph]{all_shortest_paths}} must be
#' separately run for each individual dyad. If all source vertices correspond to
#' the same set of \code{destination} vertices,
#' \code{\link{targbetween_alldyads}} should be used, as this calls
#' \code{\link{one_to_many}}, instead of using \code{\link{targbetween}} to call
#' \code{\link{assorted_dyads}}. The \code{\link{targbetween_alldyads}} function
#' takes a vector of \code{source} vertices and a vector of \code{destination}
#' vertices as arguments, and it is either \code{length(source)} or
#' \code{length(destination)} times faster than \code{\link{targbetween}},
#' whichever is larger.
#'
#' @param network An \code{igraph} network graph
#' @param dyads A two-column data.frame, with two vertices in each row,
#'   indicating the dyads to be considered in the analysis
#' @param directed A boolean value indicating whether the \code{network} should
#'   be treated as directed (default = \code{FALSE})
#' @return A list of shortest paths between the pairs of vertices indicated in
#'   each row of \code{dyads}.
#' @export
#' assorted_dyads

assorted_dyads <- function(network, dyads, directed=FALSE) {
   #WARNING: MAY BE EXTREMELY SLOW FOR LARGE NUMBERS OF DYADS, AS igraph::all_shortest_paths() MUST BE RUN ON EACH INDIVIDUAL EDGE
   dyads <- data.frame(dyads)
   if(!igraph::is_igraph(network)) {
      stop("The 'network' argument that you provided is not a valid igraph network graph.")
   }
   if(length(unlist(dyads)) - sum(unlist(dyads) %in% igraph::V(network)) > 0) {
      stop("Not all vertices in the 'dyads' argument that you provided are valid vertices in the network.")
   }

   if(directed) {
      mode = "out"
   } else {
      mode = "all"
   }
   paths <- NA
   for(i in 1:nrow(dyads)) {
      if(i == 1) {
         paths <- igraph::all_shortest_paths(network, from = dyads[[1]][[i]], to = dyads[[2]][[i]], mode = mode, weights = NULL)$`res` #This finds all shortest paths in the network from the specified source vertex to the specified destination vertex.
      } else {
         paths <- append(paths, igraph::all_shortest_paths(network, from = dyads[[1]][[i]], to = dyads[[2]][[i]], mode = mode, weights = NULL)$`res`) #This finds all shortest paths in the network from the specified source vertex to the specified destination vertex.
      }
   }
   return(paths)
}

#' Compute Targeted Betweenness Centrality from Shortest Paths
#'
#' This helper function takes the list of shortest paths, as retrieved by
#' either \code{\link{one_to_many}} or \code{\link{assorted_dyads}}, and uses it
#' to finish the computation of targeted betwenness centrality.
#'
#' This function should not be called directly. Instead, provide the
#' \code{network} and the set of \code{dyads} that you want to analyze to
#' \code{\link{targbetween_alldyads}} or \code{\link{targbetween}}. Each of
#' those functions calls \code{\link{compute_targbetween}} as needed.
#'
#' @param network An \code{igraph} network graph
#' @param paths Shortest paths retrieved by
#'   \code{\link[igraph]{all_shortest_paths}}
#' @param num_dyads Number of dyads used in the analysis that generated
#'   \code{paths}
#' @param update A numeric value indicating how many loop iterations should
#'   elapse between progress updates (default = \code{0}, which suppresses
#'   output)
#' @return A data.frame with the results for targeted betweenness centrality
#' @importFrom "utils" "flush.console"
#' @export
#' compute_targbetween

compute_targbetween <- function(network, paths, num_dyads, update = 0) {
   if(is.null(names(igraph::V(network)))) {
      network <- igraph::set_vertex_attr(network, "name", value = as.character(1:length(igraph::V(network))))
   }
   vertices <- names(igraph::V(network))
   all_paths <- vector(mode = "list", length = num_dyads)
   dyad_number <- 1
   path_number <- 1
   loop_length <- length(paths)
   for(i in 1:loop_length) {
      if(update > 0) {
         if((i %% update) == 0) {
            print(paste("Step 1 of 2, starting iteration ", i, " of ", loop_length, sep=""))
            flush.console()
         }
      }
      if(i == 1) {
         if(length(paths[i][[1]]$name) > 2) {
            all_paths[[dyad_number]][[path_number]] <- paths[i][[1]]$name[2:(length(paths[i][[1]]$name)-1)] #Copy the names for the vertices along this path into all_paths, except for the source and destination of the path, as these are not BETWEEN the endpoints of the path, they ARE the endpoints
         } else { #If the path has only two vertices (the source and the destination) then there are no vertices between them
            all_paths[[dyad_number]][[path_number]] <- numeric(0)
         }
      } else {
         if((paths[i][[1]]$name[1] == paths[i-1][[1]]$name[1]) & (paths[i][[1]]$name[length(paths[i][[1]]$name)] == paths[i-1][[1]]$name[length(paths[i-1][[1]]$name)])) { #If the origin and destination of this path are the same as the previous path...
            path_number <- path_number + 1 #...Then it's the same dyad, so add this path to the same top-level list element in all_paths
         } else { #Otherwise, the origin and destination are NOT the same as the previous path...
            dyad_number <- dyad_number + 1 #...So it's a different dyad, which means we should jump to the next top-level list element in all_paths
            path_number <- 1
         }
         if(length(paths[i][[1]]$name) > 2) {
            all_paths[[dyad_number]][[path_number]] <- paths[i][[1]]$name[2:(length(paths[i][[1]]$name)-1)] #Copy the names for the vertices along this path into all_paths, except for the source and destination of the path, as these are not BETWEEN the endpoints of the path, they ARE the endpoints
         } else { #If the path has only two vertices (the source and the destination) then there are no vertices between them
            all_paths[[dyad_number]][[path_number]] <- numeric(0)
         }
      }
   }
   loop_length <- length(all_paths)
   targeted_betweenness <- rep(0, length(vertices)) #Creates a zero vector with length equal to the total number of vertices in the network
   for(i in 1:loop_length) {
      if(update > 0) {
         if((i %% update) == 0) {
            print(paste("Step 2 of 2, starting iteration ", i, " of ", loop_length, sep=""))
            flush.console()
         }
      }
      all_bridges <- unlist(all_paths[i]) #Get the list of all vertices that fall along shortest paths for this dyad, including duplicates (for vertices that fall along multiple shortest paths)
      for(j in match(all_bridges, vertices)) {
         targeted_betweenness[j] <- targeted_betweenness[j] + (1 / length(all_paths[[i]])) #For each time a bridge vertex appears, increment its betweenness centrality by 1 divided by the number of shortest paths this dyad has
      }
   }
   results <- cbind(vertices, targeted_betweenness)
   return(results)
}

#' Targeted Betweenness Centrality for Specified Source and Destination Vertices
#'
#' This function computes targeted betweenness centrality for one or more
#' sources and one or more destinations. The set of dyads to be evaluated
#' includes all possible pairs of source vertices and destination vertices from
#' the specified \code{source} and \code{destination} vectors.
#'
#' @param network An \code{igraph} network graph
#' @param source A vector of one or more vertices in \code{network}
#' @param destination A vector of one or more vertices in \code{network}
#' @param filename A character value indicating the file location to output
#'   results (default = \code{NULL})
#' @param directed A boolean value indicating whether the \code{network} should
#'   be treated as directed (default = \code{FALSE})
#' @param update A numeric value indicating how many loop iterations should
#'   elapse between progress updates (default = \code{0}, which suppresses
#'   output)
#' @return A data.frame with the results for targeted betweenness centrality
#' @importFrom "utils" "write.csv"
#' @examples
#' my_network <- igraph::erdos.renyi.game(20, 0.5, directed = FALSE)
#' single_source <- igraph::V(my_network)[1]
#' single_destination <- igraph::V(my_network)[20]
#' multiple_sources <- igraph::V(my_network)[1:5]
#' multiple_destinations <- igraph::V(my_network)[6:10]
#' results1 <- targbetween_alldyads(my_network, single_source,
#'   single_destination)
#' results2 <- targbetween_alldyads(my_network, single_source,
#'   multiple_destinations, directed=FALSE)
#' results3 <- targbetween_alldyads(my_network, multiple_sources,
#'   single_destination, directed=TRUE)
#' results4 <- targbetween_alldyads(my_network, multiple_sources,
#'   multiple_destinations, directed=FALSE, update=1000)
#' results4
#' #       vertices targeted_betweenness
#' #  [1,] "1"      "0.533333333333333" 
#' #  [2,] "2"      "0.866666666666667" 
#' #  [3,] "3"      "0.5"               
#' #  [4,] "4"      "0.291666666666667" 
#' #  [5,] "5"      "1.11666666666667"  
#' #  [6,] "6"      "1.86666666666667"  
#' #  [7,] "7"      "0.541666666666667" 
#' #  [8,] "8"      "0.291666666666667" 
#' #  [9,] "9"      "0"                 
#' # [10,] "10"     "1.36666666666667"  
#' # [11,] "11"     "0.991666666666667" 
#' # [12,] "12"     "2.03333333333333"  
#' # [13,] "13"     "0.366666666666667" 
#' # [14,] "14"     "0.458333333333333" 
#' # [15,] "15"     "1.15833333333333"  
#' # [16,] "16"     "0.75"              
#' # [17,] "17"     "0"                 
#' # [18,] "18"     "1.20833333333333"  
#' # [19,] "19"     "0.866666666666667" 
#' # [20,] "20"     "0.791666666666667" 
#' @export
#' targbetween_alldyads

targbetween_alldyads <- function(network, source, destination, filename=NULL, directed=FALSE, update=0) {
   if(!is.null(filename)) {
      if(!dir.exists(dirname(filename))) {
         stop("The 'filename' argument that you provided does not point to a valid directory.")
      }         
   }
   paths <- NA
   if(length(source) > length(destination)) { #If there are fewer destination vertices than source vertices, then it will be faster to iterate on the destinations
      if(directed) {
         mode="in"
      } else {
         mode="all"
      }
      for(i in 1:length(destination)) {
         if(i == 1) {
            paths <- one_to_many(network, destination[i], source, mode)
         } else {
            paths <- append(paths, one_to_many(network, destination[i], source, mode))
         }
      }
   } else {
      if(directed) {
         mode="out"
      } else {
         mode="all"
      }
      for(i in 1:length(source)) {
         if(i == 1) {
            paths <- one_to_many(network, source[i], destination, mode)
         } else {
            paths <- append(paths, one_to_many(network, source[i], destination, mode))
         }
      }
   }
   results <- compute_targbetween(network, paths, (length(source)*length(destination)), update)
   if(!is.null(filename)) {
      write.csv(results, file = filename)
   }
   return(results)
}

#' Targeted Betweenness Centrality for Specified Dyads
#'
#' This function computes targeted betweenness centrality for a specified
#' set of dyads. \code{\link{targbetween}} is more flexible than
#' \code{\link{targbetween_alldyads}}, allowing different source vertices to
#' correspond to different destination vertices.
#'
#' WARNING: This function calls \code{\link{assorted_dyads}}, which may be
#' extremely slow when the number of rows in \code{dyads} is large, as
#' \code{\link[igraph]{all_shortest_paths}} must be separately run for each
#' individual dyad. If all \code{source} vertices correspond to the same set of
#' \code{destination} vertices, \code{\link{targbetween_alldyads}} should be
#' used instead of \code{\link{targbetween}}. The
#' \code{\link{targbetween_alldyads}} function takes a vector of \code{source}
#' vertices and a vector of \code{destination} vertices as arguments, and it is
#' either \code{length(source)} or \code{length(destination)} times faster than
#' \code{\link{targbetween}}, whichever is larger.
#'
#' @param network An \code{igraph} network graph
#' @param dyads A two-column data.frame, with two vertices in each row,
#'   indicating the dyads to be considered in the analysis
#' @param filename A character value indicating the file location to output
#'   results (default = \code{NULL})
#' @param directed A boolean value indicating whether the \code{network} should
#'   be treated as directed (default = \code{FALSE})
#' @param update A numeric value indicating how many loop iterations should
#'   elapse between progress updates (default = \code{0}, which suppresses
#'   output)
#' @return A data.frame with the results for targeted betweenness centrality
#' @importFrom "utils" "write.csv"
#' @examples
#' my_network <- igraph::erdos.renyi.game(20, 0.5, directed = FALSE)
#' various_dyads <- cbind(c(1,1,2,2,3,3,4,4,5,5),c(3,4,1,8,18,20,1,15,3,10))
#' results <- targbetween(my_network, various_dyads, directed=FALSE)
#' results
#' #       vertices targeted_betweenness
#' #  [1,] "1"      "0.2"               
#' #  [2,] "2"      "0"                 
#' #  [3,] "3"      "0.166666666666667" 
#' #  [4,] "4"      "0"                 
#' #  [5,] "5"      "0.2"               
#' #  [6,] "6"      "0.166666666666667" 
#' #  [7,] "7"      "0.166666666666667" 
#' #  [8,] "8"      "0.166666666666667" 
#' #  [9,] "9"      "0"                 
#' # [10,] "10"     "0.166666666666667" 
#' # [11,] "11"     "0.166666666666667" 
#' # [12,] "12"     "0.166666666666667" 
#' # [13,] "13"     "0.166666666666667" 
#' # [14,] "14"     "0.166666666666667" 
#' # [15,] "15"     "0.366666666666667" 
#' # [16,] "16"     "0.166666666666667" 
#' # [17,] "17"     "0"                 
#' # [18,] "18"     "0"                 
#' # [19,] "19"     "0.2"               
#' # [20,] "20"     "0.366666666666667" 
#' @export
#' targbetween

targbetween <- function(network, dyads, filename=NULL, directed=FALSE, update=0) {
   #WARNING: USES assorted_dyads(), WHICH MAY BE EXTREMELY SLOW FOR LARGE NUMBERS OF DYADS, AS igraph::all_shortest_paths() MUST BE RUN ON EACH INDIVIDUAL EDGE
   #If all source vertices correspond to the same set of destination vertices, targbetween_alldyads() is much more efficient than targbetween()
   if(!is.null(filename)) {
      if(!dir.exists(dirname(filename))) {
         stop("The 'filename' argument that you provided does not point to a valid directory.")
      }         
   }
   paths <- assorted_dyads(network, dyads, directed)
   results <- compute_targbetween(network, paths, nrow(dyads), update)
   if(!is.null(filename)) {
      write.csv(results, file = filename)
   }
   return(results)
}
