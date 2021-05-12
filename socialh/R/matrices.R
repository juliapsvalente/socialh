#'@title Sociomatrix 
#'@name smatrix
#'
#'@description Function to obtain the square matrix contained dyadic frequency of
#'             dominance-related behaviors (actor and reactor).
#'
#'@param x Replacement or agonistic interaction data table.
#'
#'@details The fuction smatrix is only applied for data set with columns named as follows: actor and reactor.
#'         The function form a square matrix, in which the number of "n" actors is also the 
#'         number of "n" reactors.
#'         
#'@return Sociomatrix
#'
#'@author Julia P. S. Valente, Matheus Deniz, Karolini T. de Sousa.
#'
#'@importFrom stats xtabs
#'
#'@examples             
#'x <- data.frame(actor = c(6,3,5,4,2,1,3,5,8,6,9,3,2,1,1),
#'                reactor = c(2,6,3,5,4,2,1,3,5,1,6,4,3,2,5))                
#'                                    
#'sociomatrix <- smatrix(x)                                                
#'                                                            
#'print(sociomatrix)
#'
#' @export
smatrix <- function(x){
  x <- x[with(x,order(actor)),]
  x$actor <-  factor(x$actor,levels=union(x$actor,x$reactor))
  x$reactor <-  factor(x$reactor,levels=union(x$actor,x$reactor))
  matrix <- (xtabs(~reactor+actor, x))
}

#'@title Sij dyadic relationship matrix 
#'@name dmatrix
#'
#'@description Function to obtain the Sij dyadic dominance relationship from an sociomatrix.
#'
#'@param smatrix sociomatrix
#'
#'@details The dyadic relationship is obtained by the following expression: 
#'         Sij = (Xij - Xji) / (|Xij - Xji|), where Sij is the social status of the ith animal relative 
#'         to the jth animal; Xij is the number of times the animal i won the animal j; 
#'         Xji is the number of times the animal j won the animal i.
#'
#'@return  Dyadic matrix
#'
#'@author Julia P. S. Valente, Matheus Deniz, Karolini T. de Sousa.
#'
#'@references Kondo, S., & Hurnik, J. F. (1990). Stabilization of social hierarchy in dairy cows.
#'            Applied Animal Behaviour Science, 27(4), 287-297.
#'
#'@examples 
#'            
#'x <- matrix(c(0,0,1,0,0,1,0,0,2,0,0,0,0,1,0,0,0,1,0,0,2,
#'              0,0,0,0,1,1,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,
#'              1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
#'              nrow=8,byrow=TRUE,)
#'colnames(x) <- c(1,2,3,4,5,6,8,9)
#'
#'rownames(x) <- c(1,2,3,4,5,6,8,9)
#'
#'dyadic <- dmatrix(x)
#'
#'print(dyadic)
#'                                                                                    
#'@export
dmatrix <- function(smatrix){
  transposed_matrix <- t(smatrix)
  wins_losses <- (smatrix - transposed_matrix)/abs(smatrix - transposed_matrix)
  wins_losses[is.nan(wins_losses)] <- 0
  return( wins_losses)
}
