#'@title Landau index
#'@name landau_index
#'
#'@description Function to obtain the linearity index developed by Landau (1951).
#'
#'@param dmatrix dyadic matrix
#'
#'@details The function landau_index is obtained by the following expression: h = (12/n^3-n)* sum(Va - ((n-1)/2))^2,
#'         where "h" is the linearity index, "n" is the total of animals, "Va" is the total of
#'         times that animal "i" dominated other animals.
#'
#'@return h index
#'
#'@importFrom dplyr group_by
#'@importFrom dplyr summarise
#'@importFrom dplyr n
#'@importFrom data.table data.table
#'@importFrom magrittr %>%
#'
#'@author Julia P. S. Valente, Matheus Deniz, Karolini T. de Sousa.
#'
#'@references Landau, H. G. (1951). On dominance relations and the structure of animal 
#'            societies: I. Effect of inherent characteristics. Bulletin of Mathematical 
#'            Biophysics, 13, 1-19.
#'
#'@seealso dmatrix
#'
#'@export
landau_index <- function(dmatrix){
  matrix <-  data.table(dmatrix)
  matrix <- subset(matrix, matrix$N > 0)
  count <- matrix %>%  
    group_by(matrix$actor) %>%
    summarise(reactor = n())
  count <- count[,2]
  n_anim <- matrix [!duplicated(matrix$actor), ]
  n_anim$anim <- 1
  n_anim <- sum(as.numeric(n_anim$anim), na.rm = TRUE)
  h.1 <- 12/(n_anim^3 - n_anim)
  h.2 <- sum((count - ((n_anim-1)/2))^2)
  h <- h.1 * h.2
  return(h)
}


#'@title Improved linearity index
#'@name improved_index
#'
#'@description Function to obtain the linearity index improved by de Vries (1995).
#'
#'@param dmatrix dyatic matrix
#'@param smatrix sociomatrix
#'
#'@details The function improved_index is obtained by the following expression: h' = h(6/(n^3-n)*u), 
#'         where "h'" is the linearity index, "n" is the total of animals, "u" is the unknown or 
#'         tied relationships. 
#'
#'@return h' index
#'
#'@importFrom dplyr group_by
#'@importFrom dplyr summarise
#'@importFrom dplyr n
#'@importFrom data.table data.table
#'@importFrom magrittr %>%
#' 
#'@author Julia P. S. Valente, Matheus Deniz, Karolini T. de Sousa.
#'
#'@references de Vries, H. (1995). An improved test of linearity in dominance hierarchies 
#'            containing unknown or tied relationships. Animal Behaviour, 50(5), 1375–1389. 
#'
#'@seealso dmatrix, smatrix
#'
#'@export
improved_index <- function(dmatrix, smatrix){
  transposed_matrix <- t(smatrix)
  matrix <-  data.table(dmatrix)
  matrix <- subset(matrix, matrix$N > 0)

  count <- matrix %>%
    group_by(matrix$actor) %>%
    summarise(reactor = n())
  count <- count[,2]
  n_anim <- matrix [!duplicated(matrix$actor), ]
  n_anim$anim <- 1
  n_anim <- sum(as.numeric(n_anim$anim), na.rm = TRUE)
  h.1 <- 12/(n_anim^3 - n_anim)
  h.2 <- sum((count - ((n_anim-1)/2))^2)
  h <- h.1 * h.2
  u <- apply(transposed_matrix, 2, function(x) sum(x == "0"))
  u <- sum(as.numeric(u), na.rm = TRUE)
  hl <- h + ((6/(n_anim^3-n_anim))*u)
  return(hl)
}
