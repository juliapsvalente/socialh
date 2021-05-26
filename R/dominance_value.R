#'@title Dominance value
#'@name dvalue
#'
#'@description Function to obtain the dominance value, social rank and hierarchy
#'             from Sij dyadic relationship matrix.
#'
#'@param dmatrix Sij dyadic relationship matrix
#'
#'@details The social categories (rank and hierarchy) are define according to dominance value and 
#'         is obtained by the following expression: SH =(|Distance between highest(+ X) and lowest(- Y) dominance value| + 1)
#'         /(2 or 3), where "SH" is the rank or hierarchy. The rank (high and lower) and social 
#'         category (dominant, intermediate and subordinate)are determined assigned according to 
#'         dominance value. The choice for divide the group by rank or social category depends of 
#'         the study objective. Both rank and social category are estimated by the distance between
#'         the highest (+ X) and the lowest (- Y) dominance value, plus 1 (corresponds to the dominance
#'         value zero), which determines the number of points in the range.
#'          
#'
#'@return rank and social dominance
#'
#'@author Julia P. S. Valente, Matheus Deniz, Karolini T. de Sousa.
#'
#'@references Coimbra, P. A. D., Machado Filho, L. C. P., & Hötzel, M. J. (2012). Effects of social dominance,
#'            water trough location and shade availability on drinking behaviour of cows on pasture. Applied 
#'            Animal Behaviour Science, 139(3-4), 175-182.
#'            
#'@importFrom data.table data.table
#'@importFrom stats na.omit
#'
#'@examples 
#'
#'x <- matrix(c(0,-1,1,0,-1,1,0,0,1,0,-1,-1,0,1,0,0,
#'              -1,1,0,-1,1,-1,0,0,0,1,1,0,-1,0,0,0,
#'              1,0,-1,1,0,0,1,0,-1,-1,1,0,0,0,0,1,
#'              0,0,0,0,-1,0,0,0,0,0,0,0,0,-1,0,0),
#'              nrow=8,byrow=TRUE,)
#'              
#'colnames(x) <- c(1,2,3,4,5,6,8,9)
#'
#'rownames(x) <- c(1,2,3,4,5,6,8,9)
#'
#'dominance <- dvalue(x)
#'
#'print(dominance)
#'
#
#'@export
dvalue <- function(dmatrix){
  sum. <- colSums(dmatrix)
  sum. <- data.table(sum., colnames (dmatrix))
  colnames(sum.) <- c("dominance_value", "animal_id")
  sum. <- sum.[,c(2,1)]


  min. <- min(sum.$dominance_value)
  max. <- max(sum.$dominance_value)

  interval <- seq(from = min., to = max.)
  interval <- data.table(interval)
  colnames(interval) <- "dominance_value"

  add <-  merge(sum.,interval, all = T, )

  social_categories <- levels(cut(interval$dominance_value, breaks = 3,))

  scale <- data.table(cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", social_categories) ),
                                    upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", social_categories) )))
  scale.1 <- scale[1,]
  scale.2 <- scale[2,]
  scale.3 <- scale[3,]


  add$social_hierarchy <- "subordinate"
  add$social_hierarchy[add$dominance_value> scale.2$lower & add$dominance_value <scale.2$upper] <- "intermediate"
  add$social_hierarchy[add$dominance_value >= scale.3$lower] <- "dominant"

  social_rank <- levels(cut(interval$dominance_value, breaks = 2,))
  scale2 <- data.table(cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", social_rank) ),
                                    upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", social_rank) )))
  scale2.1 <- scale2[1,]
  add$social_rank <- "lower"
  add$social_rank[add$dominance_value >= scale2.1$upper] <- "high"
  add <- na.omit(add)
}


