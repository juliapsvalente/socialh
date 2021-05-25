#'@title Identification of replacements between two animals
#'@name replacement
#'
#'@description Function to identify replacements between actor and reactor from
#'             electronic bins data.
#'
#'@param x dataset with electronic bins information.
#'@param sec interval (in seconds) between two different animals sequentially visited the
#'           same bin (feeder or drinker); 
#'
#'@details replacement is only applied for dataset with columns named as follows: equip_id (bin identification),
#'         animal_id (animal identification), IN (date - dd/mm/yyyy - and time - hh:mm:ss - when the animal entry in the bin),
#'         OUT (date - dd/mm/yyyy - and time - hh:mm:ss - when the animal left the bin).
#'
#'@return Replacement between two animals
#'
#'@importFrom utils tail
#'@importFrom utils head
#'@importFrom dplyr lag
#'
#'@author Julia P. S. Valente, Matheus Deniz, Karolini T. de Sousa.
#'
#'@examples 
#'x <- data.frame(equip_id = as.numeric("0001"), 
#'                animal_id = c(1,2,6,3,5,4,2,1,3,5,8,1,6,9,4,3,2,1,5,1))
#'
#'x$IN <-  c("01/08/2017 00:03:42","01/08/2017 00:05:26","01/08/2017 00:07:04","01/08/2017 00:08:15",
#'           "01/08/2017 00:10:35","01/08/2017 00:15:07","01/08/2017 00:18:13","01/08/2017 00:21:48",
#'           "01/08/2017 00:23:55","01/08/2017 00:30:14","01/08/2017 00:35:00","01/08/2017 00:38:11",
#'           "01/08/2017 00:39:05","01/08/2017 00:40:20","01/08/2017 00:42:08","01/08/2017 00:46:00",
#'           "01/08/2017 00:48:12","01/08/2017 00:49:40","01/08/2017 00:50:57","01/08/2017 00:52:36")
#'
#'x$OUT <-c("01/08/2017 00:05:24","01/08/2017 00:06:56","01/08/2017 00:08:12","01/08/2017 00:10:32",
#'          "01/08/2017 00:15:04","01/08/2017 00:18:10","01/08/2017 00:21:41","01/08/2017 00:23:53",
#'          "01/08/2017 00:30:10","01/08/2017 00:34:56","01/08/2017 00:37:32","01/08/2017 00:39:03",
#'          "01/08/2017 00:40:10","01/08/2017 00:41:51","01/08/2017 00:45:56","01/08/2017 00:48:10",
#'          "01/08/2017 00:49:36","01/08/2017 00:50:33","01/08/2017 00:52:32","01/08/2017 00:55:34")
#'
#'replace <- replacement(x,14)
#'
#'print(replace)
#'
#'@export
replacement <- function(x, sec){
  x$IN <- as.POSIXlt(x$IN, format="%d/%m/%Y %H:%M:%S")
  x$OUT <- as.POSIXlt(x$OUT, format="%d/%m/%Y %H:%M:%S")
  x <- x[
    order( x$OUT),
  ]
  x <- x[
    order( x$equip_id),
  ]
  x$dif <- c(NA, tail(x$IN , -1) - head(x$OUT, -1))
  x <- subset(x, x$dif>= 0)
  x$actor <- ifelse(x$dif <= sec & x$dif >= 1, x$animal_id, 0 )
  x$reactor <- ifelse(x$actor == x$animal_id, lag(x$animal_id), 0)
  x$reactor <- ifelse(x$actor == x$animal_id, lag(x$animal_id), 0)
  x$status <- ifelse(x$actor != x$reactor, "OK", 0)
  replacement <- subset(x, x$status == "OK")
  replacement <-  replacement[,c("actor","reactor")]
}
