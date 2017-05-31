testImplications <- function( covariance.matrix, sample.size ){
  library(ggm)
  tst <- function(i){ pcor.test( pcor(i,covariance.matrix), length(i)-2, sample.size )$pvalue }
  tos <- function(i){ paste(i,collapse=" ") }
  implications <- list(c("A","B"),
                       c("A","D","E"),
                       c("A","Car_ownership"),
                       c("A","Housing_tenure"),
                       c("A","affluence"),
                       c("A","car_use"),
                       c("B","E"),
                       c("B","Car_ownership"),
                       c("B","Housing_tenure"),
                       c("B","affluence"),
                       c("B","car_use"),
                       c("D","Car_ownership"),
                       c("D","Housing_tenure"),
                       c("D","affluence"),
                       c("D","car_use"),
                       c("E","Car_ownership"),
                       c("E","Housing_tenure"),
                       c("E","affluence"),
                       c("E","car_use"),
                       c("Car_ownership","Housing_tenure"),
                       c("Car_ownership","car_use"),
                       c("Housing_tenure","car_use"),
                       c("affluence","car_use"))
  data.frame( implication=unlist(lapply(implications,tos)),
              pvalue=unlist( lapply( implications, tst ) ) )
  
}