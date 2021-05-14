
#' Title
#' @title clusterParams
#'
#' @param data \code{dataframe} -- dataframe with cluster labels
#'
#' @return A \code{list} of 2 elements:
#' \describe{
#' \item{ClusterParams}{\code{numeric} -- the estimated cluster parameters.}
#' \item{mixtureWeights}{\code{numeric} -- the estimated mixture weights for the clusters.}
#' }
#' @export
#'
#' @examples
#'
clusterParams <- function(data){
  output <- vector(mode = "list")
  mixtureWeights <- vector(mode = "numeric", length = length(unique(data$Clusters)))
  clusterParameters <- matrix(nrow = length(unique(data$Clusters)), ncol = ncol(data) - 3)
  counter <- 1
  for (k in unique(data$Clusters)){
    currentClusterData <- data[data$Clusters == k, 2:(ncol(data)-2)]
    mixtureWeights[counter] <- nrow(currentClusterData)/nrow(data)
    clusterParameters[counter, ] <- colSums(currentClusterData)/nrow(currentClusterData)
    counter <- counter + 1
  }
  output$ClusterParams <- clusterParameters
  output$mixtureWeights <- mixtureWeights
  output
}
