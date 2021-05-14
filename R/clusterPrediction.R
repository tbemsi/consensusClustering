#' Title
#'
#' @param dataWithLabels -- data with cluster labels (ideally obtained from PReMiuM)
#' @param dataWithoutLabels -- data for which you want to predict cluster labels
#' @param trainingParams -- cluster parameters obtained from training set
#' @param trainMixWeights -- mixture weights obtained from training set
#'
#' @return maximum likelihood predictions for data without labels
#' @export
#'
#' @examples
clusterPredictions <- function(dataWithLabels,
                               dataWithoutLabels,
                               trainingParams,
                               trainMixWeights){
  posteriorProbs_predictingTest <- matrix(nrow = nrow(dataWithoutLabels),
                                          ncol = length(unique(dataWithLabels$Clusters)))
  for(i in 1:nrow(posteriorProbs_predictingTest))
  {
    currentDataForPrediction    <- dataWithoutLabels[i,2:(ncol(dataWithoutLabels)-2)]
    unnormalisedProbs_a         <- trainingParams[,as.logical(unlist(currentDataForPrediction)), drop = F]
    unnormalisedProbs_b         <- (1 - trainingParams)[,!as.logical(unlist(currentDataForPrediction)), drop = F]
    unnormalisedProbs                 <- exp(log(trainMixWeights) + rowSums(log(unnormalisedProbs_a)) + rowSums(log(unnormalisedProbs_b)))
    posteriorProbs_predictingTest[i,] <- unnormalisedProbs/sum(unnormalisedProbs)
  }
 apply(posteriorProbs_predictingTest, 1, which.max)
}


