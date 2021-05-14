#'
#'
#' \description{Inorder to carry out the divide-and-conquer consensus clustering, we need to
#' split a dataset into multiple subsamples. This function assigns points in
#' the dataset to different subsamples randomly.}
#'
#' @param dataframe -- dataframe upon which imputation of sample numbers is to be performed
#' @param numberOfSamples -- number of subsamples to be included in data
#'
#' @return dataframe with different datapoints assigned subsample labels
#' @export
#'
#' @examples
#' df <- imputeSampleNumbers(dataframe = iris, numberOfSamples = 3)
imputeSampleNumbers <- function(dataframe, numberOfSamples){
  sampleNumber <- sample(1:numberOfSamples, size = nrow(dataframe), replace = TRUE)
  dataframe <- dataframe %>% mutate(sampleGroup = sampleNumber)
}
