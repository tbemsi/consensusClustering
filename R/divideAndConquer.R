divideAndConquer <- function(sampleID,
                             dataframe,
                             numberOfSweeps,
                             BurnIn,
                             seed = 1234){
  train <- dataframe %>% filter(sampleGroup == sampleID)
  runInfoObjTrain  <- profRegr(yModel="Bernoulli",
                               xModel="Discrete",
                               nSweeps=numberOfSweeps,
                               nClusInit=100,
                               nBurn=BurnIn,
                               seed = seed,
                               data=train,
                               output=paste0("PremiumOutput/newtrainOutput_",sampleID),
                               covNames = names(train)[2:(length(names(train)) - 1)],
                               reportBurnIn = TRUE,
                               excludeY = TRUE)
  zTrain <- fread(paste0("PremiumOutput/newtrainOutput_",sampleID,"_z.txt"), header = FALSE)
  zMatrixTrain <- as.matrix(zTrain)
  trainPSM <- makePSM(zMatrixTrain)
  train$Clusters <- maxpear(trainPSM)$cl

  clusterParamsTrain <- clusterParams(train)

  predictedClustersForTest <- clusterPredictions(dataWithLabels = train,
                                                 dataWithoutLabels = dataframe,
                                                 trainingParams = clusterParamsTrain$ClusterParams,
                                                 trainMixWeights = clusterParamsTrain$mixtureWeights)

  predictedClustersForTest
}
