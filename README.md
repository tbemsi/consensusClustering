# Divide-and-conquer Consensus clustering for large datasets

This package extends prfile regression to larger datasets using a maximum-likelihood approach. 
It does this using the following procedure:

1. breaking the main dataset into smaller subsamples,
2. performing bayesian profile regression ono the subsamples
3. obtain cluster parameters from the cluster
4. use a maximum-likelihood approach to predict cluster labels for the rest of the dataset
