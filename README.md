# Machine-Learning-in-R
This repository contains implementations of various machine learning algorithms written in R. The current algorithms included are:

1. **Linear Discriminant Analysis (LDA)**
   - Function: `get_LDA2()`
2. **k-means Clustering**
   - Standard k-means
     - Function: `get_kmeans()`
   - Optimized k-means using dynamic programming for one-dimensional clustering
     - Function: `optimal_kmeans()`

The heuristic k-means algorithm, widely used for cluster analysis, does not guarantee optimality. The results depend on the initial choice of cluster centers, which can lead to different outcomes. To overcome this limitation, I implement an optimized k-means algorithm using a dynamic programming approach for one-dimensional clustering. 

This implementation is based on the paper: [Optimal k-means Clustering in One Dimension by Dynamic Programming](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5148156/).
