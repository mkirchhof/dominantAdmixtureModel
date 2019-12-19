# dam - performs topic modeling by using the dominant admixture model
# Input:
#  A - term document matrix with relative frequencies per document. Rows are the
#      words, columns are the documents
#  alpha - Hyperparameter between 0 and 0.4 (each document needs one topic with
#          at least this proportion)
#  delta - Hyperparameter between 0 and 0.4 (for each topic l there must be at
#          least one document with 1 - delta in l's topic's proportion)
#  catchwordMinFreq - Hyperparameter, we assume that in the term-topic matrix,
#                     each topic's catchword will have a frequency bigger than or
#                     equal to catchwordMinFreq
#  r - integer, the desired rank of the decomposition (or in other words the 
#      number of topics)
#  algorithm - either "blum" or "kmeans". This refers to whether dominant topics
#              should be found using step 3 of the blum's algorithm or by kmeans
# Output:
#  list of four elements:
#   B - term-topic matrix where rows correspond to terms and columns
#       correspond to topics
#   dominantTopic - integer vector, giving the dominant topic of each document
#   pseudoCatchword - integer vector, giving index of the found pseudo-catchword
#                     for each topic
#   pseudoPureDocs - list of integer vectors. Each vector gives the pseudo-pure
#                    documents for one topic
dam <- function(A, alpha, delta, catchwordMinFreq, r, algorithm = "blum", 
                showProgress = TRUE){
  if(alpha <= 0 || delta <= 0 || r <= 0 || catchwordMinFreq <= 0 ||
     alpha > 0.4 || delta > 0.4 || catchwordMinFreq > 1){
    stop("bad choice of hyperparameters")
  }
  if(as.integer(r) != r){
    stop("r must be an integer")
  }
  r <- as.integer(r)
  if(!is.matrix(A)){
    stop("A must be a numeric matrix")
  }
  
  # extract some constants
  n <- ncol(A)
  d <- nrow(A)
  
  # Find the dominant topic of each document
  if(showProgress) cat("Finding dominant topics...\n")
  mu <- alpha * (1 - delta) * apply(A, 1, max)
  mu <- ifelse(mu >= catchwordMinFreq * alpha * (1 - (5 * delta) / 2), mu, Inf)
  AHat <- A >= mu
  if(algorithm == "blum"){
    # Pruning of AHat
    for(i in seq(d)){
      if(showProgress && i %% 100 == 0) cat(round(i/d * 100, 1), "%...")
      for(i2 in seq(nrow(AHat))[-i]){
        if(all(AHat[i, AHat[i2, ] == 1] == 1) & any(AHat[i2, ])){
          AHat[i, ] <- FALSE
          break
        }
      }
    }
    
    # Check that we now have exactly one dominant topic per document
    if(!all(colSums(AHat) == 1) || sum(rowSums(A) > 0) != r){
      stop("Hyperparameters do not lead to unique dominant topics")
    }
    
    AHat <- AHat[rowSums(AHat) > 0, ]
    domTopic <- apply(AHat, 2, which)
  } else if(algorithm == "kmeans"){
    domTopic <- kmeans(t(AHat), centers = r, iter.max = 200, nstart = 20, 
                       algorithm = "Lloyd")$cluster
  } else {
    stop("Unknown argument for 'algorithm'")
  }
  
  # Finding the term-topic matrix
  if(showProgress) cat("Calculating quantiles...\n")
  G <- matrix(nrow = d, ncol = r)
  B <- matrix(nrow = d, ncol = r)
  for(i in seq(d)){
    if(showProgress && i %% 100 == 0) cat(round(i/d * 100, 1), "%...")
    for(l in seq(r)){
      G[i, l] <- as.numeric(quantile(A[i, domTopic == l], 1 - delta / 4))
    }
  }
  
  # Find a valid pseudo catchword for each topic
  if(showProgress) cat("Finding pseudo catchwords...\n")
  pseudoCatchwordIndex <- numeric(r)
  for(l in seq(r)){
    if(showProgress) cat(round(l/r * 100, 1), "%...")
    for(i in seq(d)){
      if(G[i, l] >= (1 - (delta/2)) * catchwordMinFreq &
         all(G[i, -l] <= (1 - 2 * delta) * alpha * G[i, l])){
        pseudoCatchwordIndex[l] <- i
        break
      }
    }
  }
  if(!all(pseudoCatchwordIndex != 0)){
    stop("Hyperparameters do not lead to proper pseudo catchwords for each topic")
  }
  
  # Find Term Topic Matrix by averaging pseudo pure documents
  if(showProgress) cat("Calculating term topic matrix...\n")
  pseudoPureDocs <- list()
  for(l in seq(r)){
    if(showProgress) cat(round(l/r * 100, 1), "%...")
    pseudoPureDocIndex <- order(A[pseudoCatchwordIndex[l], ], decreasing = TRUE)[1:ceiling(delta * n / 4)]
    pseudoPureDocs[[l]] <- pseudoPureDocIndex
    B[, l] <- rowMeans(A[, pseudoPureDocIndex])
  }
  
  return(list(B = B, 
              dominantTopic = domTopic, 
              pseudoCatchword = pseudoCatchwordIndex,
              pseudoPureDocs = pseudoPureDocs))
}

