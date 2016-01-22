splitdf <- function(input, prop, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (is.data.frame(input)){
    index <- 1:nrow(input)
    trainindex <- sample(index, trunc(length(index)*prop))
    trainset <- input[trainindex, ]
    testset <- input[-trainindex, ]
  }else if (is.vector(input)){
    index<-1:length(input)  
    trainindex <- sample(index, trunc(length(index)*prop))
    trainset <- input[trainindex]
    testset <- input[-trainindex]
  }else{
    print("Input must be a dataframe or a vector")
  }
  list(trainset=trainset,testset=testset)
}
