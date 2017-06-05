# Create TunePair object
makeTunePair <- function(learner, param.set, tune.pair = NULL, id = NULL, overwrite = FALSE) {
  assertClass(learner, "Learner")
  assertClass(param.set, "ParamSet")
  if(is.null(id))
    id = paste0("tune.", learner$id)
  
  ls = list(learner = learner, param.set = param.set)
  if(is.null(tune.pair)){
    tune.pair = list()
    tune.pair[[id]] = ls
    attr(tune.pair, "class") <- "TunePair"
  } else {
    assertClass(tune.pair, "TunePair")
  
    if(id %in% names(tune.pair) & overwrite == FALSE){
      stop("tune.pair already contains id: \"", id, "\". Please specify a new id or set overwrite = TRUE.")
    } else {
      tune.pair[[id]] = ls
    }
  }
  
  return(tune.pair)
}

# Define tuning parameters for different learners
mlr.lrn = makeLearner("classif.randomForest", 
  predict.type = "prob", 
  fix.factors.prediction = TRUE)

mlr.params = makeParamSet(
  makeIntegerParam("ntree", lower = 1, upper = 500),
  makeIntegerParam("maxnodes", lower = 2, upper = 10)
)

LEARNER_PARAM_TUNE_PAIRS = makeTunePair(learner = mlr.lrn, param.set = mlr.params)




