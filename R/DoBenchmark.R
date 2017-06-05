if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, ggplot2, batchtools, mlrMBO, mlr)

setwd("D:/03_GitHub/02_Overfitting_HyperparamTuning")
for(f in list.files(pattern = "^0.*[.R]$")){
  source(f)
}

# Create registry -------------------------------------------------------------------------------------------------
unlink("./registry", recursive = TRUE)
reg = makeExperimentRegistry(seed = 100L, 
                             source = list.files(pattern = "^0.*[.R]$"),
                             packages = c("ParamHelpers", "cluster", "lhs", "mlrMBO"))
#reg$cluster.functions = makeClusterFunctionsSocket(1)


addProblem("PimaDiabetes", data = pid.task, reg = reg)


tuneLearnerParamPair <- function(job, data, instance, inner_cv_iter, outer_cv_iter, hypParam_tune_control, tune.pair.name) {
  mlr.task = data

  res_inner = makeResampleDesc("CV", iters = inner_cv_iter)
  res_outer = makeResampleDesc("CV", iters = outer_cv_iter)
  
  mlr.wrap.lrn = makeTuneWrapper(LEARNER_PARAM_TUNE_PAIRS[[tune.pair.name]]$learner,
    resampling = res_inner,
    measures = getDefaultMeasure(data),
    par.set = LEARNER_PARAM_TUNE_PAIRS[[tune.pair.name]]$param.set,
    control = get(hypParam_tune_control))
    
  mlr.mod = resample(mlr.wrap.lrn, 
                      mlr.task, 
                      resampling = res_outer, 
                      measures = getDefaultMeasure(mlr.task), 
                      extract = getTuneResult)
  
  return(list(mlr.mod = mlr.mod))
}

addAlgorithm("tuneLearnerParamPair", fun = tuneLearnerParamPair, reg = reg)

hyper_Parameter_random <- makeTuneControlRandom()

algo.designs = list()
algo.designs$tuneLearnerParamPair = expand.grid(inner_cv_iter = c(3L),
  outer_cv_iter = c(3L),
  hypParam_tune_control = c("hyper_Parameter_random"),
  tune.pair.name = names(LEARNER_PARAM_TUNE_PAIRS),
  stringsAsFactors = FALSE)

addExperiments(algo.designs = algo.designs, reg = reg)
summarizeExperiments(reg=reg)
submitJobs(ids = findNotDone()$job.id, reg=reg)















