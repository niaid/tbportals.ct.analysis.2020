##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

##' @return
##' @author Gabriel Rosenfeld
##' @export
setup_mlr3_pipeops <- function() {

  # Resamplings
  cv5 <- rsmp("cv", folds = 5)
  hout <- rsmp("holdout")
  cv3 <- rsmp("cv", folds = 3)

  # Measures
  fbeta <- msr("classif.fbeta")
  ce <- msr("classif.ce")

  #Hyperband tuner
  rand_search <- tnr("random_search")

  # Terminator
  hyper_term <- trm("evals", n_evals = 100000L) # high value to let hyperband finish
  term200 <- trm("evals", n_evals = 200L)
  term20 <- trm("evals", n_evals = 20L)

  # Graph processing steps comparing class balancing

  #Imputation
  unk_imp <- PipeOpImputeOOR$new(id = "imp_unk")
  samp_imp <- PipeOpImputeSample$new(id = "imp")
  smote <- PipeOpSmote$new(id = "smote", param_vals = list(dup_size = 8))
  rounded_integer <- function(x) {
    as.integer(round(x))
    }
  col_apply <- po("colapply", applicator = rounded_integer, param_vals = list(affect_columns = selector_grep(pattern = "affectlevel|affectpleura|bodysite_coding_cd|bronchialobstruction|dissemination|limfoadenopatia|lungcapacitydecrease|lungcavitysize|nodicalcinatum|plevritis|pneumothorax|posttbresiduals|processprevalence|totalcavernum", ignore.case = T)))

  # Filters
  flt_anova <- po(id = "flt_anova", "filter", filter = mlr3filters::flt(.key = "anova"), param_vals = list(filter.nfeat = 5))
  flt_auc <- po(id = "flt_auc", "filter", filter = mlr3filters::flt(.key = "auc"), param_vals = list(filter.nfeat = 5))
  flt_cmim <- po(id = "flt_cmim", "filter", filter = mlr3filters::flt(.key = "cmim"), param_vals = list(filter.nfeat = 5))
  flt_disr <- po(id = "flt_disr", "filter", filter = mlr3filters::flt(.key = "disr"), param_vals = list(filter.nfeat = 5))
  flt_corr <- po(id = "flt_corr", "filter", filter = mlr3filters::flt(.key = "find_correlation"), param_vals = list(filter.nfeat = 5))
  flt_imp <- po(id = "flt_imp", "filter", filter = mlr3filters::flt(.key = "importance"), param_vals = list(filter.nfeat = 5))
  flt_ig <- po(id = "flt_ig", "filter", filter = mlr3filters::flt(.key = "information_gain"), param_vals = list(filter.nfeat = 5))
  flt_jmi <- po(id = "flt_jmi", "filter", filter = mlr3filters::flt(.key = "jmi"), param_vals = list(filter.nfeat = 5))
  flt_jmim <- po(id = "flt_jmim", "filter", filter = mlr3filters::flt(.key = "jmim"), param_vals = list(filter.nfeat = 5))
  flt_kruskal <- po(id = "flt_krus", "filter", filter = mlr3filters::flt(.key = "kruskal_test"), param_vals = list(filter.nfeat = 5))
  flt_mim <- po(id = "flt_mim", "filter", filter = mlr3filters::flt(.key = "mim"), param_vals = list(filter.nfeat = 5))
  flt_mrmr <- po(id = "flt_mrmr", "filter", filter = mlr3filters::flt(.key = "mrmr"), param_vals = list(filter.nfeat = 5))
  flt_njmim <- po(id = "flt_njmim", "filter", filter = mlr3filters::flt(.key = "njmim"), param_vals = list(filter.nfeat = 5))
  flt_performance <- po(id = "flt_perf", "filter", filter = mlr3filters::flt(.key = "performance"), param_vals = list(filter.nfeat = 5))
  flt_var <- po(id = "flt_var", "filter", filter = mlr3filters::flt(.key = "variance"), param_vals = list(filter.nfeat = 5))

  # Branch organization
  cpy15 <- PipeOpCopy$new(id = "cpy", outnum = 15)
  cpy2 <- PipeOpCopy$new(id = "cpy", outnum = 2)
  funion2 <- PipeOpFeatureUnion$new(innum = 2, id = "funion")
  flt_branches <- po(id = "flt_branch", "branch", paste("flt_", c("anova", "auc", "cmim", "disr",
                                                                  "corr", "imp", "ig", "jmi",
                                                                  "jmim", "kruskal", "mim", "mrmr",
                                                                  "njmim", "performance", "var"), sep = ""))
  flt_unbranch <- po(id = "flt_unbranch", "unbranch")
  ss <- po("subsample")

  lrn_branches  <- po(id = "lrn_branch", "branch", c("classif.log_reg",
                                                     "classif.cv_glmnet", "classif.multinom",  "classif.kknn", "classif.ranger"))
  lrn_unbranch <- po(id = "lrn_unbranch", "unbranch")

  #Preprocessing
  encode <- PipeOpEncode$new(id = "enc", param_vals = list(method = "treatment"))
  encode_lmer <- PipeOpEncodeLmer$new(id = "enc_lmer", param_vals = list(affect_columns = selector_type(c("factor", "logical",
                                                                                                         "integer", "character", "ordered"))))
  one_hot <- PipeOpEncode$new(id = "onehot", param_vals = list(method = "one-hot"))
  cbal <- po(id = "cb", "classbalancing")
  filter <- po(id = "flt", "filter", filter = mlr3filters::flt(.key = "information_gain"), param_vals = list(filter.nfeat = 5))
  rm_zv <- PipeOpRemoveConstants$new(id = "zv")
  ffact <- PipeOpFixFactors$new(id = "ffact")
  missind <- PipeOpMissInd$new(id = "mi")

  # Benchmarking various binary classifer algorithms using top 10 features selected by random forest
  lrns <- as.data.table(mlr_learners)
  lrns %<>% as.data.frame() %>% filter(key %in% c("classif.featureless", "classif.log_reg", "classif.multinom",  "classif.kknn", "classif.ranger"))
  lrns <- lapply(lrns$key, FUN = function(l) lrn(l, predict_type = "prob"))
  names(lrns) <- map_chr(lrns, .f = function(l) l$id)

  # Pipe learners
  po_lrns <- map(names(lrns), .f = function(n) po("learner", lrns[[n]]))
  names(po_lrns) <- names(lrns)

  # Benchmarking various binary classifer algorithms using top 10 features selected by random forest
  lrns_surv <- as.data.table(mlr_learners)
  lrns_surv %<>% as.data.frame() %>% filter(key %in% c("surv.kaplan", "surv.coxph", "surv.rpart"))
  lrns_surv <- lapply(lrns_surv$key, FUN = function(l) lrn(l))
  names(lrns_surv) <- map_chr(lrns_surv, .f = function(l) l$id)

  po_lrns_surv <- map(names(lrns_surv), .f = function(n) po("learner", lrns_surv[[n]]))
  names(po_lrns_surv) <- names(lrns_surv)

  n <- ls()

  out <- lapply(n, FUN = function(x) get(x = x))
  names(out) <- n

  return(out)

}
