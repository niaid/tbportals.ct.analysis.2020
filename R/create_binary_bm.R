##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param mlr3_pipeops
##' @param mlr3_tasks
##' @return
##' @author Gabriel Rosenfeld
##' @export
create_binary_bm <- function(mlr3_pipeops, mlr3_tasks) {

  flt_ig <- mlr3_pipeops$flt_ig$clone()
  samp_imp <- mlr3_pipeops$samp_imp$clone()
  encode <- mlr3_pipeops$encode$clone()
  rm_zv <- mlr3_pipeops$rm_zv$clone()
  ffact <- mlr3_pipeops$ffact$clone()
  cbal <- mlr3_pipeops$cbal$clone()
  smote <- mlr3_pipeops$smote$clone()
  col_apply <- mlr3_pipeops$col_apply$clone()

  two_filt <- map(mlr3_pipeops$po_lrns,
                  .f = function(l) flt_ig %>>% samp_imp %>>% encode %>>%
                    rm_zv %>>% ffact %>>% l$clone())

  two_filt_cbal <- map(mlr3_pipeops$po_lrns,
                       .f = function(l) flt_ig %>>% samp_imp %>>% cbal %>>%
                         encode %>>% rm_zv %>>% ffact %>>% l$clone())

  two_filt_smote <- map(mlr3_pipeops$po_lrns,
                       .f = function(l) flt_ig %>>% samp_imp %>>%
                         encode %>>% rm_zv %>>% smote %>>% col_apply %>>% ffact %>>% l$clone())

  two_filt$classif.ranger$param_set$values <- mlr3misc::insert_named(two_filt$classif.ranger$param_set$values,
                                                                     list(classif.ranger.importance = "impurity"))
  two_filt_cbal$classif.ranger$param_set$values <- mlr3misc::insert_named(two_filt_cbal$classif.ranger$param_set$values,
                                                                          list(classif.ranger.importance = "impurity"))
  two_filt_smote$classif.ranger$param_set$values <- mlr3misc::insert_named(two_filt_smote$classif.ranger$param_set$values,
                                                                          list(classif.ranger.importance = "impurity"))

  set.seed(10)
  mlr3_pipeops$cv5$instantiate(task = mlr3_tasks$classif_train)
  design_gr_filt <- benchmark_grid(
    tasks = mlr3_tasks$classif_train,
    learners = two_filt,
    resamplings = mlr3_pipeops$cv5
  )

  design_gr_filt_cbal <- benchmark_grid(
    tasks = mlr3_tasks$classif_train,
    learners = two_filt_cbal,
    resamplings = mlr3_pipeops$cv5
  )

  design_gr_filt_smote <- benchmark_grid(
    tasks = mlr3_tasks$classif_train,
    learners = two_filt_smote,
    resamplings = mlr3_pipeops$cv5
  )

  lgr::get_logger("mlr3")$set_threshold("warn")

  set.seed(2)
  bm_filt <- benchmark(design_gr_filt, store_models = TRUE)
  set.seed(2)
  bm_filt_cbal <- benchmark(design_gr_filt_cbal, store_models = TRUE)
  set.seed(2)
  bm_filt_smote <- benchmark(design_gr_filt_smote, store_models = TRUE)

  classif_m <- msrs(mlr_measures$keys()[grepl("classif", mlr_measures$keys())])
  names(classif_m) <- mlr_measures$keys()[grepl("classif", mlr_measures$keys())]
  classif_m <- classif_m[grepl("acc|ce|auc|bbrier|mcc|sensitivity|specificity", names(classif_m))]

  .plot_function <- function(bm, m, title = t) {

    autoplot(bm, measure = m) +
      ggtitle(title) +
      ggthemes::theme_clean(base_size = 24) +
      theme(axis.text.x = element_text(angle = 75, hjust = 1))

  }

  plot_bm_filt <- map(classif_m, .f = function(m) try(.plot_function(bm = bm_filt, m = m, title = "Benchmarking without class balancing")))
  names(plot_bm_filt) <- map_chr(classif_m, .f = function(x) x$id)

  plot_bm_filt_cbal <- map(classif_m, .f = function(m) try(.plot_function(bm = bm_filt_cbal, m = m, title = "Benchmarking with class balancing")))
  names(plot_bm_filt_cbal) <- map_chr(classif_m, .f = function(x) x$id)

  # Training summary statistics
  classif_m2 <- classif_m[grepl("acc|auc|bbrier|mcc|sensitivity|specificity", names(classif_m))]
  train_ss_noclassbal <- bm_filt$score(classif_m2) %>%
    as.data.table() %>%
    group_by(learner_id) %>%
    summarise_at(vars(matches("classif")), ~glue::glue("{round(median(.), 2)} +/- {round(mad(.), 2)}"))
  train_ss_classbal <- bm_filt_cbal$score(classif_m2) %>%
    as.data.table() %>%
    group_by(learner_id) %>%
    summarise_at(vars(matches("classif")), ~glue::glue("{round(median(.), 2)} +/- {round(mad(.), 2)}"))
  train_ss_smote <- bm_filt_smote$score(classif_m2) %>%
    as.data.table() %>%
    group_by(learner_id) %>%
    summarise_at(vars(matches("classif")), ~glue::glue("{round(median(.), 2)} +/- {round(mad(.), 2)}"))

  i <- seq_len(length(two_filt_cbal))

  set.seed(10)
  o <- map(i, .f = function(x) two_filt_cbal[[x]]$train(mlr3_tasks$classif_train))
  preds <- map(i, .f = function(x) two_filt_cbal[[x]]$predict(mlr3_tasks$classif_test))
  names(preds) <- names(two_filt_cbal)[i]

  set.seed(10)
  o <- map(i, .f = function(x) two_filt[[x]]$train(mlr3_tasks$classif_train))
  preds2 <- map(i, .f = function(x) two_filt[[x]]$predict(mlr3_tasks$classif_test))
  names(preds2) <- names(two_filt)[i]

  set.seed(10)
  o <- map(i, .f = function(x) two_filt_smote[[x]]$train(mlr3_tasks$classif_train))
  preds3 <- map(i, .f = function(x) two_filt_smote[[x]]$predict(mlr3_tasks$classif_test))
  names(preds3) <- names(two_filt_smote)[i]

  # Validation set metrics
  valid_ss_metrics <- bind_rows(
    map_df(preds2, .f = function(p) p[[1]]$score(classif_m2)) %>% mutate(class_balancing = "No class balancing"),
    map_df(preds, .f = function(p) p[[1]]$score(classif_m2)) %>% mutate(class_balancing = "Class balancing"),
    map_df(preds3, .f = function(p) p[[1]]$score(classif_m2)) %>% mutate(class_balancing = "SMOTE"),
  ) %>%
    mutate(base_learner = rep(names(preds), times = 3))

  ests <- two_filt_cbal$classif.log_reg$state$classif.log_reg$model %>%
    yardstick::tidy() %>%
    full_join(two_filt_cbal$classif.multinom$state$classif.multinom$model %>%
                coef() %>%
                yardstick::tidy() %>%
                rename_at(vars(2), ~ paste("multinom estimates")), by = c("term" = "names")) %>%
    mutate(OR_lr = exp(estimate),
           OR_mult = exp(`multinom estimates`))

  ests %<>%
    # full_join(glmnet_est) %>%
    mutate_if(is.numeric, ~round(., 2)) %>%
    kable() %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F, font_size = 14, fixed_thead = T) %>%
    scroll_box(width = "750px", height = "500px")

  return(list("bm_filt" = bm_filt, "bm_filt_cbal" = bm_filt_cbal, "bm_filt_smote" = bm_filt_smote,
              "plot_bm_filt" = plot_bm_filt, "plot_bm_filt_cbal" = plot_bm_filt_cbal,
              "train_ss_noclassbal" = train_ss_noclassbal, "train_ss_classbal" = train_ss_classbal, "train_ss_smote" = train_ss_smote,
              "two_filt_cbal_preds" = preds, "two_filt_preds" = preds2,
              "two_filt_smote_preds" = preds3, "valid_ss_mterics" = valid_ss_metrics, "ests" = ests))

}
