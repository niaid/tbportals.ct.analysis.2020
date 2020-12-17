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
create_survival_bm <- function(mlr3_pipeops, mlr3_tasks) {

  flt_ig <- mlr3_pipeops$flt_ig$clone()
  samp_imp <- mlr3_pipeops$samp_imp$clone()
  encode <- mlr3_pipeops$encode$clone()
  rm_zv <- mlr3_pipeops$rm_zv$clone()
  ffact <- mlr3_pipeops$ffact$clone()
  cv5 <- mlr3_pipeops$cv5$clone()

  gr_surv <- map(mlr3_pipeops$po_lrns_surv,
                 .f = function(l)  flt_ig %>>% samp_imp %>>% encode %>>% rm_zv %>>% ffact %>>% l$clone())
  names(gr_surv) <- names(mlr3_pipeops$lrns_surv)

  set.seed(-10)
  cv5$instantiate(mlr3_tasks$surv_train)
  design <- benchmark_grid(
    tasks = mlr3_tasks$surv_train, learners = gr_surv,
    resamplings = cv5
  )

  set.seed(7)
  bm_surv <- benchmark(design)

  surv_m <- msrs(mlr_measures$keys()[grepl("surv", mlr_measures$keys())])
  names(surv_m) <- mlr_measures$keys()[grepl("surv", mlr_measures$keys())]

  .plot_function <- function(bm, m, title = t) {

    autoplot(bm, measure = m) +
      ggtitle(title) +
      ggthemes::theme_clean(base_size = 24) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

  }

  plot_bm_surv <- map(surv_m, .f = function(m) try(.plot_function(bm = bm_surv, m = m, title = "Benchmarking of survival models")))
  names(plot_bm_surv) <- map_chr(surv_m, .f = function(x) x$id)

  # Training summary statistics
  surv_m2 <- surv_m[grepl("cindex|harrell", names(surv_m))]
  train_ss <- bm_surv$score(surv_m2) %>%
    as.data.table() %>%
    group_by(learner_id) %>%
    summarise_at(vars(matches("surv")), ~glue::glue("{round(median(.), 2)} +/- {round(mad(.), 2)}"))

  i <- seq_len(length(gr_surv))

  set.seed(2)
  o <- map(i, .f = function(x) gr_surv[[x]]$train(mlr3_tasks$surv_train))
  preds <- map(i, .f = function(x) gr_surv[[x]]$predict(mlr3_tasks$surv_test))
  names(preds) <- names(gr_surv)[i]
  map_df(preds, .f = function(p) p[[1]]$score())

  # Validation set metrics
  valid_ss_metrics <-
    map_df(preds, .f = function(p) p[[1]]$score(surv_m2)) %>%
    mutate(class_balancing = "Class balancing") %>%
    mutate(base_learner = names(preds))

  ests <- gr_surv$surv.coxph$state$surv.coxph$model %>%
    yardstick::tidy() %>%
    mutate(HR = exp(estimate)) %>%
    mutate_if(is.numeric, ~round(., digits = 2))

  ests %>%
    kable %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                  full_width = F, font_size = 14, fixed_thead = T) %>%
    scroll_box(width = "750px", height = "500px")

  return(list("plot_bm_surv" = plot_bm_surv, "preds" = preds, "ests" = ests, "bm_surv" = bm_surv, "train_ss" = train_ss, "valid_ss_metrics" = valid_ss_metrics))

}
