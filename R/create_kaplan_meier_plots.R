##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param mlr3_tasks
##' @return
##' @author Gabriel Rosenfeld
##' @export
create_kaplan_meier_plots <- function(mlr3_tasks) {

  d <- mlr3_tasks$surv$data()

  covariates <- colnames(d)[!colnames(d) %in% c("time", "event")]
  f <- map(covariates, .f = function(c) as.formula(glue("Surv(time, event) ~ {c}")))
  names(f) <- covariates

  fits <- map(f, .f = function(x) surv_fit(formula = x, data = d))
  names(fits) <- covariates

  km_plots <- map(names(fits), .f = function(x)
    ggsurvplot(
      fit = fits[[x]],
      data = d,
      risk.table = TRUE,
      pval = TRUE,
      conf.int = TRUE,
      xlim = c(0, 150), break.time.by = 50,
      ggtheme = theme_survminer(),
      risk.table.y.text.col = T,
      risk.table.y.text = FALSE
    ) + ggtitle(glue("Kaplan-Meier plot stratified by {x} covariate")) + xlab("Time (weeks) to died")
  )
  names(km_plots) <- covariates

  pairwise_diffs <- map(f, .f = function(x) pairwise_survdiff(formula = x, data = d))
  names(pairwise_diffs) <- covariates

  return(list("km_plots" = km_plots, "pairwise_diffs" = pairwise_diffs))

}
