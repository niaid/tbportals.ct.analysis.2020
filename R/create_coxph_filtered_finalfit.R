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
create_coxph_filtered_finalfit <- function(mlr3_pipeops, mlr3_tasks) {

  flt_ig <- mlr3_pipeops$flt_ig$clone()
  filt_df <- flt_ig$train(list(mlr3_tasks$surv))$output$data() %>%
    as.data.frame()

  full_df <- mlr3_tasks$surv$data()

  # lungcavitysize and totacavernum are colinear therefore dropping totalcavernum from final model for more accurate estimates
  all_explanatory <- colnames(full_df %>% select(-c(event, time)))
  explanatory_filtered <- colnames(filt_df)[!colnames(filt_df) %in% c("time", "event", "totalcavernum")]
  dependent_os <- "Surv(time, event)"

  full_df %<>%
    as.data.frame()

  # Relevel factors for consistent output
  full_df %<>%
    mutate(affectpleura = relevel(affectpleura, ref = "No"),
           dissemination = relevel(dissemination, ref = "No"),
           lungcavitysize = relevel(lungcavitysize, ref = "No cavities"),
           totalcavernum = relevel(totalcavernum, ref = "No cavities"))
  filt_df %<>%
    mutate(totalcavernum = relevel(totalcavernum, ref = "No cavities"))

  res_cox_filtered <- full_df %>%
    coxphmulti(dependent_os, explanatory_filtered)

  test_ph_filtered <- res_cox_filtered %>%
    cox.zph() # Test PH assumption of the multivariate coxph model

  test_ph_filtered <- test_ph_filtered$table %>%
    kable %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                  full_width = F, font_size = 14, fixed_thead = T) %>%
    scroll_box(width = "750px", height = "500px")

  # Explore coxph model -------
  multi <- full_df %>%
    finalfit.coxph(dependent = dependent_os, explanatory = all_explanatory,
                   explanatory_multi = explanatory_filtered, .data = ., keep_fit_id = T)

  full_df %>%
    select(c("time", explanatory_filtered)) %>%
    missing_predictorMatrix(
    ) -> predm

  fits <- full_df %>%
    mutate_if(is.character, as.factor) %>%
    select(c(time, event, explanatory_filtered)) %>%
    mice(m = 5, predictorMatrix = predm, printFlag = FALSE) %>%
    with(coxph(formula(ff_formula(dependent_os, explanatory_filtered))))

  mi <- full_df %>%
    summary_factorlist(dependent = dependent_os,
                       explanatory = explanatory_filtered, fit_id = TRUE) %>%
    ff_merge(fits %>%
               pool() %>%
               fit2df(estimate_suffix = " (multiply imputed)", exp = TRUE, estimate_name = "HR"))

  finalfit_comb <- multi %>%
    left_join(mi %>%
                select(fit_id, `HR (multiply imputed)`)) %>%
    mutate(`HR (multiply imputed)` = replace_na(`HR (multiply imputed)`, "-")) %>%
    select(-c(fit_id, index, all)) %>%
    kable %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F, font_size = 14, fixed_thead = T) %>%
    scroll_box(width = "1000px", height = "500px")

  # Return -----
  return(list("coxph_model" = res_cox_filtered, "ph.test" = test_ph_filtered,
              "finalfit" = finalfit_comb))

}
