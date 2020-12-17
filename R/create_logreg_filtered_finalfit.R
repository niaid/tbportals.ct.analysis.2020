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
create_logreg_filtered_finalfit <- function(mlr3_pipeops, mlr3_tasks) {

  # totalcavernum and lungcavitysize are correlated so need to drop one or recode

  flt_ig <- mlr3_pipeops$flt_ig$clone()
  filt_df <- flt_ig$train(list(mlr3_tasks$classif))$output$data()

  full_df <- mlr3_tasks$classif$data()
  all_explanatory <- colnames(full_df %>% select(-event))
  explanatory_filtered <- colnames(filt_df)[!colnames(filt_df) %in% c("time", "event", "totalcavernum")]
  dependent <- "event"

  # Relevel factors for consistent output
  full_df %<>%
    mutate(
      event = relevel(event, ref = "cured"),
      affectpleura = relevel(affectpleura, ref = "No"),
      dissemination = relevel(dissemination, ref = "No"),
      lungcavitysize = relevel(lungcavitysize, ref = "No cavities"),
      totalcavernum = relevel(totalcavernum, ref = "No cavities"))

  res_logreg_filtered <- full_df %>%
    glm(formula = formula(ff_formula(dependent = dependent, explanatory = explanatory_filtered)), family = "binomial", data = .)

  # Explore log reg model -------

  multi <- full_df %>%
    finalfit.glm(dependent = dependent, explanatory = all_explanatory, explanatory_multi = explanatory_filtered,
                 family = "binomial", keep_fit_id = T) %>%
    as.data.frame()

  full_df %>%
    select(c(explanatory_filtered)) %>%
    missing_predictorMatrix(
    ) -> predm

  fits <- full_df %>%
    mutate_if(is.character, as.factor) %>%
    select(c(event, explanatory_filtered)) %>%
    mice(m = 5, predictorMatrix = predm, printFlag = FALSE) %>%
    with(glm(formula(ff_formula(dependent, explanatory_filtered)), family = "binomial"))

  mi <- full_df %>% summary_factorlist(dependent = dependent,
                                       explanatory = explanatory_filtered, fit_id = TRUE) %>%
    ff_merge(fits %>%
               pool() %>%
               fit2df(estimate_suffix = " (multiply imputed)", exp = TRUE, estimate_name = "OR")) %>%
    as.data.frame()

  finalfit_comb <- multi %>%
    left_join(mi %>%
                select(fit_id, `OR (multiply imputed)`)) %>%
    select(-c(fit_id, index)) %>%
    mutate(`OR (multiply imputed)` = replace_na(data = `OR (multiply imputed)`, replace = "-")) %>%
    kable %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F, font_size = 14, fixed_thead = T) %>%
    scroll_box(width = "1000px", height = "500px")

  # Return -----
  return(list("logreg_model" = res_logreg_filtered, "finalfit" = finalfit_comb))

}
