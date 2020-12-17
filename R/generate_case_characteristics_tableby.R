##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param initial_df
##' @param xl_files
##' @return
##' @author Gabriel Rosenfeld
##' @export
generate_case_characteristics_tableby <- function(initial_df, xl_files) {

  cohort_ids <- initial_df$condition_id
  cases <- xl_files$`TB Portals Patient Cases_20201021.csv` %>%
    filter(condition_id %in% cohort_ids) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(registration_date = as.factor(registration_date))

  tableby(formula = outcome ~ .,
          data = cases %>%
            select(c(outcome, registration_date, age_of_onset, gender, country, type_of_resistance, bmi))
  ) %>%
    summary %>%
    data.frame %>%
    rename_at(vars(1), ~ paste("Variable")) %>%
    mutate_at(vars(1), ~ gsub("&nbsp;", "", .))

}
