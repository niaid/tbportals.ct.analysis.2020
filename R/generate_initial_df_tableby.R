##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param initial_df
##' @return
##' @author Gabriel Rosenfeld
##' @export
generate_initial_df_tableby <- function(initial_df) {

  initial_df %<>% ungroup()

  tableby(formula = outcome_cd ~ .,
          data = initial_df %>% select(outcome_cd, bodysite_coding_cd:totalcavernum)
  ) %>%
    summary %>%
    data.frame %>%
    rename_at(vars(1), ~ paste("Variable")) %>%
    mutate_at(vars(1), ~ gsub("&nbsp;", "", .))

}
