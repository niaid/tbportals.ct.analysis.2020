##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param initial_df_preprocess
##' @return
##' @author Gabriel Rosenfeld
##' @export
generate_initial_df_preprocess_tableby <- function(initial_df_preprocess) {

  tableby(formula = event ~ .,
          data = initial_df_preprocess %>%
            select(c(bodysite_coding_cd:totalcavernum, event))
  ) %>%
    summary %>%
    data.frame %>%
    rename_at(vars(1), ~ paste("Variable")) %>%
    mutate_at(vars(1), ~ gsub("&nbsp;", "", .))
}
