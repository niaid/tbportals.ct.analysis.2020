##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param initial_df
##' @return
##' @author Gabriel Rosenfeld
##' @export
preprocess_initial_df <- function(initial_df) {

  initial_df %<>%
    group_by(patient_id, condition_id) %>%
    select(-c(started, period_end, period_end2, max_act_period_end, anomalymediastinumvesselsdevelop, shadowpattern,
              affectedsegments, thromboembolismpulmonaryartery, accumulationcontrast,
              anomalylungdevelop))

  # Recode variables
  initial_df %<>%
    ungroup() %>%
    rowwise() %>%
    mutate(bodysite_coding_cd = case_when(grepl(pattern = "Right|Left", bodysite_coding_cd) ~ "one_lung",
                                          TRUE ~ "both_lungs"),
           lungcavitysize = case_when(grepl(pattern = "10", lungcavitysize) ~ "LTE to 25mm",
                                      TRUE ~ lungcavitysize),
           affectlevel = case_when(grepl(pattern = "lower|medium", affectlevel, ignore.case = T) ~ "Lower or medium",
                                   TRUE ~ affectlevel),
           totalcavernum = case_when(grepl(pattern = "More than", totalcavernum) ~ totalcavernum,
                                     grepl(pattern = "1 cavity|2 cavities", totalcavernum) ~ "LTE to 2 cavities",
                                     TRUE ~ totalcavernum)) %>%
    group_by(patient_id, condition_id) %>%
    mutate_if(is.character, as.factor) %>%
    rename(event = outcome_cd,
           time = first_ct_to_event_time) %>%
    ungroup

  return(initial_df)
}
