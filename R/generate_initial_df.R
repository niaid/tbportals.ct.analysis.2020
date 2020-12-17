##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param xl_files
##' @return
##' @author Gabriel Rosenfeld
##' @export
generate_initial_df <- function(xl_files) {

  all_ct <- xl_files$`TB Portals CTs_20201021.csv` %>%
    left_join(xl_files$`TB Portals CT_Annotations_20201021.csv`)

  ct <- all_ct %>%
    filter(!is.na(issued))

  new <- xl_files$`TB Portals Patient Cases_20201021.csv` %>%
    filter(condition_id %in% all_ct$condition_id & case_definition == "New") %>%
    select(c(patient_id, condition_id, age_of_onset, gender)) %>%
    distinct()

  treat_out <- xl_files$`TB Portals Regimens_20201021.csv` %>%
    filter(condition_id %in% all_ct$condition_id & outcome_cd %in% c("cured", "died"))

  n_distinct(intersect(new$condition_id, treat_out$condition_id)) # New cases with CT and outcome of cured or died

  miss_ct <- all_ct %>% filter(condition_id %in% intersect(new$condition_id, treat_out$condition_id[treat_out$outcome_cd == "died"]))

  miss_ct <- miss_ct %>%
    group_by(patient_id, condition_id) %>%
    filter(all(is.na(issued))) %>%
    ungroup %>%
    select(condition_id) %>%
    unique() %>%
    unlist() %>%
    unname()

  new %<>% filter(condition_id %in% ct$condition_id)

  treat_out %<>% filter(condition_id %in% ct$condition_id)

  n_distinct(intersect(new$condition_id, treat_out$condition_id)) # New cases with CT and outcome of cured or died and available annotation

  all <- new %>%
    left_join(ct) %>%
    left_join(treat_out) %>%
    filter(!is.na(period_end)) %>%
    group_by(patient_id, condition_id, started, period_end) %>%
    mutate(max_act_period_end = max(activities_period_end)) %>%
    summarise_at(vars(c(max_act_period_end, bodysite_coding_cd:totalcavernum, outcome_cd)), ~ unique(.)) %>%
    mutate(period_end2 = ifelse(period_end < max_act_period_end, max_act_period_end, period_end))

  all_initial <- all %>%
    ungroup() %>%
    group_by(patient_id, condition_id) %>%
    filter(started == min(started) & between(started, -60, 60)) %>% #between(started, x, y) to set filter for timerange of initial CT
    mutate(first_ct_to_event_time = (period_end2 - started) / 7) %>% # Convert time from days to weeks
    filter(first_ct_to_event_time >= 0) # Follow up time greater than or equal to 0 weeks

  n_distinct(all_initial$patient_id) # Final cohort numbers

  return(all_initial)

}
