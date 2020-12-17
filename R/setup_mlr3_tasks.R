##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param initial_df_preprocess
##' @return
##' @author Gabriel Rosenfeld
##' @export
setup_mlr3_tasks <- function(initial_df_preprocess) {

  set.seed(1)
  train_idx <- sample(seq_len(nrow(initial_df_preprocess)), 0.75 * nrow(initial_df_preprocess))
  test_idx <- setdiff(seq_len(nrow(initial_df_preprocess)), train_idx)

  classif_df <- initial_df_preprocess %>%
    select(-c(time, patient_id, condition_id)) %>%
    mutate(event = relevel(event, ref = "cured"))
  surv_df <- initial_df_preprocess %>%
    select(-c(patient_id, condition_id)) %>%
    mutate(event = ifelse(event == "cured", 0, 1))

  # positive class is actually reference and other will be the one used for predicting outcome (e.g. estimates will be for predicting other class)
  # Set up parameters for hyperparameter search for each ML algorithm of interest
  task_tb_classif_train <- TaskClassif$new(id = "tb", backend = classif_df[train_idx, ], target = "event", positive = "cured")
  task_tb_classif_train$set_col_role(cols = "event", new_roles = c("target", "stratum"))
  task_tb_classif_test <- TaskClassif$new(id = "tb", backend = classif_df[test_idx, ], target = "event", positive = "cured")
  task_tb_classif_test$set_col_role(cols = "event", new_roles = c("target", "stratum"))
  task_tb_classif <- TaskClassif$new(id = "tb", backend = classif_df, target = "event", positive = "cured")
  task_tb_classif$set_col_role(cols = "event", new_roles = c("target", "stratum"))

  # Set up parameters for hyperparameter search for each ML algorithm of interest
  task_tb_train <- TaskSurv$new(id = "tb", backend = surv_df[train_idx, ], time = "time", event = "event", type = "right")
  task_tb_test <- TaskSurv$new(id = "tb", backend = surv_df[test_idx, ], time = "time", event = "event", type = "right")
  task_tb <- TaskSurv$new(id = "tb", backend = surv_df, time = "time", event = "event", type = "right")

  return(list("classif" = task_tb_classif, "classif_test" = task_tb_classif_test, "classif_train" = task_tb_classif_train,
              "surv" = task_tb, "surv_test" = task_tb_test, "surv_train" = task_tb_train))

}
