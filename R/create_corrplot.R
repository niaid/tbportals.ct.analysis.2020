##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param mlr3_tasks
##' @param mlr3_pipeops
##' @return
##' @author Gabriel Rosenfeld
##' @export
create_corrplot <- function(mlr3_tasks, mlr3_pipeops) {

  t_enc <- mlr3_pipeops$one_hot$clone()
  t_enc$train(list(mlr3_tasks$classif))

  tmp <- t_enc$predict(list(mlr3_tasks$classif))$output$data()
  tmp <- tmp %>%
    mutate(event = ifelse(event == "died", 1, 0)) %>%
    filter_all(all_vars(!is.na(.)))

  tmp_cor <- cor(tmp %>% select(-matches("pneumothorax")))
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  corrplot(tmp_cor, title = "Correlation",
           tl.cex = 1, mar = c(0, 0, 1, 0), method = "color", col = col(10), cl.cex = 1,
           diag = FALSE, type = "upper", order = "hclust", insig = "blank")

}
