predict_h2o <- function(model, newdata) {
  results_tbl <- h2o.predict(model, newdata = as.h2o(newdata)) %>% as_tibble()
  results_tbl %>% pull(Yes)
}