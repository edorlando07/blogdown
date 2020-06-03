data_prep_predictions <- function(data) {
  
  data <- data %>%
    filter(Attrition == "No",
           predict   == "Yes") %>%
    arrange(-Yes) %>%
    mutate(index = row_number()) %>%
    filter(index <= 15) %>%
    select(EmployeeNumber, Yes) %>%
    mutate(Yes = round(Yes, 4)) %>%
    mutate(Probability_Label = percent(Yes)) %>%
    mutate(Employee = as.character(EmployeeNumber)) %>%
    mutate(Probability = Yes) %>%
    mutate(Employee = fct_reorder(Employee, Probability))
  
}