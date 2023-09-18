create_data_dictionary <- function(data, form = "wide"){

  # Adopted from https://github.com/tidyverse/haven/issues/540


  # fetch variable class ----
  var_class <- purrr::map(data, class) %>%
    purrr::map(., 1) %>%
    tibble::enframe(name = "variable_name", value = "variable_class") %>%
    tibble::rowid_to_column(var = "variable_id") %>%
    tidyr::unnest(variable_class)

  # number of factors ----
  num_factor <- sum(var_class[["variable_class"]] == "factor")

  # number of haven labelled ----
  num_haven  <- sum(var_class[["variable_class"]] == "haven_labelled")


  if(num_haven == 0) {
    code_values_labelled <- NULL
    code_labels_labelled <- NULL
  } else {
    # fetch variable labels ----
    var_labels <- data %>%
      sjlabelled:::get_label() %>%
      tibble::enframe(name = "variable_name", value = "variable_label") %>%
      tibble::rowid_to_column(var = "variable_id")

    # fetch value codes from haven labelled ----
    code_values_labelled <- data %>%
      dplyr::select_if(haven::is.labelled) %>%
      purrr::map(.f = sjlabelled::get_values) %>%
      purrr::map(as.character) %>%
      list_to_df(name1 = "variable_name", name2 = "value_code") %>%
      dplyr::group_by(variable_name) %>%
      dplyr::mutate(value_id = dplyr::row_number()) %>%
      dplyr::ungroup()

    # fetch value labels from haven labelled ----
    code_labels_labelled <- data %>%
      dplyr::select_if(haven::is.labelled) %>%
      purrr::map(.f = sjlabelled::get_labels) %>%
      list_to_df(name1 = "variable_name", name2 = "value_label")  %>%
      dplyr::group_by(variable_name) %>%
      dplyr::mutate(value_id = dplyr::row_number()) %>%
      dplyr::ungroup()
  }

  if(num_factor == 0) {
    code_factor <- NULL
  } else {
    # fetch value levels from factor ----
    code_factor <- data %>%
      dplyr::select_if(is.factor) %>%
      purrr::map(.f = levels) %>%
      list_to_df(name1 = "variable_name", name2 = "value_label") %>%
      dplyr::group_by(variable_name) %>%
      dplyr::mutate(
        value_id = dplyr::row_number(),
        value_code = as.character(value_id)
      ) %>%
      dplyr::ungroup()
  }

  # stack labels and values from haven labelled and factors ----
  if(num_haven == 0 & num_factor == 0){
    code_labels <- NULL
    code_values <- NULL
  } else if (num_haven > 0 & num_factor == 0) {
    code_labels <- code_labels_labelled
    code_values <- code_values_labelled
  } else if (num_haven == 0 & num_factor > 0) {
    code_labels <- code_factor %>% dplyr::select(variable_name, value_label, value_id)
    code_values <- code_factor %>% dplyr::select(variable_name, value_code, value_id)
  } else if (num_haven > 0 & num_factor > 0) {
    code_labels <- dplyr::bind_rows(code_labels_labelled, code_factor %>% dplyr::select(variable_name, value_label, value_id))
    code_values <- dplyr::bind_rows(code_values_labelled, code_factor %>% dplyr::select(variable_name, value_code, value_id))
  }

  # if either haven labelled or factors are present, assemble dictionary with value and
  # label look ups ----
  if (num_haven > 0 | num_factor > 0) {
    # assemble into long data frame with 1 row per code ----
    variable_view_long <- var_labels %>%
      dplyr::left_join(var_class, by = c("variable_name", "variable_id")) %>%
      dplyr::left_join(code_values, by = c("variable_name")) %>%
      dplyr::left_join(code_labels, by = c("variable_name", "value_id")) %>%
      dplyr::mutate(
        value_string = case_when(
          is.na(value_id) ~ NA_character_,
          variable_class == "factor" ~ as.character(glue::glue('{value_label}')),
          variable_class == "haven_labelled" ~ as.character(glue::glue('{value_code} = {value_label}'))
        )
      ) %>%
      dplyr::select(variable_id, variable_name, variable_label, variable_class, value_id, value_code, value_label, value_string)

    # create look up of values for wide data frame ----
    value_lookup <- variable_view_long %>%
      tidyr::drop_na(value_id) %>%
      dplyr::select(variable_name, value_string) %>%
      split(., f = .$variable_name) %>%
      purrr::map(., 2) %>%
      list_to_lookup(name1 = "variable_name", name2 = "value_codes")

    # assemble into wide data frame with 1 row per variable ----
    variable_view_wide <- variable_view_long %>%
      dplyr::select(variable_id, variable_name, variable_label, variable_class) %>%
      dplyr::distinct() %>%
      dplyr::left_join(value_lookup, by = "variable_name")
  }

  # if neither haven labelled nor factors are present, assemble dictionary with just class
  if (num_haven == 0 & num_factor == 0) {
    return_view <- var_class
  } else {
    if (form == "long") return_view <- variable_view_long
    if (form == "wide") return_view <- variable_view_wide
  }

  return_view = return_view %>% dplyr::mutate(haven_labelled = variable_class=="haven_labelled")
  for(j in 1:nrow(return_view)){
    return_view$variable_class[j] = data %>% purrr::pluck(return_view$variable_name[j]) %>% zap_attributes() %>% class()

  }
  return(return_view)

}
