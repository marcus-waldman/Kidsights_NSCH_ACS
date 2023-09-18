list_to_lookup <- function(var_list, name1 = "section", name2 = "variable"){

  # Taken from https://github.com/tidyverse/haven/issues/540


  purrr::map2_df(
    names(var_list),
    var_list,
    ~ tibble::tibble(!!name1 := .x, !!name2 := paste(.y, collapse = ", "))
  )
}
