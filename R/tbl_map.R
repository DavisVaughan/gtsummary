
tbl_map <- function(.x, .f, ...,
                    .combine_with = c("tbl_stack", "tbl_merge"), .combine_args = NULL) {
  .combine_fun <- match.arg(.combine_with) %>%
    purrr::when(
      identical(., "tbl_stack") ~ tbl_stack,
      identical(., "tbl_merge") ~ tbl_merge
    )

  .combine_args <-
    # default arguments
    switch(
      .combine_with,
      "tbl_merge" = list(tab_spanner = df_tbls$header),
      "tbl_stack" = list(group_header = df_tbls$header, quiet = .quiet)
    ) %>%
    # update with user-passed arguments
    purrr::list_modify(!!!.combine_args)

  purrr::map(.x, .f, ...) %>%
    .combine_fun()
}

tbl_map2 <- function(.x, .y, .f, ...,
                     .combine_with = c("tbl_stack", "tbl_merge"), .combine_args = NULL) {
  .combine_fun <- match.arg(.combine_with)

  purrr::map2(.x, .y, .f, ...) %>%
    .combine_fun()
}

tbl_pmap <- function(.l, .f, ...,
                     .combine_with = c("tbl_stack", "tbl_merge"), .combine_args = NULL) {
  .combine_fun <- match.arg(.combine_with)

  purrr::pmap(.l, .f, ...) %>%
    .combine_fun()
}


