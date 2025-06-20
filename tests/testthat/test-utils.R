# Add a few more tests to ones that are lacking
test_that("`util_replace_null` returns NA", {
  expect_equal(util_replace_null(NULL), NA)
  expect_equal(util_replace_null(NULL, val = "none"), "none")
  expect_equal(util_replace_null(1), 1)
})


test_that("`convert_df_to_list` is working", {
  starwars_list <- suppressWarnings(convert_df_to_list(dplyr::starwars))
  
  expect_type(starwars_list, "list")
  expect_type(starwars_list, "list")
})


test_that("`convert_list_to_df1` is working", {
  starwars_df <- suppressWarnings(convert_list_to_df1(as.list(dplyr::starwars)[2]))
  
  expect_equal(starwars_df |> nrow(), 87)
  expect_match(starwars_df$value |> class(), "integer")
  expect_match(starwars_df$key |> unique(), "height")
  expect_named(starwars_df, c("key", "value"))
})


test_that("`convert_list_to_df2` is working", {
  expect_equal(convert_list_to_df2(NULL) |> suppressWarnings(), NA)
  expect_equal(convert_list_to_df2(NA) |> suppressWarnings(), NA)

  my_list <-suppressWarnings(convert_df_to_list(dplyr::starwars))
  expect_match(class(convert_list_to_df2(my_list))[1] |> suppressWarnings(), "tbl_df")
  expect_equal(convert_list_to_df2(my_list, as_character = TRUE)[[1,2]] |> suppressWarnings(), "172")
  expect_equal(convert_list_to_df2(my_list, as_character = FALSE)[[1,2]] |> suppressWarnings(), 172)

  my_list <- list(NA)
  expect_equal(convert_list_to_df2(my_list) |> suppressWarnings(), NA)
})


test_that("`util_df_convert_character` is working", {
  expect_type(dplyr::starwars$height, "integer")
  expect_type(util_df_convert_character(dplyr::starwars)$height, "character")
})


test_that("`util_extract_list_element` is working", {
  test_list <- convert_df_to_list(dplyr::starwars) |> suppressWarnings()
  names(test_list) <- paste("row", seq_len(nrow(dplyr::starwars)))

  expect_type(util_extract_list_element(1, test_list, "height"), "integer")
  expect_equal(util_extract_list_element(1, test_list,  "height"), 172)
  expect_equal(util_extract_list_element("row 2", test_list, "skin_color"), "gold")
  expect_error(util_extract_list_element((length(test_list) + 1), test_list, "height"), "subscript out of bounds")
})


test_that("`util_append_to_list` is working", {
  my_list <- as.list(dplyr::starwars) 
  expect_equal(util_append_to_list(my_list, NULL), my_list)
  expect_length(util_append_to_list(my_list, NA), 15)
  expect_gt(length(util_append_to_list(my_list, NA)), length(my_list))
  expect_type(util_append_to_list(my_list[seq(1, length(my_list) - 1)], my_list[14]), "list")
  expect_length(util_append_to_list(my_list[seq(1, length(my_list) - 1)], my_list[14]), 14)
  expect_error(util_append_to_list(my_list), 'argument "to_append" is missing, with no default')
})


test_that("`util_separate_and_sort` returns alphabetically sorted characters", {
  expect_type(util_separate_and_sort("z y x"), "character")
  expect_match(util_separate_and_sort("z y x"), "x y z")
  expect_match(util_separate_and_sort("300 200 100 1 2 3"), "1 100 2 200 3 300")
})


test_that("testing env is working", {
  expect_true(is_testing_env())
})

