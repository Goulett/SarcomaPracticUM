# Test for list_to_excel() - WIP

test_that("Excel list elements match object's", {
  mtcars_list_df <- list(mtcars)
  list_to_excel(
    mtcars_list_df,
    dir = "./output",
    file_name = "mtcarsReport"
  )

  expect_equal(mtcars_import, mtcars_list_df)
})
