# Test for excel_to_list() - WIP

# Test if excel_to_list imports data correctly. This function will export all
# columns as 'text' class and we'll import them as 'character' class, so we will
#   convert the test object columns to character class for now.
# Test fails because names are not present in mtcars_list_df_char, but are in
#   the .xlsx file. The list_to_excel() function gives the sheet the name "1".

test_that("Excel list elements match object's", {

  # Export mtcars df
  # Q: should tests use tempdirs only?
  mtcars_list_df <- list(mtcars)
  list_to_excel(
    mtcars_list_df,
    dir = "./output",
    file_name = "mtcarsReport"
  )

  # Import mtcars df
  mtcars_import <- excel_to_list("output/mtcarsReport.xlsx")
  # Note: returns message "Invalid environment name: '1"

  # Convert numeric columns from mtcars to character class
  mtcars_list_df_char <- list(lapply(mtcars_list_df[[1]], as.character))
  mtcars_list_df_char[[1]] <- as.data.frame(mtcars_list_df_char[[1]])

  expect_equal(mtcars_import, mtcars_list_df_char)
  # names are absent because list_to_excel creates list with 1 item named '1'
})


# next: Test for Excel file with multiple sheets
