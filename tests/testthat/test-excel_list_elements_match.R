# Test for excel_to_list() - WIP
# Test if excel_to_list imports data correctly. Currently, this function will
#   treat all columns as 'text' and import them as 'character' class, so we will
#   convert the object columns to character class for now.
# Test fails because names are not present in mtcars_list_df_char
test_that("Excel list elements match object's", {

  # Export mtcars df
  mtcars_list_df <- list(mtcars)
  list_to_excel(
    mtcars_list_df,
    dir = "./output",
    file_name = "mtcarsReport"
  )

  # Import mtcars df
  mtcars_import <- excel_to_list("C:/Users/natal/OneDrive - Florida International University (1)/MPH/UM Project/SarcomaPracticUM/output/mtcarsReport.xlsx")

  # Convert numeric columns from mtcars to character class
  mtcars_list_df_char <- list(lapply(mtcars_list_df[[1]], as.character))
  mtcars_list_df_char[[1]] <- as.data.frame(mtcars_list_df_char[[1]])

  expect_equal(mtcars_import, mtcars_list_df_char)
})
