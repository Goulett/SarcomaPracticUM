library(openxlsx) # For reading report
library(writexl) # For writing report
# Test for list_to_excel() from RosyUtils
# Note: list_to_excel() and read.xlsx() treat all variables as character.
# in list_to_excel(), we could set rio::import() col_types = NULL? I will
#   convert all df1 and df2 columns to character before comparing.

test_that("list_to_excel() creates Excel file", {

  # Create a temporary directory and file name
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "testReport.xlsx")

  # Create sample list of data frames
  df1 <- data.frame(A = 1:3, B = c("x", "y", "z"))
  df2 <- data.frame(X = c("a", "b", "c"), Y = 4:6)
  test_list <- list(Sheet1 = df1, Sheet2 = df2)

  # Export as Excel file
  list_to_excel(
    test_list,
    dir = temp_dir,
    file_name = "testReport"
    )

  # Verify report is created
  expect_true(file.exists(temp_file))

  # Check if the file contains the expected sheets
  output_sheets <- openxlsx::getSheetNames(temp_file)
  expect_equal(sort(output_sheets), sort(names(test_list)))

  # Check if the data in the file matches the input data
  df1_read <- openxlsx::read.xlsx(temp_file, sheet = "Sheet1")
  df2_read <- openxlsx::read.xlsx(temp_file, sheet = "Sheet2")

  # Convert df1 and df2 to character type because the excel columns are read
  #   as character type
  df1_char <- df1
  df1_char$A <- as.character(df1_char$A)

  df2_char <- df2
  df2_char$Y <- as.character(df2_char$Y)

  # Compare the read data to the versions with character vectors
  expect_equal(df1_read, df1_char)
  expect_equal(df2_read, df2_char)
  # vector class data are lost; all excel cols are imported as character vectors

  unlink(temp_file)
})
