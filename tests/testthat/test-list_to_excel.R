# Test for list_to_excel() from RosyUtils

# Note: list_to_excel() and read.xlsx() treat all variables as character.
# in list_to_excel(), we could set rio::import() col_types = NULL? I will
#   convert all df1 and df2 columns to character before comparing.

# Required packages
library(openxlsx) # For reading report
library(writexl) # For writing report

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



# next: Test for list with multiple lists

test_that("list_to_excel creates multiple Excel files", {

  # Create temporary directory and file names
  temp_dir <- tempdir()
  temp_files <- file.path(
    temp_dir,
    c("testReports_Sheet1.xlsx", "testReports_Sheet2.xlsx")
  )

  # Export as Excel files with separate = TRUE
  list_to_excel(
    test_list,
    dir = temp_dir,
    file_name = "testReports",
    separate = TRUE
  )

  # Check if Excel files exist
  expect_true(all(file.exists(temp_files)))

  # Verify that files can be read
  wb_read_1 <- read.xlsx(temp_files[1])
  wb_read_2 <- read.xlsx(temp_files[2])

  # Convert original list to character class only
  test_list_char <- lapply(test_list, function(df) {
    as.data.frame(lapply(df, as.character))
  })

  # Compare character versions
  expect_equal(wb_read_1, test_list_char[[1]])
  expect_equal(wb_read_2, test_list_char[[2]])

})



