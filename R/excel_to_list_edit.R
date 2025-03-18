# testing parameters of excel_to_list

# Can we edit rio parameters so col_types = "NULL" and not "text"?
# Answer: No. Class is lost regardless.

# function with edit:
excel_to_list2 <- function (path)
{
  sheets <- readxl::excel_sheets(path)
  clean_sheets <- clean_env_names(sheets)
  out <- list()
  for (i in seq_along(sheets)) {
    out[[i]] <- rio::import(path, col_types = "NULL", sheet = i)
  }
  names(out) <- clean_sheets
  return(out)
}



# Test

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

df1_read <- openxlsx::read.xlsx(temp_file, sheet = "Sheet1")
df2_read <- openxlsx::read.xlsx(temp_file, sheet = "Sheet2")

expect_equal(df1_read, df1)
expect_equal(df2_read, df2)
# Fails; df1_read and df2_read became character class. Excel columns are
#   in general format.
