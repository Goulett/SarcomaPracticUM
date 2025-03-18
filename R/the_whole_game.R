# clear --------
#.rs.restartR()
#RosyUtils::clear_env()

# functions ------------
library("MASS")
# library("RosyREDCap")
library("RosyUtils")
library("RosyDB")
library("plotly")
library("survival")
library("survminer")
library("lubridate")
library("tidyverse")
library("REDCapSync")
library("testthat")

plotly_hbar <- function(DF, x_col, y_col, name) {
  # Check if x_col and y_col are in DF
  if (!(x_col %in% colnames(DF)) || !(y_col %in% colnames(DF))) {
    stop("Invalid variable names.")
  }

  # Define colors for each level of y_col
  colors <- RColorBrewer::brewer.pal(n = length(levels(DF[[y_col]])), "Accent")

  # Initialize plot
  fig <- plotly::plot_ly()

  # Create traces (bar segments) for every level of y_col
  for (i in seq_along(levels(DF[[y_col]]))) {
    level <- levels(DF[[y_col]])[i]
    subset_data <- DF[DF[[y_col]] == level, ]
    fig <- fig %>% plotly::add_trace(
      x = table(subset_data[[x_col]]),  # Counts observations in each x_col level
      y = names(table(subset_data[[x_col]])),  # Gets x_col level names
      type = 'bar',
      orientation = 'h',
      name = level,
      marker = list(
        color = colors[i],
        line = list(color = 'rgba(0, 0, 0, 1.0)', width = 1)
      )
    )
  }
  # Set layout options for stacked horizontal bar chart
  fig <- fig %>% plotly::layout(
    barmode = 'stack',
    xaxis = list(
      title = "Number of Patients",
      font = list(
        size = 12,
        color = "black"
      )
    ),
    yaxis = list(
      title = ''
    )
  )

  return(fig)
}



# import -----------

projects <- REDCapSync::get_projects()  # load projects
# old package:
# DB <- load_RosyREDCap("PRAC")       # load REDCap data using "PRAC" token
# # Run to update REDCap data
# DB <- update_RosyREDCap(DB)         # update with latest REDCap data
# New package:

DB <-setup_project(
  short_name = "PRAC",
  dir_path = getwd() %>% normalizePath(),
  # for security, the API token is defined in the .Renviron file
  token_name = "PRAC_token",
  redcap_base = "https://redcap.fiu.edu/",
  merge_form_name = "patient"

)

# Sync project
get_projects()
DB_Sync <- sync_project(DB)



#explore -------
# table the number of each field type in DB
DB$metadata$fields$field_type_R %>% table()
# returns column names
DB$metadata$fields$field_name %>% RosyUtils::vec_to_cvec()

#tidy ----------
# add DB metadata to the global environment
DB$metadata %>% RosyUtils::add_list_to_global()
# optional transformation for REDCap in default format
#DB <- add_forms_transformation(DB,forms_transformation =  default_forms_transformation(DB))
# turns factors into R factors
DB <- clean_DB(DB)
DB$data %>% RosyUtils::add_list_to_global()

# create wide data.frame of patient and sarcoma instrument data
DF <- patient %>% merge(sarcoma, by = "record_id")
# We clean this new DF even after cleaning DB because every merge drops some attributes
DF <- clean_DF(DF, DB$metadata$fields)

# communicate ------
#DF <- RosyUtils::clean_df_blanks(DF)

# Create vector of factors from DB
cats <- DB$metadata$fields$field_name[which(DB$metadata$fields$field_type_R=="factor")]

# Randomly sample two factors without replacement
our_sample <- cats %>% sample(2)

# Label samples using metadata
our_sample_labels <- sapply(our_sample, function(field_name){field_name %>% field_names_to_field_labels(DB)})

# Test function using samples. Rinse and repeat
plotly_hbar(
  DF = DF,
  x_col = our_sample[1],
  y_col = our_sample[2],
  name = paste0(our_sample_labels[1], " by ",our_sample_labels[2])
)

# Test on DF
plotly_hbar(
  DF = DF,
  x_col = DF$sex_at_birth,
  y_col = DF$is_hispanic
)



# create table of DF
DT::datatable(DF)
DF %>%
  # Select all rows in vector 1 that are in vector 2
  dplyr::select(all_of(RosyUtils::vec1_in_vec2(colnames(DF),cats))) %>%
  # if remove_missing = T, it will plot nothing (current code) (bc some columns empty)
  RosyApp::plotly_parcats(remove_missing = F)

# compare to new make DT table
DF %>% RosyApp::make_DT_table()
# publication table 1:
DF %>% dplyr::select(is_hispanic, sex_at_birth) %>% RosyApp::make_table1()


#save ----------

# Create .xlsx of DB including with summary statistics
DB <- drop_redcap_dir(DB)
DB <- summarize_RosyREDCap(DB)



#practice survival function -------------

# starter function suggestion given by Dr. Rose:
make_survival_curve <- function(DF, start_date_col, end_date_col, status_col, strat_col){
  if(!is.data.frame(DF))stop("DF has to be a data.frame")
  fit <- survfit(Surv(end_date_col - start_date_col, status) ~ sex, data = DF) # lots of fixing here
  if(is.null(fit))return(NULL)
  if(is.factor(DF[[strat_col]])){
    legend.labs <- levels(DF[[strat_col]])
  }else{
    legend.labs <- unique(DF[[strat_col]])
  }
  plot <- ggsurvplot(
    fit = fit,
    data = DF,
    size = 1,                 # change line size
    #palette =
    #  c("#E7B800", "#2E9FDF"),# custom color palettes
    conf.int = TRUE,          # Add confidence interval
    pval = TRUE,              # Add p-value
    risk.table = TRUE,        # Add risk table
    risk.table.col = "strata",# Risk table color by groups
    legend.labs = legend.labs,    # Change legend labels
    risk.table.height = 0.25, # Useful to change when you have multiple groups
    ggtheme = theme_bw()      # Change ggplot2 theme
  )
  return(plot)
}

DF <- lung
DF %>% names()
time_col <- "time"
status_col <- "status"
strat_col <- "sex"
# My survival curve function
# *** fix axis labels
make_survival_curve <- function(DF, time_col, status_col, strat_col,pval = T,risk.table = T){

  if(!is.data.frame(DF))stop("DF has to be a data.frame")
  # remove incomplete cases
  vars <-  c(time_col, status_col)
  if(!missing(strat_col))vars <-  append(vars, strat_col)
  DF <- DF[complete.cases(DF[,vars]), ]
  DF[["time_col"]] <- DF[[time_col]]
  DF[["status_col"]] <- DF[[status_col]]
  # fit with stratification if strat_col provided
  if(!missing(strat_col)){
    DF[["strat_col"]] <- DF[[strat_col]]
    fit <- survival::survfit(survival::Surv(time_col,status_col) ~ strat_col, data = DF)
  }else{
    # fit without stratification if no strat_col provided
    fit <- survival::survfit(survival::Surv(time_col,status_col) ~ 1, data = DF)
  }
  # handle missing fits
  if(is.null(fit))return(NULL)
  legend.labs <- NULL
  # create legend labels
  if(!missing(strat_col)){
    if(is.factor(DF[[strat_col]])){
      legend.labs <- levels(DF[[strat_col]])
    }else{
      legend.labs <- unique(DF[[strat_col]])
    }
  }

  plot <-  survminer::ggsurvplot(
    fit = fit,
    data = DF,
    size = 1,                 # change line size
    #palette =
    #  c("#E7B800", "#2E9FDF"),# custom color palettes
    conf.int = TRUE,          # Add confidence interval
    pval = pval,           # Add p-value
    risk.table = risk.table,
    surv.scale = "percent",# Add risk table
    #risk.table.col = strat_col,# Risk table color by groups
    legend.labs = legend.labs,    # Change legend labels
    risk.table.height = 0.25, # Useful to change when you have multiple groups
    ggtheme = theme_bw()      # Change ggplot2 theme
  )
  return(plot)
}
make_survival_curve(DF,"time","status")


# test function: plot Melanoma data
# Recode 'status' column: 1 = died from melanoma (event), others = censored
data(Melanoma)
Melanoma$status_recode <- ifelse(Melanoma$status == 1, 1, 0)

# Use the function
plot <- make_survival_curve(
  DF = Melanoma,
  time_col = "time",
  status_col = "status_recode",
  strat_col = "sex"
  # what should `event_statuses` argument do?
  # event_statuses = 1
)

# attempt with lung data
plot2 <- make_survival_curve(
  DF = lung,
  time_col = "time",
  status_col = "status",
  strat_col = "sex"
)

# Display the plots
plot
plot2



# plot 3: scatterplot (WIP, stable version: 20250105_makescatterplot.R)
# Edit basic plotly scatterplot to set size of points to scale of a continuous
#   variable
##### Example standard plot_ly
# Note: plot_ly takes columns as functions (I wonder why?); must use ~
fig <- plot_ly(
  data = mtcars,
  x = ~disp,
  y = ~mpg,
  size = ~wt,
  # gear should be a factor in this case:
  color = ~as.factor(gear)
)
# plot_ly help file doesn't mention x,y arguments. Why? It uses x,y arguments
#   of a sub-function? Something to do with the trace data?
fig



##### Wrapper
make_scatterplot <- function(DF, x_col, y_col, color_col = NULL, size_col = NULL) {

  # Ensure DF is a data frame
  if (!is.data.frame(DF)) stop("DF has to be a data.frame")
  # Check if x_col and y_col exist in DF
  if (!(x_col %in% colnames(DF)) || !(y_col %in% colnames(DF))) {
    stop("Invalid variable names.")
  }
  # Check if color_col exists DF, if provided
  if (!is.null(color_col) && !(color_col %in% colnames(DF))) {
    stop("color_col does not exist.")
  }
  # Check if size_col exists in DF, if provided
  if (!is.null(size_col) && !(size_col %in% colnames(DF))) {
    stop("size variable does not exist.")
  }
  # If color_col provided, ensure it is numeric, factor, or convert character to factor
  if (!is.null(color_col)) {
    color_data <- DF[[color_col]]
    # Factorize color_col if it is a character vector
    if (is.character(color_data)) {
      DF[[color_col]] <- as.factor(color_data)
      # Stop if color_col is not a factor or numeric type vector
    } else if (!is.factor(color_data) && !is.numeric(color_data)) {
      stop("color variable must be numeric, factor, or character.")
    }
  }
  # If size_col provided, ensure it is numeric
  if (!is.null(size_col)) {
    size_data <- DF[[size_col]]
    if (!is.numeric(size_data)) {
      stop("size variable must be numeric.")
    }
  }

  # Remove incomplete cases
  vars <- c(x_col, y_col)
  if (!is.null(color_col)) vars <- c(vars, color_col)
  if (!is.null(size_col)) vars <- c(vars, size_col)
  DF <- DF[complete.cases(DF[, vars]), ]

  # Normalize marker size if size_col provided
  marker_size <- if (!is.null(size_col)) {
    10 * DF[[size_col]] / max(DF[[size_col]])
  } else {
    10
  }

  # Initialize plot
  fig <- plotly::plot_ly(
    data = DF,
    x = DF[[x_col]],
    y = DF[[y_col]],
    type = 'scatter',
    mode = 'markers',
    marker = list(
      size = marker_size,
      sizemode = 'diameter'
    ),
    color = if (!is.null(color_col)) { DF[[color_col]] } else { NULL }
    # Hover text shows (x,y) values. We could change it to show size, if desired
    #  text = if (!is.null(size_col)) { paste0(size_col, ': ', DF[[size_col]]) } else { NULL },
    #  hoverinfo = 'text'
  ) %>%
    layout(
      # plotly takes lists for labels (why?) so we can't just use
      #    legend = as_string(vector)
      xaxis = list(title = x_col),
      yaxis = list(title = y_col),
      legend = list(
        title = list(
          text = if (!is.null(color_col)) color_col else NULL
        )
        # these give an ugly legend title:
        # legend = list(title = as_string(color_col))
        # legend = list(title = list(text = if (!is.null(color_col)) as_string(color_col) else "Legend"))
      )
      # Note: plotly doesn't create a legend for size, only color
    )

  # Return output fig
  return(fig)
}



##### Test make_scatterplot

# My function explicitly retrieves argument columns from DF, so they don't need
#   to be passed as functions. It requires columns wrapped in single quotes.
#   To change this, we could use non-standard evaluation(?)

# Test with all variables
# using double size vector (continuous) and integer color vector (discrete)
make_scatterplot(
  DF = mtcars,
  x_col = 'disp',
  y_col = 'mpg',
  color_col = 'gear',
  size_col = 'wt'
)

# Make gear variable a factor
mtcars$gear <- as.factor(mtcars$gear)
make_scatterplot(
  DF = mtcars,
  x_col = 'disp',
  y_col = 'mpg',
  color_col = 'gear',
  size_col = 'wt'
)
#   Note: plot_ly does not create titles for numeric color legend

# Test with no quotes
make_scatterplot(
  DF = mtcars,
  x_col = disp,
  y_col = mpg,
  color_col = qsec,
  size_col = wt
)
# It can't find DF's columns

# Test without color_col
make_scatterplot(
  DF = mtcars,
  x_col = 'disp',
  y_col = 'mpg',
  size_col = 'wt'
)

# Test without size_col
make_scatterplot(
  DF = mtcars,
  x_col = 'disp',
  y_col = 'mpg',
  size_col = 'wt'
)
# Note: no legend for size. Should we display point size in hover text?
#   Create a legend, even?

# Test without size_col and color_col
make_scatterplot(
  DF = mtcars,
  x_col = 'disp',
  y_col = 'mpg'
)

# Test with character data
# Note: iris$Species is a character vector
make_scatterplot(
  DF = iris,
  x_col = 'Sepal.Length',
  y_col = 'Sepal.Width',
  color_col = 'Species',
  size_col = 'Petal.Width'
)







print(DB$metadata)
# issue?: DB$data is empty
print(DB$data)
str(DB$data)
print(DB$data[[]])

# Use the refreshing pak install
# repository version: to see fxns to test, we wanna pull the most recent repo;
#    use devtools_load_all() - this makes internal fxns visible.

# Unit tests -----

## list_to_excel -----

# attempting to export my DB
list_to_excel(DB$data, ".")
# previous error: "returns [1] NA" when trying to list DB$metadata_only (?)

# 1. Demonstrate exporting mtcars data to Excel file
# bug?: list_to_excel works on a list comprised of one data.frame, but errors
# if given list of vectors. Compare:
# A. list of 1 data.frame:
mtcars_list_df <- list(mtcars)
# B. simple list of atomic vectors:
mtcars_list <- as.list(mtcars)
# Export list of 1 data.frame
list_to_excel(
  mtcars_list_df,
  dir = "./output",
  file_name = "mtcarsReport"
  )
# Export list of atomic vectors (throws error: list must be a list)
list_to_excel(
  mtcars_list,
  dir = "./output",
  file_name = "mtcarsReport2"
)

# demonstrate reading the mtcarsReport.xlsx file
mtcars_import <- excel_to_list(path = "./output/mtcarsReport.xlsx")

# Check if original list and imported list are the same
#   All mtcars_list vectors are numeric, but all mtcars_import vectors are char.
#   They both contain a df with the same number of observations and variables.
# Question: will every vector of a list produced by excel_to_list be character class?
# If so, we can coerce the original df vectors to character:
mtcars_list_df[[1]] <- data.frame(lapply(mtcars_list_df[[1]], as.character))
# Then check if they're identical:
identical(mtcars_list_df, mtcars_import) # FALSE; lists are not identical.
# Maybe because data.frame is unnamed in mtcars_import but named in mtcars_list_df?
mapply(identical, mtcars_list_df, mtcars_import) # TRUE; their vectors are identical!

# 2. Demo with iris data
iris_list <- as.list(iris)
# export to excel
list_to_excel(
  iris_list,
  dir = "./output",
  file_name = "irisReport"
)
# bug: list arg must be a list, but it is:
class(iris_list)

# 3. Demo with list of many dfs
df1 <- data.frame(A = 1:3, B = c("x", "y", "z"))
df2 <- data.frame(X = c("a", "b", "c"), Y = 4:6)
test_list <- list(Sheet1 = df1, Sheet2 = df2)
list_to_excel(
  test_list,
  dir = "./output",
  file_name = "testReport"
  )

test_import <- excel_to_list("./output/testReport.xlsx")
# check if identical
identical(test_list, test_import) # FALSE; lists are not identical.
mapply(identical, test_list, test_import) # FALSE

# Unit testing
# library(testthat)
# library(usethis)
usethis::use_test("excel_list_elements_match")
usethis::use_test("excel_file_creation")





## Dev notes -----

### list_to_excel()
# In help file, state file_name arg should not include file type extension

### excel_to_list()
# Needs a help file. function has no comments or arguments besides `path`.

# scrap -------

# Example plot using lung dataset
library("survminer")
library("survival")
lung <- survival::lung
fit <- survfit(Surv(time, status) ~ sex, data = lung)
ggsurvplot(fit, data = lung)
ggsurvplot(fit, data = lung, censor.shape="|", censor.size = 4)
ggsurvplot(
  fit,
  data = lung,
  size = 1,                 # change line size
  palette =
    c("#E7B800", "#2E9FDF"),# custom color palettes
  conf.int = TRUE,          # Add confidence interval
  pval = TRUE,              # Add p-value
  risk.table = TRUE,        # Add risk table
  risk.table.col = "strata",# Risk table color by groups
  legend.labs =
    c("Male", "Female"),    # Change legend labels
  risk.table.height = 0.25, # Useful to change when you have multiple groups
  ggtheme = theme_bw()      # Change ggplot2 theme
)

# end -----
