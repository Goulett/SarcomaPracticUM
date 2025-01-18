# Title: Plot #3 - Scatterplot
# Author: Natalie Goulett
# Date last edited: 2025/01/17



##### Objective
# Wrap plotly scatterplot to scale size of points by a continuous
#   variable
# plot_ly() has this feature built-in. It can also stratify groups with the
#   `color` argument. I'll make a wrapper function that does both.



##### Required packages
library(plotly)



##### Example standard plot_ly
# Note: plot_ly takes columns as functions (I wonder why?); must use `~`
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
# It can't find DF's columns :'(

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



##### Testing

# Create test file for when I get my function to work... stay tuned
usethis::use_test("make_scatterplot")



##### Questions for Dr. Rose
# 1. Do we want to keep the color variable? Should we allow a continuous color var?
# 2. If so, should we title the legend for continuous colors?
# 3. Do we want to create a legend for size? plot_ly does not make one.
# 4. Should we use non-standard evaluation (ie. rlang functions like enquo and
#    ensym) to allow unquoted arguments? Reasoning:

# This version of make_scatterplot works but requires quoted arguments. I'm
#   struggling to allow unquoted color_col and size_col arguments due to how R
#   and plot_ly handle data masking (I think).
#   If desired, I can work on figuring this out, but may not need to change
#   that if it's not a problem for the App. It just bothers me for some reason.
