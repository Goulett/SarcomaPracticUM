# clear --------
#.rs.restartR()
#RosyUtils::clear_env()

# functions ------------
library("MASS")
library("RosyREDCap")
library("RosyDB")
library("plotly")
library("survival")
library("survminer")
library("lubridate")
library("tidyverse")

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

projects <- RosyDB::get_projects()  # load projects
DB <- load_RosyREDCap("PRAC")       # load REDCap data using "PRAC" token
# Run to update REDCap data
DB <- update_RosyREDCap(DB)         # update with latest REDCap data
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



# plot 3: scatterplot (WIP in make_scatterplot.R)
# Edit basic plotly scatterplot to set size of points to scale of a continuous
#   variable
# plot_ly() already has this feature. It can also stratify groups with the
#   `color` argument.

# test with standard plot_ly function
fig <- plot_ly(
  data = mtcars, x = ~disp, y = ~mpg,
  size = ~wt
  )
fig

# wrapper for plotly function
make_scatterplot <- function(DF, x_col, y_col, strat_col, size_col){

}

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
