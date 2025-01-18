# Title: Setup Github Repository
# Author: Natalie Goulett
# Date created: 2025-01-17
# references: How to Use Git/Github with R by David Keyes
#   https://rfortherestofus.com/2021/02/how-to-use-git-github-with-r


# Objective -----
# I'll use Github for version control and collaboration on my practicum project.
#   This script explains my process for creating a Github repository using an
#   existing RStudio project.



# Required packages -----
library(usethis)
install.packages("gitcreds")
library(gitcreds)



# Setup ----
# I edited the description file authors, license

# Create a readme.md to explain my Repository authorship, purpose, license, etc.
use_readme_md()

# Keep track of changes during development with NEWS.md
use_news_md()

# Set up tests/testthat folder to perform unit testing
use_testthat()

# Create Git Repository
use_git()

# edit .gitignore file to specify which files to not upload to repository.
#   I hid my folder "SarcomaPracticUM/scrap" by adding the line "scrap/" to my
#   .gitignore file. This is where I'll save old versions of code and other work
#   that shouldn't be kept on my repository.

# Configure Git
#   here I enter my credentials for Github
usethis::edit_git_config(user.name = "Goulett", user.email = "no@nope.edu")
# or, to edit file directly:
usethis::edit_git_config()

# create PAT
create_github_token()
gitcreds::gitcreds_set()
# I store my PAT here

# create repository on my Github
use_github()

