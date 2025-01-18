# Title: Package setup
# Date created: 2025-01-17

# required packages
library(usethis)

# edit description file for authors, license

# Create a readme.md to explain my Repository authorship, purpose, license, etc.
use_readme_md()

# Keep track of changes during development with NEWS.md
use_news_md()

# Set up tests/testthat folder to perform unit testing
use_testthat()

# create Git Repository
use_git()

# edit .gitignore file to specify which files to not upload to repository.
#   I hid my folder "SarcomaPracticUM/scrap" by adding the line "scrap/" to my
#   .gitignore file. This is where I'll save old versions of code and other work
#   that shouldn't be kept on my repository.
