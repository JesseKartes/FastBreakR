# Activate renv if used in the project
if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
}

# Enable strict formatting globally with styler
options(styler.strict = TRUE)

# Customize styling via styler (if you want manual control)
library(styler)

styler::style_pkg(
  line_length = 80,   # Set max line length to 80 characters
  strict = TRUE       # Apply strict formatting rules
)