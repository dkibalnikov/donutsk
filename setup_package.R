
# Fulfill DESCRIPTION -------------------------------------------------------------------------------------------------------------------------------------
create_package(path = '~/Documents/donutsk/', fields = list( # GitHub first approach is used then create package project
  Package = "donutsk",
  Title = "Construct advanced donut charts",
  Description = "The Donutsk package allows you to build donut/pie charts with ggplot2 layer by layer, exploiting the advantages of polar symmetry."))
use_author(given = "Dmitry", family = "Kibalnikov", email = "d.kibalnikov@gmail.com", role = c("aut", "cre"))
use_mit_license("Kibalnikov Dmitry") # MIT license --> DESCRIPTION
use_github_links() # URL, BugReport --> DESCRIPTION
use_readme_rmd() # R markdown for README
build_readme() # Rebuild R markdown for README

#  Package dependencies -----------------------------------------------------------------------------------------------------------------------------------
use_package("ggplot2", type = "Depends", min_version = "3.5.0")
use_package("dplyr", type = "Imports", min_version = "1.1.2")
use_package("rlang", type = "Imports", min_version = "1.1.1")
use_package("glue", type = "Imports", min_version = "1.6.2")
use_package("scales", type = "Suggests", min_version = "1.3.0")

# Functions required --------------------------------------------------------------------------------------------------------------------------------------
use_package_doc() # setup separate document for importing functions

use_import_from("dplyr", c("all_of", "any_of", "arrange", "between", "bind_cols", "distinct",
                           "group_by", "if_else", "lag", "left_join", "mutate", "n", "pick",
                           "row_number", "rowwise", "select", "summarise", "tibble", "ungroup"))
use_import_from("rlang", "enquo")
use_import_from("glue", "glue")

# Create source files -------------------------------------------------------------------------------------------------------------------------------------
use_r("packing")
use_r("utils")
use_r("layouts")
use_r("geom_donut")
use_r("donut_label")
use_r("pins")

# Testing -------------------------------------------------------------------------------------------------------------------------------------------------
usethis::use_testthat()
use_test("packing")
