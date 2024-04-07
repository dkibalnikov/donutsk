
# Fulfill DESCRIPTION -------------------------------------------------------------------------------------------------------------------------------------
create_package(path = '~/Documents/donutsk/', fields = list( # GitHub first approach is used then create package project
  Package = "donutsk",
  Title = "Construct advanced donut charts",
  Description = "The Donutsk package allows you to build donut/pie charts with ggplot2 layer by layer, exploiting the advantages of polar symmetry."))
use_author(given = "Dmitry", family = "Kibalnikov", email = "d.kibalnikov@gmail.com", role = c("aut", "cre", "cph"))
use_mit_license("Kibalnikov Dmitry") # MIT license --> DESCRIPTION
use_github_links() # URL, BugReport --> DESCRIPTION
use_readme_rmd() # R markdown for README
use_logo("man/figures/logo.png")

# Package dependencies -----------------------------------------------------------------------------------------------------------------------------------
use_package("ggplot2", type = "Depends", min_version = "3.5.0")
use_package("dplyr", type = "Imports", min_version = "1.1.2")
use_package("rlang", type = "Imports", min_version = "1.1.1")
use_package("glue", type = "Imports", min_version = "1.6.2")
use_package("scales", type = "Suggests", min_version = "1.3.0")
use_package("stringr", type = "Suggests", min_version = "1.5.0")
use_package("tidyr", type = "Suggests", min_version = "1.3.0")

# Functions required --------------------------------------------------------------------------------------------------------------------------------------
use_package_doc() # setup separate document for importing functions

use_import_from("dplyr", c("all_of", "any_of", "arrange", "between", "bind_cols", "bind_rows", "distinct", "filter",
                           "group_by", "if_else", "lag", "left_join", "mutate", "n", "pick",
                           "row_number", "rowwise", "select", "summarise", "tibble", "ungroup"))
use_import_from("rlang", "enquo")
use_import_from("glue", "glue")

# Create source files -------------------------------------------------------------------------------------------------------------------------------------
use_r("packing")
use_r("utils")
use_r("layouts")
use_r("donut_geom")
use_r("donut_label")
use_r("pins")

# Testing -------------------------------------------------------------------------------------------------------------------------------------------------
usethis::use_testthat()
use_test("packing")

# Lifecycle preparation -----------------------------------------------------------------------------------------------------------------------------------
create_github_token() # Create token to support GitHub Actions
gitcreds::gitcreds_set() # Update token storage
use_pkgdown_github_pages() # Add site
use_github_action("check-standard") # Add badge R CMD check status
use_github_action("test-coverage")
use_news_md() # Add news
use_cran_comments()
use_release_issue()

# Lifecycle -----------------------------------------------------------------------------------------------------------------------------------------------
use_coverage() # reports test coverage
use_version()
build_readme() # Rebuild R markdown for README
build_site()

# Documentation -------------------------------------------------------------------------------------------------------------------------------------------
document()
use_data_raw("GDP_data")
use_vignette("real_life_example", title = "Real life example")


# Sticker -------------------------------------------------------------------------------------------------------------------------------------------------
library(hexSticker)

DONUTSK <- c("D", "O", "N", "U", "T", "S", "K") |> factor(levels = c("D", "O", "N", "U", "T", "S", "K"), ordered = T)

ptl <- petal(n = 6, bend = 0.1, scale = 3, thinner = F, thinner_gap = .2, rotate = 90)
set.seed(2021)
p <- tibble(lvl1 = rep(DONUTSK, each = 7), lvl2 = rep(DONUTSK, 7),
            value = sample(1:10, 49, replace =T),  highlight_ext = sample(c(F,T), 49, T, c(.9, .1))) |>
  mutate(highlight_int = if_else(lvl1 == "A", TRUE, FALSE)) |>
  #group_by(lvl1, lvl2, highlight_ext, highlight_int) |>
  #reframe(value = sum(value)) |>
  #packing(value, lvl1) |>
  arrange(lvl1, lvl2, value) |>
  ggplot(aes(value = value, fill = lvl1)) +
  geom_donut_int(aes(highlight = highlight_int), alpha=.5, r_int = .25,  col="gray10", linewidth=.1) +
  geom_label_int(aes(label = as.character(lvl1)), alpha = .8, col = "white", size=4, r = 1.1) +
  geom_donut_ext(aes(alpha = ordered(lvl2), highlight = highlight_ext), col="gray10", linewidth=.1) +
  #geom_pin(aes(col = lvl1), layout = ptl, size=.5, linewidth=.2, show.legend = F, cut=.2) +
  #geom_label_ext(aes(col = lvl1, label = paste0(lvl2, ": {scales::percent(.prc_grp)}")), show.legend = F, layout=ptl, size=3, col = "white", angle=90, hjust=1) +
  scale_fill_viridis_d(option = "inferno", begin = .1, end = .8) +
  scale_color_viridis_d(option = "inferno", begin = .1, end = .8) +
  coord_radial(theta = "y", expand = F, rotate_angle = T, start = -pi/2, end = pi/2) +
  xlim(0, 4) +
  guides(alpha = guide_legend(ncol = 2), fill = guide_legend(ncol = 2)) +
  theme_void() +
  theme(legend.position = "none") +
  theme_transparent()

library(showtext)

## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Truculenta")
## Automatically use showtext to render text for future devices
showtext_auto()

sticker(p, package = "donutsk", s_x=1, s_y=1.75, s_width=3.5, s_height=3.5, dpi = 200, p_color = "mediumpurple4",h_color = "mediumpurple4", h_fill = "gray90",
        p_y=.7, p_size = 25,
        filename="man/figures/donutsk_logo.png", p_family="Truculenta")


