n <- 20

set.seed(2021)
df <- tibble(
  lvl1 = sample(LETTERS[1:5], n, TRUE),
  lvl2 = sample(LETTERS[6:24], n, TRUE),
  value = sample(1:20, n, TRUE)
)

# Arrange all values
simple <- structure(list(lvl1 = c("D", "D", "C", "C", "B", "D", "B", "A",
                                "D", "D", "C", "E", "C", "E", "E", "D", "D", "B", "B", "C"),
                       lvl2 = c("N", "K", "M", "V", "J", "K", "G", "W", "X", "T",
                                "W", "J", "W", "T", "P", "U", "K", "I", "V", "K"),
                       value = c(2L, 19L, 2L, 18L, 3L, 17L, 4L, 17L, 5L, 17L, 6L, 16L, 8L, 15L,
                                 9L, 14L, 9L, 12L, 9L, 11L)), row.names = c(NA, -20L),
                  class = c("tbl_df", "tbl", "data.frame"))

# Arrange values within values
grouped <- structure(list(lvl1 = c("A", "D", "D", "D", "D", "D", "D", "D",
                                "B", "B", "B", "B", "C", "C", "C", "C", "C", "E", "E", "E"),
                       lvl2 = c("W", "N", "K", "X", "K", "K", "T", "U", "J", "I",
                                "G", "V", "M", "V", "W", "K", "W", "P", "J", "T"),
                       value = c(17L,
                                 2L, 19L, 5L, 17L, 9L, 17L, 14L, 3L, 12L, 4L, 9L, 2L, 18L,
                                 6L, 11L, 8L, 9L, 16L, 15L)), row.names = c(NA, -20L),
                  class = c("tbl_df",
                            "tbl", "data.frame"))

test_that("simple arrangement", {
  expect_identical(packing(df, value), simple)
})

test_that("grouped arrangement", {
  expect_identical(packing(df, value, lvl1), grouped)
})
