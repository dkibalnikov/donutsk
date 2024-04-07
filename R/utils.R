# Calculates coordinates for polar system
calc_coords <- function(df, val){
  ymin <- ymax <- NULL
  val <- enquo(val)

  mutate(df,
         ymax = (!!val/sum(!!val)) |> cumsum(),
         ymin = lag(ymax, 1, 0),
         y = (ymin + ymax)/2)
}


# Calculates aggregation for internal level
calc_aggr <- function(df, val, lvl, r_int=0, r_ext=1){
  .sum <- NULL
  lvl <- enquo(lvl)
  val <- enquo(val)

  all_aes <- c("label", "PANEL", "group", "highlight", "fill", "alpha", "linetype",
               "linewidth", "angle", "colour", "hjust", "vjust", "size")

  group_by(df, !!lvl, pick(any_of(all_aes))) |>
    mutate(.sum = sum(!!val),
           .mean = mean(!!val),
           .median = stats::median(!!val),
           .n = n())  |>
    select(-!!val) |>
    ungroup() |>
    distinct() |>
    mutate(.prc = .sum/sum(.sum)) |>
    calc_coords(.sum)
}
