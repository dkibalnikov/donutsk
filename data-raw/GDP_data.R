## code to prepare `GDP_data` dataset goes here

# GDP, PPP (current international $)
# wbstats::wb_search("GDP.+PPP")
GDP_PPP <- wbstats::wb_data("NY.GDP.MKTP.PP.CD")
# wbstats::wb_search("countries")
# str(wbstats::wb_cachelist, max.level = 1)


GDP <- left_join(GDP_PPP, wbstats::wb_cachelist$countries, c("iso3c", "country")) |>
  select(date, country, region, region_ISO = region_iso3c, GDP = NY.GDP.MKTP.PP.CD) |>
  filter(date > 1990 & !is.na(GDP)) |>
  tidyr::replace_na(list(region = "Other", region_ISO = "OTH"))


usethis::use_data(GDP, overwrite = TRUE)
