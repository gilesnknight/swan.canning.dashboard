library(lubridate)
library(chron)
library(timeDate)

"sensor_repository_00800"

sensorData  <- c("2459231.812", "2459231.833", "2459231.854", "2459231.875", "2459231.896", "2459231.917")
as.numeric(knownDate)
knownDate  <- sensorData[1]
knownDateTime  <- as.POSIXlt(as.numeric(knownDate)*86400,
                                origin=structure(-210866760000,
                                                    class=c("POSIXct", "POSIXt"),
                                                    tzone="Australia/Perth"),
                                tz="Australia/Perth")
knownDateTime #"2021-01-17 15:29:16 AWST"

# 2459216 days between 4713 BC and 2021-01-17



# 1721424 days between 4713 BC and 0001-01-01
# 738,174 days between 0001-01-01 and 2021-01-17
1721424+738174
2459598 - 2459216
2459216- 2459194

aug_birth <- ymd("0000-09-23") - years(63 - 1)

parse_bce_ymd <- function(str) {
  regex <- "(\\d{4})(-\\d{2}-\\d{2})"
  match <- stringr::str_match(str, regex)
  years_n <- readr::parse_number(match[, 2]) - 1 # Beware the -1 here
  right_side <- match[, 3]
  date <- ymd(paste0("0000-",right_side)) - lubridate::years(years_n)
  return(date)
}
# Test the function.
aug_birth <- parse_bce_ymd("4713-01-01")
aug_death <- ymd("2021-01-17")
age <- aug_death - aug_birth
age
as.numeric(age)

lubridate::duration(age, "seconds")

duration  <- as.duration(age)
duration$
#> [1] "2395353600s (~75.9 years)"
# Yay that's correct!



# 1720693 days from 4713 BC to 0000
julian(d = 17, x = 1, y = 2021, origin = c(month = 1, day = 1, year = 0000))
# 738172 days since 0000
1720693 + 738172
# 2458865 days since 4713 BC
# off by 366!
2458865
2459231.812
