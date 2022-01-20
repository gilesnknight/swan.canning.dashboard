library(lubridate)
library(chron)
library(timeDate)

sensorData  <- c("2459231.812", "2459231.833", "2459231.854", "2459231.875", "2459231.896", "2459231.917")
as.numeric(knownDate)
knownDate  <- sensorData[1]
knownDateTime  <- as.POSIXlt(as.numeric(knownDate)*86400,
                                origin=structure(-210866760000,
                                                    class=c("POSIXct", "POSIXt"),
                                                    tzone="Australia/Perth"),
                                tz="Australia/Perth")
knownDateTime #"2021-01-17 15:29:16 AWST"


# 1720693 days from 4713 BC to 0000
julian(d = 17, x = 1, y = 2021, origin = c(month = 1, day = 1, year = 0000))
# 738172 days since 0000
1720693 + 738172
# 2458865 days since 4713 BC
# off by 366!
2458865
2459231.812
