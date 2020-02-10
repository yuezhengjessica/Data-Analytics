source("FUN.tool.r")
needs(zoo)

sd.rt = connect.and.query("SELECT * FROM daily_return") %>%
  arrange(Date) %>%
  mutate_at(vars(-Date), percent)

rollvar = function(x, width = 504){
  a = rollapply(x, FUN = 'var', width = width)
  b = c(rep(NA, width - 1), a)
  return(b)
}


sd.rt1 = sd.rt[,-1] %>%
  mutate_all(rollvar, 504)

sd.rt2 = bind_cols(Date = sd.rt[,1], sd.rt1) %>%
  arrange(desc(Date)) %>%
  mutate(Date = format(Date, format = "%Y-%m-%d")) %>%
  mutate(Date = as.Date.character(Date)) %>%
  mutate_at(vars(-Date), SD.day2year) %>%
  mutate_at(vars(-Date), `*`, 100)

#write.csv(sd.rt2, file = "individualSD.csv", row.names = FALSE)

con = readRDS("database_con.rds")
dbWriteTable(con, "proj_sd", sd.rt2, row.names = FALSE, overwrite = TRUE)

# update asset_pool
asset_pool.sd = data.frame(asset = colnames(sd.rt2)[-1], projectedSD = sd.rt2[1, -1] %>% unlist())

asset_pool = connect.and.query("SELECT * FROM asset_pool")
asset_pool$projectedSD = asset_pool.sd %>%
  arrange(asset) %>%
  select(projectedSD) %>%
  unlist()

projectedReturn =  connect.and.query("SELECT * FROM adjusted_rt LIMIT 1")[,-1] %>%
  unlist()
projectedReturn = projectedReturn[order(names(projectedReturn))]

asset_pool$projectedReturn = projectedReturn

dbWriteTable(con, "asset_pool", asset_pool, row.names = FALSE, overwrite = TRUE)

