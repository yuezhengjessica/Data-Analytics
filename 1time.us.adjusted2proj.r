source("FUN.tool.r")

needs(zoo, tidyr)

moment.days = 50
exclu.days = 10
days.lookBackSD = 504

adjusted_rt = connect.and.query(paste0("SELECT * FROM adjusted_rt")) %>%
  mutate(Date = as.Date.character(Date)) %>%
  mutate_at(vars(-Date), percent) %>%
  filter(!is.na(Date)) %>%
  replace(is.na(.), 0)
  

daily_rts = connect.and.query(paste0("SELECT * FROM daily_return_pre")) %>%
  mutate(Date = as.Date.character(Date)) %>%
  mutate_at(vars(-Date), percent) %>%
  filter(!is.na(Date))

######################
if(TRUE){# old MMT
  daily_rts_sum = daily_rts %>%
    arrange(Date) %>%
    replace(is.na(.), 0) %>%
    mutate_at(vars(-Date), roll_apply, width = moment.days, FUN = "sum") %>%
    arrange(desc(Date))
  
  n_MA = nrow(daily_rts_sum)
  
  daily_shifted =  data.frame(Date = daily_rts_sum$Date[1: (n_MA - exclu.days - moment.days)],
                              daily_rts_sum[(exclu.days +1): (n_MA - moment.days), -1])
  
  MMT = daily_shifted %>%
    mutate_at(vars(-Date), `-`, 0.0025) %>%
    mutate_at(vars(-Date), abs.threshold, limit = 0.0025, replace.with = 0) %>%
    mutate_at(vars(-Date), `*`, 0.5)
  
} else {# trail MMT. New, but unstable
  daily_rts_sum = daily_rts %>%
    arrange(Date) %>%
    replace(is.na(.), 0) %>%
    #mutate_at(vars(-Date), roll_apply, width = moment.days, FUN = "sum") %>%
    arrange(desc(Date))
  
  n_MA = nrow(daily_rts_sum)
  
  daily_shifted =  data.frame(Date = daily_rts_sum$Date[1: (n_MA - exclu.days - moment.days)],
                              daily_rts_sum[(exclu.days +1): (n_MA - moment.days), -1])

  MMT = daily_shifted %>%
    arrange(Date) %>%
    mutate_at(vars(-Date), MMT.capture)
}

#######################
full_date = as.Date.n(Reduce(union, list(adjusted_rt$Date, MMT$Date))) %>% sort()
full_date = data.frame(Date = full_date)
full_adjusted = full_join(full_date, adjusted_rt, by = "Date")
full_MMT = full_join(full_date, MMT, by = "Date")

proj_rt = data.frame(Date = full_date,
                     full_adjusted[,-1] + full_MMT[,-1]) %>%
  arrange(desc(Date))

#######################
nnnn = nrow(daily_rts)

for(i in 2:dim(daily_rts)[2]){
  first_non_na = max(which(!is.na(daily_rts[,i])))
  cutoff = daily_rts[first_non_na - floor(days.lookBackSD / 2),]$Date
  cutoff_rt = daily_rts[first_non_na,]$Date
  proj_rt[,i] = na.approx(proj_rt[,i], proj_rt[,1], na.rm = FALSE)
  
  if (first_non_na < nnnn){
    rt.mean = mean(filter(daily_rts, Date > cutoff_rt & Date < cutoff)[,i], na.rm = T)
    rt.sd = sd(filter(daily_rts, Date > cutoff_rt & Date < cutoff)[,i], na.rm = T)
    
    daily_rts[(first_non_na + 1):nnnn, i] = rnorm((nnnn - first_non_na), rt.mean, rt.sd)
    
  }

  proj_rt[proj_rt$Date < cutoff,i]= 0
  proj_rt[,i] = replace(proj_rt[,i], is.na(proj_rt[,i]), 0)
}
proj_rt = proj_rt[-1,]

# gg.compare(adjusted_rt, proj_rt, compare_column = "SJNK")

#######################
con <- dbConnect(MySQL(),
                 user="treesrobo", password="treesrobo",
                 dbname="treesrobo", host="treesrobo-db.crw1otiz3w41.us-east-1.rds.amazonaws.com")

#write.csv(proj_rt, file = "proj_return.csv", row.names = FALSE)
dbWriteTable(con, "proj_return", proj_rt %>% mutate_at(vars(-Date),`*`,100), row.names = FALSE, overwrite = TRUE)
dbWriteTable(con, "daily_return", daily_rts %>% mutate_at(vars(-Date),`*`,100), row.names = FALSE, overwrite = TRUE)
# dbWriteTable(con, "adjusted_rt", adjusted_rt %>% mutate_at(vars(-Date),`*`,100), row.names = FALSE, overwrite = TRUE)

dbDisconnect(con)



