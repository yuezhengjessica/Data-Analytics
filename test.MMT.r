source("FUN.tool.r")
source("FUN.routine.r")

# ============================================
daily_rt = connect.and.query(paste0("SELECT * FROM daily_return")) %>%
  mutate(Date = as.Date.character(Date)) %>%
  mutate_at(vars(-Date), percent)%>%
  filter(Date >= as.Date.character("2013-07-06") & Date <= as.Date.character("2016-07-08"))

cum_rt.dataframe = daily_rt  %>%
  filter(Date >= as.Date.character("2013-07-06") & Date <= as.Date.character("2016-07-08")) %>%
  arrange(Date) %>%
  mutate_at(vars(-Date), `+`, 1) %>%
  mutate_at(vars(-Date), cumprod)

# ============================================

if(TRUE){ # general look
  gg.each_column(cum_rt.dataframe, "Date")
}

if(TRUE){ # look at the momentum comparison
  
}