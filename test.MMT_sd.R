#"Low", "Medium", "High"
riskTolerance="High"

# "Short", "Intermediate", "Long"
investmentHorizon="Intermediate"

stress_drawdown = -15

DATE = "2017-06-13" %>% as.Date.character()

#' change preference
SD_1year.max = if(riskTolerance == "Low") {
  2.5
} else if(riskTolerance == "Medium"){
  3.5
} else if(riskTolerance == "High"){
  4.5
} else {stop("invalid riskTolerance")}

source("FUN.tool.r")

data.asset_pool = GET.column("asset_pool", column = "asset, category, credit, maturity")

slec.tickers = applyPreference(data.asset_pool, excludeSheet, riskTolerance, investmentHorizon)$asset %>%
  check.valid.tickers(DATE-7,DATE, 504) %>% # temp
  paste0(collapse = ",")

forward_rt = GET.rt(tickers = slec.tickers, 
                    DATE = DATE, 
                    table = "adjusted_rt",
                    days.all = 1,
                    delim = ",") %>%
  mutate(Date = as.Date.character(Date)) %>%
  filter(!is.na(Date)) %>%
  replace(is.na(.), 0)


daily_rt = GET.rt(tickers = slec.tickers, 
                  DATE = DATE, 
                  table = "daily_return",
                  days.all = 504,
                  delim = ",") %>%
  mutate(Date = as.Date.character(Date)) %>%
  filter(!is.na(Date)) %>%
  replace(is.na(.), 0)


stress_rt = GET.column(table = "stressful_period_rt", # 08 constraint
                       column = slec.tickers) %>%
  as.numeric()

# main ===========================
daily_rt = daily_rt
proj_rt = forward_rt
DATE = DATE
days.lookBackSD = 504
days.lookBackMmt = 105
days.excludeMmt = 21
SD_1year.max = SD_1year.max
rt_1quater.min = -3
forward_rt_type = "projected"

cov_period = cov.period(DATE, days.lookBackSD, daily_rt)

SR_today = GET.value.lookup(table = "scaling", 
                            col2lookup = "Scaling_Ratio", 
                            col.name = "Date", 
                            col.value = DATE)

cov2 = cov.matrix2(cov_period, SR_today)
#cov2 = cov(cov_period)
result = opti.wets.donlp2(SD_1year.max, rt_1quater.min, proj_rt, cov2, stress_rt, stress_drawdown)
