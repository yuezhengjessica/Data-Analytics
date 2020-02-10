needs(dplyr)

if(exists("input")){
  
} else {
  #"Low", "Medium", "High"
  riskTolerance="High"
  
  # "Short", "Intermediate", "Long"
  investmentHorizon="Intermediate"
  
  stress_drawdown = -15
  
  DATE = "2017-06-13" %>% as.Date.character()
}


# -----------------------------------------------------------------------------------------
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
                    table = "proj_return",
                    days.all = 1,
                    delim = ",")

daily_rt = GET.rt(tickers = slec.tickers, 
                  DATE = DATE, 
                  table = "daily_return",
                  days.all = 504,
                  delim = ",")

stress_rt = GET.column(table = "stressful_period_rt", # 08 constraint
                       column = slec.tickers) %>%
  as.numeric()

weights = opti.wets(daily_rt = daily_rt, 
                    forward_rt = forward_rt, 
                    DATE = DATE, 
                    days.lookBackSD = 504, 
                    days.lookBackMmt = 105, 
                    days.excludeMmt = 21, 
                    SD_1year.max = SD_1year.max, 
                    rt_1quater.min = -3, 
                    forward_rt_type = "projected",
                    stress_rt,
                    stress_drawdown) # actually is "adjusted" but things are not clear


# Portfolio check
SR_portflio_DATE = GET.value.lookup(table = "scaling", 
                                    col2lookup = "Scaling_Ratio", 
                                    col.name = "Date", 
                                    col.value = DATE)
cov_matrix2 = cov.matrix2(cov.period(DATE, daily_rt = daily_rt), SR_portflio_DATE)
portfolio_SD = weights%*%cov_matrix2%*%weights %>% SD.day2year()

# showing the result ----------------------------------------------------------
names(weights) = names(daily_rt[-1])
print(weights %>% round(digits = 4))
barplot(weights)
