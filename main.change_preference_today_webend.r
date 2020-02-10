needs(dplyr)

whether.debug = FALSE
if(exists("input")){
  data = fromJSON(toJSON(input))
  
  ID = as.integer(data$id)
  riskTolerance = as.character(data$riskLevel)
  investmentHorizon = as.character(data$investmentHorizon)
  totalAsset =  as.integer(data$totalAsset)
  stress_drawdown = -15
  #message("exists input!")
} else {
  #"Low", "Medium", "High"
  riskTolerance="Medium"
  
  # "Short", "Intermediate", "Long"
  investmentHorizon="Intermediate"
  
  stress_drawdown = -15
  ID = 1
}


# -----------------------------------------------------------------------------------------
#' change preference
SD_1year.max = switch(riskTolerance,Low=2.5,Medium=3.5,High=4.5)

# data import and library----------------------------------------
needs(dplyr, Rdonlp2, jsonlite, RMySQL, tidyr, purrr, ggplot2)

connect.and.query = function(query, type = "get", dbGetQuery = FALSE){
  con <- dbConnect(MySQL(),
                   user="treesrobo", password="treesrobo",
                   dbname="treesrobo", host="treesrobo-db.crw1otiz3w41.us-east-1.rds.amazonaws.com")
  
  if (dbGetQuery){
    dbGetQuery(con, query)
    result = "Updated in GET mode!"
  } else if (type == "get"){
    rs = dbSendQuery(con, query)
    result = dbFetch(rs, -1)
  } else if (type == "update") {
    dbSendQuery(con, query)
    result = "Updated!"
  }
  dbClearResult(dbListResults(con)[[1]])
  
  dbDisconnect(con)
  result
}

GET.value.lookup = function(table = "scaling", col2lookup = "Scaling_Ratio", col.name = "Date", col.value){
  # General function
  # get one value from column "col2lookup" where "col.name" == "col.value"
  # default use is to get value from today's volatility
  statement = paste0("SELECT ", col2lookup, " FROM ", table, " WHERE ", col.name, "=\'", col.value, "\'")
  connect.and.query(statement, type = "get") %>% as.numeric()
}

GET.n.until = function(column, table, n, DATE){
  statement = if(table %in% c("proj_return", "adj_return", "TYM", "daily_TYM")){
    paste0("SELECT ", column, " FROM ", table, " WHERE Date < \'", DATE, "\' LIMIT ", n)
  } else {
    paste0("SELECT ", column, " FROM ", table, " WHERE Date <= \'", DATE, "\' LIMIT ", n)
  }
  connect.and.query(statement, type = "get")
}

GET.column = function(table, column){
  statement = paste0("SELECT ", column, " FROM ", table)
  connect.and.query(statement, type = "get")
}

GET.rt.dateRange = function(first_day, last_day, tickers = slec.tickers, table = "daily_return"){
  # ad-hoc function
  
  day.diff = last_day - first_day
  statement = paste0("SELECT Date,", tickers, " FROM ", table, " WHERE Date BETWEEN \'", first_day, "\' AND \'", last_day, "\'")
  a = connect.and.query(statement) %>%
    # arrange(Date) %>%
    mutate_at(vars(-Date), percent)
  
  # cat(day.diff);cat(" days");cat("\n")
  # cat(nrow(a)-1); cat(" trading days")
  
  return(a)
}

GET.rt = function(tickers = "SHV,SHY,IEI,AGZ,STIP,TIP", table, 
                  DATE, days.all = 504, delim = ",", percent_ = TRUE){
  if(table == "proj_return" & days.all == 504) {days.all = 1}
  if(table == "proj_return"){DATE = DATE - 1}
  
  a = strsplit(tickers, split = delim)[[1]] %>%
    paste0(collapse = ",")
  d = paste0("Date,", a)
  
  daily_rt = GET.n.until(d, table, days.all, DATE)
  if(isTRUE(percent_)){
    daily_rt = daily_rt %>%
      mutate_at(vars(-Date), percent)
  }
  
  
  #cat(paste0("got ", days.all, " day(s) data from "))
  #cat(table)
  
  if(nrow(daily_rt) < days.all){
    warning("less data rows than expected!!!")
    cat(paste("date range of the target dataset is from", min(daily_rt$Date), "to", max(daily_rt$Date)))
  }
  return(daily_rt)
}

GET.latest_nrow.by = function(tickers = "SHV,SHY,IEI,AGZ,STIP,TIP", table, 
                              by = "Date", nrows = 504, delim = ","){
  a = strsplit(tickers, split = delim)[[1]] %>%
    paste0(collapse = ",")
  d = paste0("Date,", a)
  
  statement = paste0("SELECT ",d," FROM ", table, " ORDER BY ", by, " DESC LIMIT ",nrows)
  
  rts = connect.and.query(statement, type = "get")
  return(rts)
}

GET.value = function(table, column, ID){ #from old script
  con <- dbConnect(MySQL(),
                   user="treesrobo", password="treesrobo",
                   dbname="treesrobo", host="treesrobo-db.crw1otiz3w41.us-east-1.rds.amazonaws.com")
  statement = paste0("SELECT ", column, " FROM ", table, " WHERE id=", ID)
  rs = dbSendQuery(con, statement)
  result = fetch(rs, 10)[1,1]
  dbClearResult(dbListResults(con)[[1]])
  
  dbDisconnect(con)
  result
}

GET.value.2dim = function(table, column, row.title, row.value){ #from old script
  #GET entry value AT the cross of column and row.title=row.value
  con <- dbConnect(MySQL(),
                   user="treesrobo", password="treesrobo",
                   dbname="treesrobo", host="treesrobo-db.crw1otiz3w41.us-east-1.rds.amazonaws.com")
  
  statement = paste0("SELECT ", column, " FROM ", table, " WHERE ", row.title, "=", row.value)
  rs = dbSendQuery(con, statement)
  result = fetch(rs, 10)[1,1]
  dbClearResult(dbListResults(con)[[1]])
  
  dbDisconnect(con)
  result
}


UPDATE.row.exclude = function(Frame, table = "alloc_asset_dist", exclude = "id|username", ID = 1){ #from old script
  # Will upload the content in "upload" column of dataframe named "Frame"
  con <- dbConnect(MySQL(),
                   user="treesrobo", password="treesrobo",
                   dbname="treesrobo", host="treesrobo-db.crw1otiz3w41.us-east-1.rds.amazonaws.com")
  
  statement.colnames = paste0("SELECT * FROM ", table, " WHERE id=", ID)
  col.names = dbSendQuery(con, statement.colnames) %>%
    fetch(10) %>%
    select(-matches(exclude)) %>%
    colnames()
  dbClearResult(dbListResults(con)[[1]])
  
  col.names0 = paste0(col.names, "=0")
  all.equal.zero = paste0(col.names0, collapse = ",")
  statement.clear.row = paste0("UPDATE ", table, " SET ", all.equal.zero, " WHERE id=", ID)
  dbSendQuery(con, statement.clear.row)
  dbClearResult(dbListResults(con)[[1]])
  
  a = Frame %>%
    mutate(arg = paste0(names, "=", upload))
  add.value.row = paste0(a$arg, collapse = ",")
  statement.add.value = paste0("UPDATE ", table, " SET ", add.value.row, " WHERE id=", ID)
  dbSendQuery(con, statement.add.value)
  dbClearResult(dbListResults(con)[[1]])
  
  dbDisconnect(con)
} 

# replaceDataSQL <- function(data, # a data frame
#                      tableName, # table name, possibly qualified
#                      ...){
#   
#   # WRITE THE DATA TO A LOCAL FILE
#   TEMPFILE  <-  tempfile(fileext='.csv', tmpdir = "D:\\Temp")
#   write.csv(data,TEMPFILE, row.names = FALSE)
#   on.exit(file.remove(TEMPFILE))
#   query.delete = paste("TRUNCATE TABLE", tableName)
#   
#   connect.and.query(query.delete, "update")
# 
#   query  <-  sprintf("LOAD DATA INFILE '%s' 
#                      INTO TABLE %s 
#                      FIELDS TERMINATED BY ','
#                      LINES TERMINATED BY '\\n'
#                      IGNORE 1 LINES;" , TEMPFILE,tableName)
#   
#   connect.and.query(query, "update", dbGetQuery = TRUE)
# }

# selection part ------------------------------------------------
# Hard coded variable==========
excludeSheet = array(data = '', dim = c(3,3,3), dimnames = list(c("Low", "Medium", "High"), #riskTolerance
                                                                c("Short", "Intermediate", "Long"), #investmentHorizon
                                                                c("category", "credit", "maturity"))) #column names used to exclude certain assets

# if there are more than one features to be excluded under one column, use "|" to seperate
# For example: "Short|Ultral_short"
excludeSheet[1,1,1]="REIT";   excludeSheet[1,1,2]="Sub_Investment_Grade";  excludeSheet[1,1,3]="Long|Ultral_Long"
excludeSheet[1,2,1]="REIT";   excludeSheet[1,2,2]="Sub_Investment_Grade"
excludeSheet[1,3,2]="Sub_Investment_Grade"
excludeSheet[2,1,3]="Long|Ultral_Long"
excludeSheet[3,1,3]="Long|Ultral_Long"
excludeSheet[which(excludeSheet == '')] = "Nothing"
#

# change of preference function
applyPreference = function(features, excludeSheet, riskTolerance="Low", investmentHorizon="Short"){
  exclude = function(features, excludeSheet, riskTolerance, investmentHorizon){
    ri.maping = function(riskTolerance, investmentHorizon){
      if(riskTolerance == "Low")    {ri.r = 1}
      if(riskTolerance == "Medium") {ri.r = 2}
      if(riskTolerance == "High")   {ri.r = 3}
      
      if(investmentHorizon == "Short") {ri.i = 1}
      if(investmentHorizon == "Intermediate") {ri.i = 2}
      if(investmentHorizon == "Long") {ri.i = 3}
      
      # $r and $i, risk tolerance and investment horizon repectively
      return(data.frame(r=ri.r, i=ri.i))
    }
    ri = ri.maping(riskTolerance, investmentHorizon)
    
    e.features = excludeSheet[ri$r, ri$i, ] %>% as.vector()
    
    foobar = features %>%
      filter(!grepl(e.features[1], category)) %>%
      filter(!grepl(e.features[2], credit)) %>%
      filter(!grepl(e.features[3], maturity))
    foobar
  }
  result = exclude(features, excludeSheet, riskTolerance, investmentHorizon)
  return(result)
}

# check format --------------------------------------------------
check.and.print = function(x){
  cat("\014")
  message(x)
}

check.dateRange = function(dataset, column = "Date"){
  a = dataset %>%
    arrange_(column)
  
  c(a[[column]][1], a[[column]][nrow(a)])
}

check.valid.tickers = function(tickers, first_day, last_day, lookback.days){
  forward_rts = GET.rt.dateRange(first_day, last_day, tickers %>% paste0(collapse = ","), table = "proj_return") %>% 
    mutate(Date = as.Date.character(Date)) 
  nnn = nrow(forward_rts)
  forward_rts = forward_rts %>%
    summarise_at(tickers,sum) %>%
    select_if(function(x){!is.na(x)})
  new_tickers = names(forward_rts)
  
  daily_rts = GET.rt(new_tickers %>% paste0(collapse = ","), 
                     DATE = last_day, 
                     table = "daily_return",
                     days.all = nnn + lookback.days,
                     delim = ",") %>% 
    mutate(Date = as.Date.character(Date)) %>%
    summarise_at(new_tickers,sum) %>%
    select_if(function(x){!is.na(x)})
  new_tickers = names(daily_rts)
  
  return(new_tickers)
}

# format conversion ---------------------------------------------
sigmoid = function(x) {
  1 / (1 + exp(-x))
}

as.Date.n = function(x){
  as.Date.numeric(x, origin = "1970-01-01")
}

SD.year2day = function(SD_year, is.percent = FALSE){
  SD = if (is.percent == TRUE){
    SD_year/100 
  } else if (is.percent == FALSE) {
    SD_year
  }
  SD^2/252
}

SD.day2year = function(SD_day){
  # input is actually variance instead of SD!!!
  # output is SD :: sqrt()
  sqrt(SD_day*252)
}

rt.quater2year = function(rt_quater){
  (1 + as.numeric(rt_quater))^4 - 1
}

rt.year2quater = function(rt_year){
  (rt_day + 1)^0.25 - 1
}

percent = function(x){x / 100}

YTM2adjust_rt = function(YTM){
  # !!! currently no content ####
  adjusted_rt = YTM
  return(adjusted_rt)
}

shorten.rts = function(dataset, exclude = "Date"){
  dataset = dataset %>% tbl_df()
  tickers = names(dataset)
  tickers = tickers[-which(tickers == exclude)]
  a = apply(dataset %>% select_(paste0("-",exclude)), 1, list) %>%
    lapply(unlist)
  b = list(Date = dataset %>% select_(exclude) %>% unlist(), returns = a) %>%
    tbl_df() %>%
    mutate(Date =as.Date.n(Date))
  names(b)[1] = exclude
  return(b)
}

# MMT related -----------------------------------------------
needs(zoo)

rollsd = function(x, width = 504){
  a = rollapply(x, FUN = 'sd', width = width)
  b = c(rep(NA, width - 1), a)
  return(b)
}

roll_mean = function(x, width){
  a = rollapply(x, FUN = 'mean', width = width)
  b = c(rep(NA, width - 1), a)
  return(b)
}

roll_apply = function(x, width, FUN){
  a = rollapply(x, FUN = FUN, width = width)
  b = c(rep(NA, width - 1), a)
  return(b)
}

roll_apply_ = function(x, width, interval, FUN){
  a = rollapply(x, FUN = FUN, width = width)
  b = c(rep(NA, width - 1 + interval), a[1:(length(a) - interval)])
  return(b)
}

abs.threshold = function(x, limit, replace.with){
  # replace .. where abs of x < limit
  a = which(abs(x) < limit)
  x[a] = replace.with
  
  x
}

MMT.capture = function(x){ # about to be obsolete -20171102
  # x is one column of daily return, already arranged by Date
  x = cumprod(x + 1)
  
  MA_short = roll_mean(x, 10)
  MA_long = roll_mean(x, 120)
  
  DF0 = (x - MA_short) / log(9)
  DF1 = (MA_short - MA_long) / log(110)
  
  trend = (DF0 + DF1) / x
  #trend = trend / (exp(abs(trend))^2) * 5
  return(trend)
}

MMT.ratio.linear.exclude = function(x, tuning = 1){
  moment.days = 50
  exclu.days = 10
  days.lookBackSD = 504
  
  x = cumprod(x + 1)
  
  sum0 = roll_apply(x, width = moment.days, FUN = "sum") %>%
    rev() # arrange(desc(Date))
  
  n_MA = length(sum0)
  
  x_shifted = sum0[-1:-exclu.days] %>%
    append(rep(NA, exclu.days))
  
  MMT = (x_shifted - 0.0025) %>%
    abs.threshold(limit = 0.0025, replace.with = 0) %>%
    `*`(tuning) %>%
    rev() # arrange back: arrange(Date)
}

MMT.ratio.hedgeLog = function(x, tuning = 48){
  # x have to be already arranged by Date
  # [input] 
  x = cumprod(x + 1)
  #sd_x = rollsd(x, 504)
  
  MA_short = roll_mean(x, 5)
  MA_long = roll_mean(x, 120)
  
  DF0 = (x - MA_short) / log(4)
  DF1 = (MA_short - MA_long) / log(100)
  
  trend = (DF0 + DF1) / x * tuning
  
  return(trend)
}


# old Kernel -> 3 new parts -------------------------------------
adjusted_rt2proj_rt = function(adjusted_rt, daily_rt, DATE, 
                               days.lookBackMmt=105, days.excludeMmt=21){
  # !!! direct copy! needs revision!!! ####
  needs(dplyr)
  
  excluded = daily_rt %>%
    filter(Date <= DATE) %>%
    top_n(as.numeric(days.excludeMmt), Date) %>%
    arrange(Date)
  excluded = excluded$Date[1]
  moment_period.r = daily_rt %>%
    filter(Date < excluded) %>%
    top_n(as.numeric(days.lookBackMmt), Date)
  
  momentum_period = moment_period.r %>% 
    select(-Date) %>% 
    as.data.frame()
  
  momentum2 = apply(1+momentum_period/100, 2, prod)
  momentum2 = (momentum2-1)*as.numeric(input$scale)
  
  adjusted_rt.at.DATE = adjusted_rt %>% 
    filter(Date == DATE) %>%
    select(-Date) %>%
    as.numeric()
  a = as.matrix(adjusted_rt.at.DATE) + momentum2/2
  
  return(a)
}

toDictJson = function(x, key, value){
  sd = x[,value] %>%
    t() %>%
    as.data.frame()%>%
    `colnames<-`(x[,key]) %>%
    toJSON()
  
  sd <- gsub(pattern = '^\\[', replacement = "", x = sd)
  sd <- gsub(pattern = '\\]$', replacement = "", x = sd)
  
  return(sd)
}

cov.period = function(DATE = NA, days.lookBackSD = 504, daily_rt) {
  # format check
  if(days.lookBackSD > nrow(daily_rt)){
    warning("no enough daily return data, might cause problems.")
  }
  
  # select corresponding range from daily return to prepare for calculating covariance matrix
  # as 'cov_period(DATE, daily_rt)'
  if(!is.na(DATE)){
    if(daily_rt[1,1] > DATE){
      warning("DATE < the most current date in dataset. Might cause problems.")
    }
    
    cov_period.r = daily_rt %>%
      filter(Date <= DATE) %>%
      top_n(as.numeric(days.lookBackSD), Date)
    
    cov_period = cov_period.r %>% 
      select(-Date) %>% 
      na.omit() %>%
      as.data.frame() %>%
      as.matrix()
  } else {
    cov_period.r = daily_rt %>%
      top_n(as.numeric(days.lookBackSD), Date)
    
    cov_period = cov_period.r %>% 
      select(-Date) %>% 
      na.omit() %>%
      as.data.frame() %>%
      as.matrix()
  }
  
  return(cov_period)
}

cov.matrix2 = function(cov_period, SR){
  # number of assets (tickers)
  n = ncol(cov_period)
  
  # calculate scaling ratio as 'SR(DATE)'
  if(!TRUE){ 
    cov_matrix2 = cov(cov_period) * 1^2
  } # temporaryly using a constant
  
  # calculate a special covariance matrix as 'cov_matrix2(cov_period, SR)'
  if(TRUE){ 
    cov_matrix2 = cov(cov_period) * SR^2
  }
  
  return(cov_matrix2)
}

cov2.addMMT.ratio = function(cov2, MMT_ratio, switch_ = TRUE){
  if(switch_){
    diag(cov2) = MMT_ratio * diag(cov2)
  }
  
  return(cov2)
}

opti.wets.donlp2 = function(SD_1year.max=3.5, rt_1quater.min=-3, proj_rt, cov_matrix2, 
                            stress_rt, stress_dw=-15, 
                            init.weight = rep(1/nrow(cov_matrix2),nrow(cov_matrix2))){
  # This function has already had nothing to do with the date.
  # This is the inner most function.
  # SD_1year.max       # positive decimal number eg. 3.5
  # rt_1quater.min     # negative decimal number eg. -3
  # proj_rt            # a horizontal 1×(n+1) tibble, n equals to number of assets, 1 row for Date
  # cov_matrix2        # n×n labeled matrix filled with small decimal numbers, labels are asset tickers
  
  tickers = names(proj_rt[ ,-1])
  
  proj_rt = proj_rt[ ,-1] %>% 
    as.matrix %>%
    t() 
  
  needs(Rdonlp2)
  # Convert SD and return to day format
  SD_1day.max = SD.year2day(percent(SD_1year.max))       # originally year
  rt_1year.min = rt.quater2year(percent(rt_1quater.min))  # originally quater
  
  # *[n] Number of assets (tickers)
  n = ncol(cov_matrix2)
  
  # [fn] Objective function to be minimized
  fn=function(par){-par%*%proj_rt}
  
  #________________________________________________________________________________________
  # [par] Initial value
  P = init.weight
  
  # [par.upper & par.lower] -> par
  par.u = rep(0.3,n)
  par.l = rep(0,n)
  
  # [A] the matrix object that represents linear constraints
  A=rbind(t(c(rep(1,n))), t(proj_rt), t(stress_rt))
  
  # [lin.upper $ lin.lower] -> A
  lin.u=c(1, +Inf, +Inf)
  lin.l=c(1, rt_1year.min, stress_dw)
  
  
  # [nlin] list of functions that represents nonlinear constraints
  cov_cons = function(par){t(par)%*%cov_matrix2%*%par}
  nlin = list(cov_cons)
  
  # [nlin.upper & nlin.lower] -> nlin
  nlin.u=c(SD_1day.max)
  nlin.l=c(0)
  
  # Check criteria
  if(!TRUE){
    cat("------------------Check variables------------------\n")
    message("par"); message(P)
    #message("fn"); message(fn)
    message("par.upper"); message(par.u)
    message("par.lower"); message(par.l); cat("\n")
    
    message("A"); message(A)
    message("lin.upper"); message(lin.u)
    message("lin.lower"); message(lin.l)
    
    
  }
  
  #_____________________________________________________________________
  # donlp2 core
  bug("ready for donlp2 (internal) opti.wets.donlp2")
  test = list(P, fn, par.u, par.l, A, lin.u, lin.l, nlin, nlin.u, nlin.l)
  bug(toJSON(test))
  result = donlp2(par = P, 
                  fn,
                  par.upper = par.u,
                  par.lower = par.l,
                  
                  A,
                  lin.upper = lin.u,
                  lin.lower = lin.l,
                  
                  nlin,
                  nlin.upper = nlin.u,
                  nlin.lower = nlin.l,
                  
                  name = NULL)
  
  end_result = result$par
  names(end_result) = tickers
  return(end_result)
}

# calcualte optimum weights -------------------------------------
opti.wets = function(daily_rt, forward_rt, DATE, 
                     days.lookBackSD=504, days.lookBackMmt=105, days.excludeMmt=21, 
                     SD_1year.max=3.5, rt_1quater.min=-3,
                     forward_rt_type = "projected",
                     stress_rt, stress_drawdown){
  
  # convert the type of forward return: YTM -> adjusted -> projected ====
  if(forward_rt_type == "YTM"){
    proj_rt = forward_rt %>% YTM2adjust_rt() %>% adjusted_rt2proj_rt()
  } else if (forward_rt_type == "adjusted") {
    proj_rt = forward_rt %>% 
      adjusted_rt2proj_rt(daily_rt = daily_rt, 
                          DATE = DATE, 
                          days.lookBackMmt = days.lookBackMmt, 
                          days.excludeMmt = days.excludeMmt)
  } else if (forward_rt_type == "projected") {
    proj_rt = forward_rt
  } else {stop("The type of forward return must be one from the following: YTM, adjusted, projected")}
  
  # main ====
  cov_period = cov.period(DATE, days.lookBackSD, daily_rt)
  
  SR_today = GET.value.lookup(table = "scaling", 
                              col2lookup = "Scaling_Ratio", 
                              col.name = "Date", 
                              col.value = DATE)
  
  cov2 = cov.matrix2(cov_period, SR_today)
  #cov2 = cov(cov_period)
  bug("ready for donlp2...")
  result = opti.wets.donlp2(SD_1year.max, rt_1quater.min, proj_rt, cov2, stress_rt, stress_drawdown)
  
  # END of opti.wets ====
  return(result)
}

# related portfolio return and standard deviation ====
portfolio.SD = function(weights, cov_matrix2){
  t(weights)%*%cov_matrix2%*%weights
}

portfolio.rt = function(weights, rt_){
  if("Date" %in% names(rt_)){rt_ = rt_ %>% select(-Date)}
  as.matrix(rt_) %*% as.matrix(weights) %>% as.numeric()
}

# calculate efficient frontier ----------------------------------
opti.wets.efficient_frontier = function(daily_rt, forward_rt, SR_today,
                                        days.lookBackSD =504,
                                        rt_1quater.min =-3,
                                        SD.test_point = seq(2.5, 8, 0.1),
                                        forward_rt_type = "projected",
                                        stress_rt, stress_drawdown, rt_toShow){
  
  gather.weight.change = TRUE
  # main ====
  cov_period = cov.period(DATE = NA, days.lookBackSD, daily_rt)
  
  cov2 = cov.matrix2(cov_period, SR_today)
  
  # efficient frontier
  weight = rep(1/nrow(cov2),nrow(cov2))
  
  frontier.notebook = data.frame(SD = SD.test_point, Return = NA)
  
  SD.w.portfolio = data.frame(SD = SD.test_point, weights = NA) %>%
    tbl_df() %>%
    mutate(weights = rep(list(NULL), length(SD.test_point)))
  
  for(i in 1:length(SD.test_point)){
    weight = opti.wets.donlp2(SD.test_point[i], rt_1quater.min, forward_rt, cov2, 
                              stress_rt, stress_drawdown, weight)
    portfolio_rt = portfolio.rt(weight, rt_toShow)
    frontier.notebook$Return[i] = portfolio_rt
    
    if(gather.weight.change){
      SD.w.portfolio$weights[[i]] = weight
    }
    if(!exists("input", where = globalenv())){
      cat('\r', SD.test_point[i] %>% format(nsmall = 1))
      flush.console()
    }
  }
  #SD.w.portfolio <<- SD.w.portfolio
  if(gather.weight.change == F){
    SD.weight <- SD.w.portfolio %>%
      mutate(weights = lapply(weights, as.data.frame)) %>%
      mutate(weights = lapply(weights, t)) %>%
      mutate(weights = lapply(weights, as.data.frame))
    
    weight_stack = hisw.stack(SD.weight, by = "SD") %>%
      gather(key = "asset", value = "weight", -Date)
    
    needs(grid) # for unit()
    cols <- colorRampPalette(brewer.pal(12, "Set3"))
    myPal <- cols(length(unique(weight_stack$asset)))
    
    ggplot(weight_stack, aes(x = Date, y = weight)) + 
      geom_area(aes(fill = asset), position = "stack") +
      xlab("SD") +
      scale_fill_manual(values = myPal) +
      theme(legend.position="bottom",legend.direction = "horizontal")
  }
  
  # END of opti.wets ====
  return(frontier.notebook)
}

# rebalancing functions -----------------------------------------
global.bounds = function(SD, limit){
  lbound <<- SD * (1 - limit)
  ubound <<- SD * (1 + limit)
}

notebook.declare = function(daily_rt){
  # declaration of a chronological notebook based on date from daily return
  notebook.init = daily_rt %>%
    select(Date) %>%
    arrange(Date) %>%
    tbl_df() %>%
    mutate(Date = as.Date(Date))
  
  rows = nrow(daily_rt)
  
  note = notebook.init %>%
    mutate(weights = rep(list(NULL), rows),
           cov_matrix2 = rep(list(NULL), rows),
           #daily_rt = rep(list(NULL), rows),
           actual_rt = NA,
           cum_rt = NA,
           SD = NA,
           rt = NA,
           status = NA)
  
  return(note)
}

compound = function(notebook, thisDay, cov2, returns.thisDay, SR_thisDay){
  #' compare SD of the compounded with sd from yesterday
  # based on the continuous fraction share assumption
  returns.thisDay = returns.thisDay[,-1]
  thisRow = which(notebook$Date == thisDay)
  yesterRow = thisRow - 1
  yesterDay = notebook$Date[yesterRow] #%>% as.Date()
  # h_period = GET.rt.dateRange(yesterDay, thisDay, "daily_return") %>%
  #   select(-Date)
  h_period = daily_rts %>%
    filter(Date == thisDay | Date == yesterDay) %>%
    select(-Date)
  
  weight.yesterDay = notebook$weights[[yesterRow]]
  # SR_thisDay = GET.value.lookup(table = "scaling", 
  #                             col2lookup = "Scaling_Ratio", 
  #                             col.name = "Date", 
  #                             col.value = thisDay)
  #cov2 = cov.matrix2(cov_period.thisDay, SR_thisDay)
  
  # quick and dirty. Copy and paste.
  ## calculation
  if(TRUE){
    cm_rate = apply(h_period+1, 2, cumprod)
    cm_rate = cm_rate[nrow(cm_rate),]
    weights.thisDay=(weight.yesterDay*cm_rate)/sum(weight.yesterDay*cm_rate)
    SD.thisDay = sqrt(weights.thisDay %*% cov2 %*% weights.thisDay) * sqrt(252)
    if(is.na(SD.thisDay)){
      message("[NaN]://FUN.tool.r$compound")
    }
    rt.thisDay = sum(weights.thisDay * returns.thisDay)
    actual_rt = sum(weights * notebook$returns[[thisRow]])
    cum_rt = (1 + actual_rt) * notebook$cum_rt[yesterRow]
    
  }
  
  # Save result to renew the notebook
  if(TRUE){
    notebook$weights[[thisRow]] = weights.thisDay
    notebook$SD[thisRow] = SD.thisDay
    notebook$rt[thisRow] = rt.thisDay
    notebook$cov_matrix2[[thisRow]] = cov2
    notebook$actual_rt[thisRow] = actual_rt
    notebook$cum_rt[thisRow] = cum_rt
    
  }
  notebook$status[thisRow] = "compound"
  return(notebook)
}

check.if.rebalance = function(SD_thisDay, lowerBound, upperBound){
  SD_thisDay = as.numeric(SD_thisDay)
  lowerBound = as.numeric(lowerBound)
  upperBound = as.numeric(upperBound)
  SD_thisDay >= upperBound | SD_thisDay <= lowerBound
}

# interpret result ======================
hisw.detail = function(history_weight, n){
  aa = history_weight[n, 2][[1]][[1]] %>% round(digit = 4)
  bb = aa[which(aa != 0)] %>% t()
  colnames(bb) = history_weight$Date[n] %>% as.character.Date()
  bb
}

hisw.stack = function(history_weight, by = 'Date'){
  nn = nrow(history_weight)
  foobar = data.frame(Date = history_weight[[by]][1], round(history_weight$weights[1][[1]], 4))
  for (i in 2:nn){
    foobar1 = data.frame(Date = history_weight[[by]][i], round(history_weight$weights[i][[1]], 4))
    foobar = rbind(foobar, foobar1)
  }
  row.names(foobar) = NULL
  foobar
}

gg.each_column = function(x, by){
  require(ggplot2)
  require(reshape2)
  require(RColorBrewer)
  d <- melt(x, id.vars=by)
  
  cols <- colorRampPalette(brewer.pal(12, "Set3"))
  myPal <- cols(dim(x)[2] - 1)
  
  # Everything on the same plot
  q = ggplot(d, aes_string(by, "value", col = "variable")) + 
    geom_line(size = 1, alpha = 0.7) +
    scale_color_manual(values = myPal)
  
  # Separate plots
  # ggplot(d, aes(Xax,value)) + 
  #   geom_point() + 
  #   stat_smooth() +
  #   facet_wrap(~variable)
  return(q)
}

gg.sd_rt = function(DATE, daily_rts, forward_rt){
  SDs = daily_rts  %>%
    arrange(Date) %>%
    mutate_at(vars(-Date), roll_apply, width = 504, FUN = var) %>%
    mutate_at(vars(-Date), SD.day2year) %>%
    arrange(desc(Date)) %>%
    filter(Date <= DATE) %>%
    top_n(1, Date) %>%
    select(-Date) %>% 
    unlist()
  
  RTs = forward_rts  %>%
    arrange(desc(Date)) %>%
    filter(Date < DATE) %>%
    top_n(1, Date) %>%
    select(-Date) %>% 
    unlist()
  
  tickers = names(SDs)
  
  fooo = data.frame(ticker = tickers, SD = SDs, RT = RTs)
  
  ggplot(fooo, aes(SD, RT, label = ticker)) +
    geom_text() +
    geom_vline(xintercept = 0.035)
  
}

gg.mmtTest = function(aa, moment.days = 105, exclu.days = 21, FUN = sum){
  require(tidyr)
  # aa should be a two-col dataframe (from daily_rt) with one column be Date
  # function used came from source FUN.rountine.r
  
  ticker = colnames(aa)[colnames(aa) != "Date"][1]
  qwe = aa %>%
    arrange(Date) %>%
    mutate_(Cumulative = paste("cumprod(",ticker," + 1)")) %>%
    mutate_(Momentum = paste("roll_apply_(",ticker,", width = moment.days, interval = exclu.days, FUN=FUN)")) %>%
    mutate(positiveMMT = Momentum > 0)
  
  k = gather(qwe,key = "key", value = "value", Cumulative, Momentum, na.rm = T)
  
  ggplot(k, aes(x = Date, y = value, group = key)) +
    geom_point(aes(col = positiveMMT)) + 
    facet_grid(key~.,scales = "free") +
    ylab(ticker)
}


gg.cum_rt.bench <- function(notebook, first_day, last_day) {
  needs(ggplot2, tidyr)
  daily_rt.eql_w = connect.and.query(paste0("SELECT * FROM daily_return")) %>%
    mutate(Date = as.Date.character(Date)) %>%
    mutate_at(vars(-Date), percent) %>%
    arrange(Date) %>%
    filter(Date >= first_day & Date <= last_day)%>%
    mutate_at(vars(-Date), `+`, 1) %>%
    mutate_at(vars(-Date), cumprod)
  
  avg.rt = select(daily_rt.eql_w, -Date) %>%
    apply(1, mean)
  
  avg.rt.df = bind_cols(Date = daily_rt.eql_w$Date, Return = avg.rt) %>%
    filter(Date >= first_day & Date <= last_day)
  
  benchmark = GET.rt.dateRange(first_day, last_day, 
                               "barclays_daily_price,vanguard_daily_price", 
                               "benchmarks") %>%
    mutate(Date = as.Date.character(Date)) %>%
    arrange(Date) %>%
    mutate(barclays_daily_price = barclays_daily_price/barclays_daily_price[1],
           vanguard_daily_price = vanguard_daily_price/vanguard_daily_price[1])
  #test <<- inner_join(notebook, benchmark,by = "Date")
  ggplot(inner_join(notebook, 
                    benchmark,by = "Date"), aes(x = Date, y = cum_rt)) +
    geom_line(stat = "identity")+
    geom_line(aes(Date, barclays_daily_price), color = "blue", alpha = 0.3) +
    geom_line(aes(Date, vanguard_daily_price), color = "orange", alpha = 0.5) +
    geom_line(data = avg.rt.df, aes(Date, Return), color = "grey", alpha = 0.7) +
    geom_point(data = notebook %>% filter(status == "rebalance"), color = "red") +
    scale_x_date(labels = date_format("%Y-%m")) +
    labs(title = "Grey:equally distributed\nblue:barclays\norange:vanguard")
}

gg.compare = function(A, B, compare_column, by_column = "Date", 
                      name_A = deparse(substitute(A)), name_B = deparse(substitute(B))){
  A = A %>%
    select_(by_column, compare_column) %>%
    mutate(dataFrame = name_A)
  B = B %>%
    select_(by_column, compare_column)%>%
    mutate(dataFrame = name_B)
  
  gg_ = bind_rows(A, B)
  
  ggplot(gg_, aes_string(by_column, compare_column, color = "dataFrame")) +
    geom_line()
  
}

# debug =================
bug = function(x, sources = ''){
  if(exists("whether.debug", globalenv())){
    if(globalenv()$whether.debug){
      needs(RMySQL)
      con <- dbConnect(MySQL(),
                       user="treesrobo", password="treesrobo",
                       dbname="treesrobo", host="treesrobo-db.crw1otiz3w41.us-east-1.rds.amazonaws.com")
      ti =suppressWarnings(format(Sys.time(), tz="EST"))
      table2upload = data.frame(timestamp = ti,
                                status = x,
                                source = sources)
      dbWriteTable(con, "debug", table2upload, append = TRUE, row.names = FALSE)
      dbDisconnect(con)
    }
  }
}


# END of script -------------------------------------------------s = connect.and.query("TRUNCATE TABLE debug", type = "update")
bug("start of main.change_preference_today.r")
bug("making sure debug function itself is not a problem...")


data.asset_pool = GET.column("asset_pool", column = "asset, category, credit, maturity")


slec.tickers = applyPreference(data.asset_pool, excludeSheet, riskTolerance, investmentHorizon)$asset %>%
  paste0(collapse = ",")

forward_rt = GET.latest_nrow.by(tickers = slec.tickers, 
                                table = "proj_return",
                                nrows = 1,
                                by = "Date",
                                delim = ",") %>%
  mutate_at(vars(-Date), percent)

if(TRUE){
  rt_toShow = GET.latest_nrow.by(tickers = slec.tickers, 
                                 table = "adjusted_rt",
                                 nrows = 1,
                                 by = "Date",
                                 delim = ",") %>%
    mutate_at(vars(-Date), percent)
}

daily_rt = GET.latest_nrow.by(tickers = slec.tickers, 
                              table = "daily_return",
                              nrows = 504,
                              by = "Date",
                              delim = ",") %>%
  mutate_at(vars(-Date), percent)

DATE = daily_rt$Date[1]

stress_rt = GET.column(table = "stressful_period_rt", # 08 constraint
                       column = slec.tickers) %>%
  as.numeric()

bug("data readin...")
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
                    stress_drawdown) 

bug("weight calculated.")

# Portfolio check
SR_portflio_DATE = GET.value.lookup(table = "scaling", 
                                    col2lookup = "Scaling_Ratio", 
                                    col.name = "Date", 
                                    col.value = DATE)

SR_portflio_DATE = ifelse(is.na(SR_portflio_DATE), 1,SR_portflio_DATE)
# SR_portflio_DATE = 1.3
cov_matrix2 = cov.matrix2(cov.period(DATE, daily_rt = daily_rt), SR_portflio_DATE)
portfolio_SD = weights%*%cov_matrix2%*%weights %>% SD.day2year()
bug("cov_matrix2 calaculated.")
# efficient frontier
dia = diag(cov_matrix2) %>% SD.day2year() %>% sort() %>% `*`(100)
start.SD = mean(dia[1:3]) %>% ceiling()
end.SD = median(rev(dia)[1:5])

bug("ploting efficient frontier...")
efficient_frontier = opti.wets.efficient_frontier(daily_rt, forward_rt, SR_portflio_DATE,
                                                  days.lookBackSD=504,
                                                  rt_1quater.min=-3,
                                                  SD.test_point = seq(start.SD, end.SD, 0.1),
                                                  forward_rt_type = "projected",
                                                  stress_rt, stress_drawdown, rt_toShow)

# showing the result ----------------------------------------------------------

bug("ploted.")
if(exists("input")){
  # UPLOAD weights=====================================
  money = GET.value(table = "profile", column = "totalAsset", ID = ID)
  # money = totalAsset
  
  wets = data.frame(names = names(weights),
                    Weight = round(weights, 4))
  d = wets %>%
    select(names, Weight) %>%
    filter(Weight != 0)
  
  d = d %>%
    mutate(name0 = paste0("\'", names, "\'")) %>%
    mutate(price = GET.value.2dim("asset_pool", "unitPrice", "asset", name0),
           shares = money * Weight / price) %>%
    mutate(upload = shares)
  
  try(UPDATE.row.exclude(d,"cur_asset_dist","id|username", ID = ID))
  try(UPDATE.row.exclude(d,"alloc_asset_dist","id|username", ID = ID))
  
  # UPLOAD sfficient frontier==========================
  ff = efficient_frontier  %>%
    mutate(Return = Return * 100) %>%
    rename(ax = SD, ay = Return) %>%
    toJSON()
  ef = paste0("UPDATE allocation_overview SET efficientFrontierCurveDataset = \'",
              ff, "\', projectedSD = ", portfolio.SD(weights, cov_matrix2) %>% SD.day2year(), 
              ", projectedReturn = ", portfolio.rt(weights, rt_toShow), " WHERE id = ",ID)
  connect.and.query(ef,"update")
  
  bug('things been updated.')
} else {
  names(weights) = names(daily_rt[-1])
  weights = (weights %>% round(digits = 4))
  barplot(weights)
  
  #plot(efficient_frontier, type='l')
  #text(SD_individual,rt_individual, names(rt_individual))
  
  SD_individual = diag(cov_matrix2) %>% 
    SD.day2year() %>% 
    `*`(100) 
  
  rt_individual = rt_toShow[-1] 
  
  foobar1 = bind_rows(SD_individual, rt_individual) %>% 
    t() %>%
    as.data.frame() %>%
    mutate(name = rownames(.))
  colnames(foobar1) = c("SD", "rt", "name")
  ggplot(efficient_frontier, aes(SD, Return)) +
    geom_line() +
    geom_point(data = foobar1, aes(x = SD,y = rt)) +
    geom_text(data = foobar1, aes(x = SD,y = rt, label = name, hjust = -0.2))
}


