source("FUN.tool.r")
animate = FALSE
whether.debug = FALSE

connect.and.query("TRUNCATE TABLE debug", type = "update")
bug("start of main.rebalance.r")
bug("making sure debug function itself is not a problem...")
# MMT.added_to = "SD" #"TYM" is temporarily offline
# get data -------------------------------------------------
if(exists("input")){
  data = fromJSON(toJSON(input))
  
  ID = as.integer(data$id)
  
  first_day = data$startDate %>% as.Date.character()
  last_day = data$endDate %>% as.Date.character()
  
  riskTolerance = data$riskLevel %>% as.character()
  investmentHorizon = data$investmentHorizon %>% as.character()
  
  trigger_limit = data$rebalancingToleranceTrigger %>% as.numeric() %>% percent()
  
  whether.MMT = data$enableMomentumFactor == '1'
  momentumTuningFactor = data$momentumTuningFactor %>% as.numeric()
  
  days.lookBackSD = 504
  rt_1quater.min = -3
  
  stress_drawdown = data$maxQuarterlyDrawdown_GFC %>% as.numeric()
  try(saveRDS(input, "input.RDS"))
  
} else {
  needs(RColorBrewer, ggplot2, plotly, scales)
  
  first_day = "2013-07-08" %>% as.Date.character()
  last_day = "2017-07-08" %>% as.Date.character()
  
  # first_day = "2014-07-08" %>% as.Date.character()
  # last_day = "2016-02-08" %>% as.Date.character()
  trigger_limit = 15 %>% percent() %>% as.numeric()
  
  whether.MMT = TRUE
  momentumTuningFactor = 48
  
  #"Low", "Medium", "High"
  riskTolerance="Medium"
  
  # "Short", "Intermediate", "Long"
  investmentHorizon="Intermediate"
  
  days.lookBackSD = 504
  rt_1quater.min = -3
  
  stress_drawdown = -15
}


SD_1year.max = if(riskTolerance == "Low") {
  2.5
} else if(riskTolerance == "Medium"){
  3.5
} else if(riskTolerance == "High"){
  4.5
} else {stop("invalid riskTolerance")}
bug("input has been taken in")

# GET universal data --------
data.asset_pool = GET.column("asset_pool", column = "asset, category, credit, maturity")

slec.tickers = applyPreference(data.asset_pool, excludeSheet, riskTolerance, investmentHorizon)$asset %>%
  #check.valid.tickers(first_day, last_day, days.lookBackSD) %>%
  paste0(collapse = ",")

stress_rt = GET.column(table = "stressful_period_rt", # 08 constraint
                       column = slec.tickers) %>%
  as.numeric()

## Implied data
global.bounds(percent(SD_1year.max), trigger_limit)
# lbound = percent(SD_1year.max) * (1 - trigger_limit)
# ubound = percent(SD_1year.max) * (1 + trigger_limit)

# ---------------------------------------------------------------
#' doing rebalance

notebook = notebook.declare(GET.rt.dateRange(first_day, last_day))
actual.first_day = notebook$Date[1] #%>% as.Date()
actual.last_day = notebook$Date[nrow(notebook)] #%>% as.Date()

# format check
if(first_day <= actual.first_day & actual.first_day < actual.last_day & actual.last_day <= last_day){
  #message("Date check ok...")
} else {
  stop("Date check failed...")
}

bug("data readin and format check...")
# rebalancing ---------------------------------------------------
if(!exists("input")){
  pb = txtProgressBar(as.numeric(first_day), as.numeric(last_day), initial = as.numeric(first_day), style = 3)
  if(isTRUE(animate)){
    x11()
  }
}

bug("start of for loop...")
for(thisDay in notebook$Date){
  thisDay = thisDay %>% as.Date.n()
  SRthisDay = GET.value.lookup(table = "scaling",
                               col2lookup = "Scaling_Ratio", 
                               col.name = "Date", 
                               col.value = thisDay)
  SRthisDay = ifelse(is.na(SRthisDay), 1, SRthisDay)
  if(!exists("input")){
    setTxtProgressBar(pb, as.numeric(thisDay))
  }
  
  if(thisDay == actual.first_day){ #****First day ==========
    bug(thisDay)
    # First day of the ranged days
    # forward rt ===============
    forward_rts = GET.rt.dateRange(actual.first_day - 10, actual.last_day, 
                                   tickers = slec.tickers,
                                   table = "adjusted_rt") %>%
                                   #table = "proj_return") %>%
      #table = ifelse(whether.MMT, "proj_return", "adjusted_rt")) %>%
      mutate(Date = as.Date.character(Date)) %>%
      filter(!is.na(Date)) %>%
      replace(is.na(.), 0)
    if(actual.first_day - first_day > 7){
      stop("Too much gap (> 7 days difference) between indicated first day and actual first day.\nDetails at the beginning of the rebalacing part.")
    }
    
    days_span = nrow(forward_rts)
    
    # daily rt ====================
    daily_rts = GET.rt(tickers = slec.tickers, 
                       DATE = actual.last_day, 
                       table = "daily_return",
                       days.all = days_span + days.lookBackSD + 1,
                       delim = ",", percent_ = TRUE) %>% 
      mutate(Date = as.Date.character(Date))
    
    # MMT:load MMT_ratios-------

    if(whether.MMT){
      needs(zoo, tidyr)
      MMT_ratios = daily_rts %>%
        mutate_at(vars(-Date), `*`, 100) %>%
        arrange(Date) %>%
        replace(is.na(.), 0) %>%
        mutate_at(vars(-Date), MMT.ratio.hedgeLog, tuning = momentumTuningFactor) %>%
        arrange(desc(Date))
    }
    
    # daily return volume check
    if(nrow(daily_rts) < days_span + days.lookBackSD + 1){
      warning("less data rows than expected!!!")
      cat(paste("date range of the target dataset is from", min(daily_rts$Date), "to", max(daily_rts$Date), "\n"))
      cat(paste("We are supposed to have", days_span + days.lookBackSD + 1, "rows of data.\n"))
      cat(paste("But we currently have only", nrow(daily_rts), "rows data."))
    }
    
    # ()===============================================
    forward_rt = forward_rts %>%
      arrange(Date) %>%
      filter(Date < thisDay) %>%
      top_n(1, wt = Date)
    
    daily_rt = daily_rts %>%
      filter(Date <= thisDay) %>%
      top_n(days.lookBackSD, wt = Date)
    # 
    
    cov_period = cov.period(thisDay, days.lookBackSD, daily_rt)
    
    notebook = left_join(notebook %>% select(Date), daily_rts, by = "Date") %>%
      shorten.rts(exclude = "Date") %>%
      right_join(notebook, by = "Date")
    
    thisRow = which(notebook$Date == thisDay)
    #Caculation
    if(whether.MMT){
      MMT_ratio.r = MMT_ratios %>%
        filter(Date <= thisDay) %>%
        top_n(1, Date)
      MMT_ratio = MMT_ratio.r %>%
        select(-Date) %>%
        unlist() %>%
        `*`(-1) %>% 
        sigmoid() %>%
        `+`(0.5)
    }
    cov_matrix2 = cov.matrix2(cov_period, SRthisDay) %>%
      cov2.addMMT.ratio(MMT_ratio, whether.MMT)
    
    if(TRUE){
      weights = opti.wets.donlp2(SD_1year.max, rt_1quater.min, forward_rt, cov_matrix2,stress_rt, stress_drawdown) 
      portfolio_SD = portfolio.SD(weights, cov_matrix2)
      portfolio_rt = portfolio.rt(weights, forward_rt)
      actual_rt = sum(weights * notebook$returns[[thisRow]])
      cum_rt = 1 + actual_rt
    }
    
    # save result into notebook
    if(TRUE){
      notebook$cov_matrix2[[thisRow]] = cov_matrix2
      notebook$weights[[thisRow]] = weights 
      notebook$SD[thisRow] = portfolio_SD %>% SD.day2year()
      notebook$rt[thisRow] = portfolio_rt
      notebook$actual_rt[thisRow] = actual_rt
      notebook$cum_rt[thisRow] = cum_rt
    }
    notebook$status[thisRow] = "firstSet"
    global.bounds(portfolio_SD %>% SD.day2year(), trigger_limit)
    
  } else {#****Second+ day ==========
    # start from the second day of the ranged days
    thisRow = which(notebook$Date == thisDay)
    yesterRow = thisRow - 1
    
    forward_rt = forward_rts %>%
      arrange(Date) %>% 
      filter(Date < thisDay) %>%
      top_n(1, wt = Date)
    
    
    daily_rt = daily_rts %>%
      filter(Date <= thisDay) %>%
      top_n(days.lookBackSD, wt = Date)
    
    cov_period = cov.period(thisDay, days.lookBackSD, daily_rt)
    if(whether.MMT){
      MMT_ratio.r = MMT_ratios %>%
        filter(Date <= thisDay) %>%
        top_n(1, Date)
      MMT_ratio = MMT_ratio.r %>%
        select(-Date) %>%
        unlist() %>%
        `*`(-1) %>% 
        sigmoid() %>%
        `+`(0.5)
    }
    cov_matrix2 = cov.matrix2(cov_period, SRthisDay) %>%
      cov2.addMMT.ratio(MMT_ratio, whether.MMT)
    
    notebook = compound(notebook, thisDay, cov_matrix2, forward_rt, SRthisDay)
    
    if(check.if.rebalance(notebook$SD[thisRow], lbound, ubound)){
      #Caculation
      
      
      if(TRUE){
        weights = opti.wets.donlp2(SD_1year.max, rt_1quater.min, forward_rt, cov_matrix2, stress_rt, stress_drawdown) 
        portfolio_SD = portfolio.SD(weights, cov_matrix2)
        portfolio_rt = portfolio.rt(weights, forward_rt)
        actual_rt = sum(weights * notebook$returns[[thisRow]])
        cum_rt = (1 + actual_rt) * notebook$cum_rt[yesterRow]
        
      }
      
      # save result into notebook
      if(TRUE){
        notebook$cov_matrix2[[thisRow]] = cov_matrix2
        notebook$weights[[thisRow]] = weights 
        notebook$SD[thisRow] = portfolio_SD %>% SD.day2year()
        notebook$rt[thisRow] = portfolio_rt
        notebook$actual_rt[thisRow] = actual_rt
        notebook$cum_rt[thisRow] = cum_rt
        
      }
      notebook$status[thisRow] = "rebalance"
      global.bounds(portfolio_SD %>% SD.day2year(), trigger_limit)
      #cat("rebalanced\n")
      
    }
    
  }
  
  if(!exists("input") & animate){
    #plot(notebook$Date, notebook$cum_rt, type = 'l')
    barplot(weights)
  }
}

if(!exists("input")){
  close(pb)
  
  if(isTRUE(animate)){
    dev.off()
  }
  
}

bug("calculation finished")
if(exists("input")){
  # update to db-------------------------------
  if(TRUE){
    cum.rt = notebook$cum_rt %>%
      t() %>%
      as.data.frame()%>%
      `colnames<-`(notebook$Date) %>%
      toJSON()
    
    
    cum.rt <- gsub(pattern = '^\\[', replacement = "", x = cum.rt)
    cum.rt <- gsub(pattern = '\\]$', replacement = "", x = cum.rt)
  }
  
  connect.and.query(paste0("UPDATE simulation SET cum_rt = \'",cum.rt,"\' WHERE id = ",ID),"update")
  bug("cum_rt updated.")
  
  # sd <- notebook$SD %>%
  #   as.numeric() %>%
  #   `names<-`(notebook$Date) %>%
  #   RJSONIO::toJSON()
  if(TRUE){
    SD = notebook$SD %>%
      t() %>%
      as.data.frame()%>%
      `colnames<-`(notebook$Date) %>%
      toJSON()
    
    
    SD <- gsub(pattern = '^\\[', replacement = "", x = SD)
    SD <- gsub(pattern = '\\]$', replacement = "", x = SD)
  }
  
  connect.and.query(paste0("UPDATE simulation SET sd = \'",SD,"\' WHERE id = ",ID),"update")
  bug("SD updated.")
  
  if(TRUE){
    history.weight <- notebook %>%
      filter(status %in% c("firstSet", "rebalance")) %>%
      select(Date, weights) %>%
      mutate(weights = lapply(weights, as.data.frame)) %>%
      mutate(weights = lapply(weights, t)) %>%
      mutate(weights = lapply(weights, as.data.frame))
    
    ww.json = history.weight$weights %>%
      lapply(`rownames<-`, NULL) %>%
      `names<-`(history.weight$Date) %>%
      toJSON()
  }
  
  connect.and.query(paste0("UPDATE simulation SET history_weight = \'",ww.json,"\' WHERE id = ",ID),"update")
  bug("history weight updated.")
} 
  # Showing result -----------------------------------------------------
  
  # "PDF" of respective assets time series
if(!exists("input")){
    gg.cum_rt.bench(notebook = notebook, first_day = first_day, last_day = last_day)
  }
  
  # Portfolio standard deviation time series
if(!exists("input")){
    ggplot(notebook, aes(x = Date, y = SD)) +
      geom_line(stat = "identity")+
      geom_hline(yintercept = percent(SD_1year.max), lty = 2) +
      geom_point(data = notebook %>% filter(status == "rebalance"), color = "red") +
      scale_x_date(labels = date_format("%Y-%m"))
  }
  
if(!exists("input")){
    history.weight <- notebook %>%
        filter(status %in% c("firstSet", "rebalance")) %>%
        select(Date, weights) %>%
        mutate(weights = lapply(weights, as.data.frame)) %>%
        mutate(weights = lapply(weights, t)) %>%
        mutate(weights = lapply(weights, as.data.frame))

    weight_stack = hisw.stack(history.weight) %>%
      gather(key = "asset", value = "weight", -Date)
    
    needs(grid) # for unit()
    cols <- colorRampPalette(brewer.pal(9, "Reds"))
    myPal <- cols(length(unique(weight_stack$asset)))#[order(risklevel.MMT)]
    
    risklevel = diag(cov_matrix2)
    weight_stack$asset = factor(weight_stack$asset, 
                                levels = names(risklevel)[order(risklevel)])

    gggg = ggplot(weight_stack, aes(x = Date, y = weight)) + 
      geom_area(aes(fill = asset), position = "stack") +
      scale_fill_manual(values = myPal) +
      geom_vline(xintercept = as.numeric(weight_stack$Date)) #+
      #theme(legend.position="bottom",legend.direction = "horizontal")
    
    ggplotly(gggg)
  }

