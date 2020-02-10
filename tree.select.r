cat("\014")
needs(dplyr, tidyr, stringr, forecast, zoo, RMySQL, ggplot2, jsonlite)
# GETTER ------------------------------------------------------------------
id = 1
DATE = "2016-12-30"
riskTolerance = "Low"
investmentHorizon = "Short"

# define Function ---------------------------------------------------------
connect.and.query = function(query, type = "get"){
  con <- dbConnect(MySQL(),
                   user="treesrobo", password="treesrobo",
                   dbname="treesrobo", host="treesrobo-db.crw1otiz3w41.us-east-1.rds.amazonaws.com")
  
  if (type == "get"){
    rs = dbSendQuery(con, query)
    result = dbFetch(rs, -1)
  }
  
  if (type == "update") {
    dbSendQuery(con, query)
    result = "Updated!"
  }
  dbClearResult(dbListResults(con)[[1]])
  
  dbDisconnect(con)
  result
}

clean.charInNum = function(data, ignore = "Date|anyColumnHere"){
  # This function is incomplete.
  # It is supposed to find character value in numerical column and replace them.
  # Now it only gives a warning because it seems new data source do not have this problem.
  library(dplyr)
  check = data %>%
    select(-matches(ignore)) %>%
    summarise_each(funs = "class") %>%
    unlist() %>%
    unique()
  if(check != "numeric") warning("Found non-numeric value(s) in column(s) supposed to be numeric column. Please revise.")
  data
}

clean.spike = function(data){
  library(forecast)
  library(zoo)
  data = data %>%
    mutate_each(funs(tsclean(., replace.missing = FALSE)), -Date) %>%
    mutate_each(funs(na.approx(., na.rm = FALSE)), -Date)
  data
}

GET.value.2dim = function(table, column, row.title, row.value){
  #GET entry value AT the cross of column and row.title=row.value
  statement = paste0("SELECT ", column, " FROM ", table, " WHERE ", row.title, "=", row.value)
  connet.and.query(statement, type = "get")
}

GET.column = function(table, column){
  statement = paste0("SELECT ", column, " FROM ", table)
  connect.and.query(statement, type = "get")
}


# DATASET Import ----------------------------------------------------------
data.asset_pool = GET.column("asset_pool", column = "asset, category, credit, maturity")

# data.projReturn: calculated from daily YTM outside of R. 
#It is the projected return of alternative assets: look into the future.
data.projReturn = GET.column("proj_return", "*") %>%
  tbl_df() %>%
  clean.charInNum() %>%
  clean.spike()
# By far, only this dataset have had problems

# data.dlyReturn: daily return of alternative assets pulled from Bloomberg
data.dlyReturn = GET.column("daily_return", "*") %>%
  tbl_df()

# data.scale: scaling
data.scale = GET.column("scaling", "Date, Scaling_Ratio") %>%
  tbl_df

# Hard coded variable -----------------------------------------------------
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
#



# MAIN BODY: selection part -----------------------------------------------
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
excluded = applyPreference(data.asset_pool, excludeSheet, riskTolerance, investmentHorizon)
rm(applyPreference); print(excluded)

slec.tickers = excluded$asset %>%
  paste0(collapse = "|")
print(slec.tickers)

slec.projReturn <<- data.projReturn %>% 
  select(Date, matches(slec.tickers))

slec.dlyReturn <<- data.dlyReturn %>% 
  select(Date, matches(slec.tickers))


# CHECK POINT -------------------------------------------------------------
cat("slec.projReturn\t"); cat(slec.projReturn$Date[1]);cat("\n"); cat("slec.dlyReturn\t"); cat(slec.dlyReturn$Date[1])


# OUTPUT ------------------------------------------------------------------
rm(riskTolerance, investmentHorizon, excludeSheet, data.asset_pool, data.dlyReturn, data.projReturn, excluded)
# slec.projReturn
# slec.dlyReturn
