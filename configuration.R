needs(Rdonlp2, dplyr, tidyr, stringr, forecast, zoo, RMySQL, ggplot2, jsonlite)

# query function ---------------------------------------------------------
connect.and.query = function(query){
  con <- dbConnect(MySQL(),
                   user="treesrobo", password="treesrobo",
                   dbname="treesrobo", host="treesrobo-db.crw1otiz3w41.us-east-1.rds.amazonaws.com")
  
  rs = dbSendQuery(con, query)
  result = dbFetch(rs, -1)
  
  dbClearResult(dbListResults(con)[[1]])
  dbDisconnect(con)
  
  result
}

# clean functions -------------------------------------------------------

# clean.charInNum = function(data, ignore = "Date|anyColumnHere"){
#   # This function is incomplete.
#   # It is supposed to find character value in numerical column and replace them.
#   # Now it only gives a warning because it seems new data source do not have this problem.
#   library(dplyr)
#   check = data %>%
#     select(-matches(ignore)) %>%
#     summarise_all(funs = "class") %>%
#     unlist() %>%
#     unique()
#   if(check != "numeric") warning("Found non-numeric value(s) in column(s) supposed to be numeric column. Please revise.")
#   data
# }
# 
# clean.spike = function(data){
#   library(forecast)
#   library(zoo)
#   data = data %>%
#     mutate_at(funs(tsclean(., replace.missing = FALSE)), -Date) %>%
#     mutate_at(funs(na.approx(., na.rm = FALSE)), -Date)
#   data
# }


# exclustion table  ----------------------------------------------------
excludeSheet = array(data = '', dim = c(3,3,3), dimnames = list(c("Low", "Medium", "High"), #riskTolerance
                                                                c("Short", "Intermediate", "Long"), #investmentHorizon
                                                                c("category", "credit", "maturity"))) #column names used to exclude certain assets
    
excludeSheet[1,1,1]="REIT";   excludeSheet[1,1,2]="Sub_Investment_Grade";  excludeSheet[1,1,3]="Long|Ultral_Long"
excludeSheet[1,2,1]="REIT";   excludeSheet[1,2,2]="Sub_Investment_Grade"
excludeSheet[1,3,2]="Sub_Investment_Grade"
excludeSheet[2,1,3]="Long|Ultral_Long"
excludeSheet[3,1,3]="Long|Ultral_Long" # for multiple features, use "|" to seperate

apply.Preference = function(features, excludeSheet, riskTolerance="Low", investmentHorizon="Short"){
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
  
  selection = exclude(features, excludeSheet, riskTolerance, investmentHorizon)
  
  result = selection$asset 
  return(result)
}


# Optimize weights ---------------------------------------------------------

# memo
# rt: config, r_rt: proj_r, data = daily_r

optimized.weights <- function(daily_return,proj_return,SD){
  
  rt_c = -3  #downside risk by default
  rt = (1 + rt_c/100)^4 - 1

  P = rep(1/ncol(daily_return),ncol(daily_return)) #parameter

  fn=function(par){    #objective function
    -par%*%proj_return
  }
  
  par.l=rep(0,ncol(daily_return))
  par.u=rep(1,ncol(daily_return))
  
  lin.u=c(1,+Inf)
  lin.l=c(1,rt)
  
  A=rbind(rep(1,ncol(daily_return)),proj_return)

  nlin.u=c(SD)
  nlin.l=c(-Inf)
  
  weights = donlp2(P,fn,par.upper = par.u,          
                  par.lower = par.l,
                  A,
                  lin.upper = lin.u,
                  lin.lower = lin.l,
                  nlin = list(cov_cons),
                  nlin.upper = nlin.u,
                  nlin.lower = nlin.l,
                  name = 'simple')$par
  weights
}


# Efficient Frontier -------------------------------------------------------

efficient.frontier <- function(daily_return,proj_return,SR){
  
  f.SD.element = seq(1,14, by = 0.1) # set-ups
  
  frontier = data.frame(checkSD = NA, realSD = NA, rt = NA) 
  n = rep(NA, length(f.SD.element))
  frontier = data.frame(checkSD = n, realSD = n, rt = n)
  frontier$checkSD = f.SD.element
  
  proj_rt = proj_return %>%
    mutate_each(funs(./100))
   
  cov_matrix=cov(daily_return/100)
  ast_number=ncol(daily_return)
  SD_vector=matrix(ncol = 1,nrow = ast_number)
  for(i in 1:ast_number){
    SD_vector[i,] = c(sqrt(cov_matrix[i,i]))
  }
  cor_matrix=cor(daily_return)
  sc_ratio=c(rep(SR,ncol(data))) #set up the scaling ratio
  SD_vector=SD_vector*sc_ratio
  cov_matrix2=cor_matrix*(SD_vector%*%t(SD_vector)) #calculating the real cov matrix
  
  for (i in frontier$checkSD) {
    
    SD = ((i/100)/(sqrt(252)))^2
    weights = optimized.weights(daily_return,proj_return,SD) # optimizing weights
    
    portfolio_rt = round(sum(weights*proj_rt),4)
    portfolio_SD = c(sqrt(t(weights)%*%cov_matrix2%*%weights)*sqrt(252))
    
    frontier$realSD[which(frontier$checkSD == i)] = portfolio_SD * 100
    frontier$rt[which(frontier$checkSD == i)] = portfolio_rt
  }
  
  frontier = frontier %>%
    filter(abs(checkSD - realSD) < 1)
  frontierToMySQL = frontier %>% 
    select(-checkSD) 
  colnames(frontierToMySQL) = c("ax", "ay")
  frontierToMySQL =  toJSON(frontierToMySQL)
  
  frontierToMySQK
}


