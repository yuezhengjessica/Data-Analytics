# this file use data from tree.select.r or its quivalence
# GETTER ------------------------------------------------------------------
id = id
DATE = DATE



weight.opti = function(daily_rt, forward_rt, SR=0.75, DATE="2016-12-30", days.lookBackSD=504, days.lookBackMmt=105, days.excludeMmt=21, forward_rt_type = "adjusted"){
  # daily_rt   is a dataframe
  # forward_rt is one row of a dataframe
  # DATE follow the format of YYYY-mm-dd
  percent2decimal = function(x){
    x/100
  }
  
  
  YTM2adjust_rt = function(YTM){
    adjusted_rt = YTM
    return(adjusted_rt)
  }
  adjusted_rt2proj_rt = function(adjusted_rt, daily_rt, SR, DATE, days.lookBackMmt, days.excludeMmt){
    needs(dplyr)
    
    excluded = daily_rt %>%
      filter(Date <= DATE) %>%
      top_n(as.numeric(days.excludeMmt), Date)$Date[1]
    
    moment_period.r = daily_rt %>%
      filter(Date < excluded) %>%
      top_n(as.numeric(days.lookBackMmt), Date)
    
    momentum_period = momentum_period.r %>% 
      select(-Date) %>% 
      as.data.frame()
    
    momentum2 = apply(1+momentum_period/100,2,prod)
    momentum2 = (momentum2-1)*as.numeric(input$scale)
    
    adjusted_rt.at.DATE = adjusted_rt %>% 
      filter(Date == DATE) %>%
      select(-Date) %>%
      as.numeric()
    a = as.matrix(adjusted_rt.at.DATE) + momentum2/2
      
    
    return(proj_rt)
  }
  
  
  if(forward_rt_type == "YTM"){
    forward_rt = forward_rt %>% YTM2adjust_rt() %>% adjusted_rt2proj_rt()
  } else if (forward_rt_type == "adjusted") {
    forward_rt = forward_rt %>% adjusted_rt2proj_rt()
  } else if (forward_rt_type == "projected") {
    forward_rt = forward_rt
  } else {stop("The type of forward return must be one from the following: YTM, adjusted, projected")}
  
  
  
}
