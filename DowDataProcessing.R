fundamentals = read.csv(file.choose(), header = TRUE)
pricesSplitAdjusted = read.csv(file.choose(), header = TRUE)
prices = read.csv(file.choose(), header = TRUE)
securities = read.csv(file.choose(), header = TRUE)

dowIndex <- pricesSplitAdjusted %>% filter(symbol %in% c("MMM", "AXP", "AAPL", "BA", "CAT", "CVX", "CSCO", "KO", "DIS", "DOW",
                                                         "XOM", "GS", "HD", "IBM", "INTC", "JNJ", "JPM", "MCD", "MRK", "MSFT", 
                                                         "NKE", "PFE", "PG", "TRV", "UTX", "UNH", "VZ", "V", "WMT", "WBA"))
dowIndex <- dowIndex %>% mutate(dailyPrice = (open + close)/2)
dowIndex <- dowIndex %>% mutate(year = substring(date, 1, 4))

dowFundamentals <- inner_join(fundamentals, securities, by = c("Ticker.Symbol" = "Ticker.symbol")) %>% 
  select(Ticker.Symbol, Period.Ending, 
         Cash.Ratio, Total.Assets, Total.Revenue, Current.Ratio, Gross.Margin,
         Quick.Ratio, Research.and.Development, For.Year, Earnings.Per.Share, Estimated.Shares.Outstanding, Security,
         GICS.Sector, GICS.Sub.Industry, Address.of.Headquarters) %>% filter(Ticker.Symbol %in% c("MMM", "AXP", "AAPL", "BA", "CAT", "CVX", "CSCO", "KO", "DIS", "DOW",
         "XOM", "GS", "HD", "IBM", "INTC", "JNJ", "JPM", "MCD", "MRK", "MSFT", 
         "NKE", "PFE", "PG", "TRV", "UTX", "UNH", "VZ", "V", "WMT", "WBA"))
dowTotal <- inner_join(dowFundamentals, dowIndex, by = c("Ticker.Symbol" = "symbol"))
dowTotal %>% group_by(For.Year) %>% summarise(n_distinct(For.Year))
dowTotal <- dowTotal %>% select("For.Year", "Ticker.Symbol", "Cash.Ratio", "Total.Revenue", "Current.Ratio", "Gross.Margin", "Quick.Ratio", "Research.and.Development", "Earnings.Per.Share")
