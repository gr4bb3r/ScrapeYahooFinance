####Packages & Libraries####
#install.packages('rvest')
#install.packages('stringr')
library('rvest')
library('stringr')

####Table Architecture####
holdings = data.frame('Overall_Portfolio_Composition' = NA, 'Overall_Portfolio_Composition' = NA,
                      'Sector_Weightings' = NA, 'Sector_Weightings' = NA, 'Equity_Holdings' = NA,
                      'Equity_Holdings' = NA, 'Bond_Ratings' = NA, 'Bond_Ratings' = NA, 'Top_Ten' = NA)

holdings[1,1] = 'Stocks'
holdings[2,1] = 'Bonds'

holdings[1,3] = 'Basic Materials'
holdings[2,3] = 'CONSUMER_CYCLICAL'
holdings[3,3] = 'Financial Services'
holdings[4,3] = 'Realestate'
holdings[5,3] = 'Consumer Defensive'
holdings[6,3] = 'Healthcare'
holdings[7,3] = 'Utilities'
holdings[8,3] = 'Communication Services'
holdings[9,3] = 'Energy'
holdings[10,3] = 'Industrials'
holdings[11,3] = 'Technology'

holdings[1,5] = 'Price/Earnings'
holdings[2,5] = 'Price/Book'
holdings[3,5] = 'Price/Sales'
holdings[4,5] = 'Price/Cashflow'
holdings[5,5] = 'Median Market Cap'
holdings[6,5] = '3 Year Earnings Growth'

holdings[1,7] = 'US_Government'
holdings[2,7] = 'AAA'
holdings[3,7] = 'AA'
holdings[4,7] = 'A'
holdings[5,7] = 'BBB'
holdings[6,7] = 'BB'
holdings[7,7] = 'B'
holdings[8,7] = 'Below_B'
holdings[9,7] = 'Others'



####Scraping & Carpentry####
url = 'https://finance.yahoo.com/quote/ZHU.TO/holdings?p=ZHU.TO'

webpage = read_html(url)

equity_data_html = html_nodes(webpage,'section')

raw_equity_data = html_text(equity_data_html)
equity_data = raw_equity_data[2]

ptflo_comp_pos = stringr::str_locate(equity_data,"Overall Portfolio Composition")
sctr_wts_pos = stringr::str_locate(equity_data,"Sector Weightings")
eqty_hldgs_pos = stringr::str_locate(equity_data,"Equity Holdings")
bnd_hldgs_pos = stringr::str_locate(equity_data,"Bond Ratings")
bnd_hldgs_end_pos = stringr::str_locate(equity_data,"Top 10 Holdings")
top_ten_pos = stringr::str_locate(equity_data,"Top 10 Holdings")

raw_ptflo_comp = substr(equity_data,ptflo_comp_pos[2]+5,sctr_wts_pos[1]-1)
raw_sctr_wts = substr(equity_data,sctr_wts_pos[2]+5,eqty_hldgs_pos[1]-1)
raw_eqty_hldgs = substr(equity_data,eqty_hldgs_pos[2]+1,bnd_hldgs_pos[1]-1)
raw_bnd_hldgs = substr(equity_data,bnd_hldgs_pos[2]+13,bnd_hldgs_end_pos[1]-1)
top_ten = substr(equity_data,top_ten_pos[2]-14,nchar(equity_data))

count1 = 1
for (asset_class in na.omit(unique(holdings[,1]))){
  holdings[count1,2] = substr(raw_ptflo_comp,stringr::str_locate(raw_ptflo_comp,asset_class)[2]+1,
                              stringr::str_locate(raw_ptflo_comp,asset_class)[2]+5)
  count1 = count1 + 1
}

count2 = 1
for (sector in na.omit(unique(holdings[,3]))){
  holdings[count2,4] = substr(raw_sctr_wts,stringr::str_locate(raw_sctr_wts,sector)[2]+1,
                              stringr::str_locate(raw_sctr_wts,sector)[2]+5)
  count2 = count2 + 1
}

count3 = 1
for (equity in na.omit(unique(holdings[,5]))){
  holdings[count3,6] = substr(raw_eqty_hldgs,stringr::str_locate(raw_eqty_hldgs,equity)[2]+1,
                              stringr::str_locate(raw_eqty_hldgs,equity)[2]+5)
  count3 = count3 + 1
}

raw_bnd_hldgs = str_replace_all(raw_bnd_hldgs,"%"," ")
raw_bnd_hldgs = str_replace_all(raw_bnd_hldgs,"US Government","US_Government")
raw_bnd_hldgs = str_replace_all(raw_bnd_hldgs,"Below B","Below_B")

tblated_raw_bnd_hldgs = read.table(text = raw_bnd_hldgs, sep = " ",
                                   colClasses=c("character","character","character","character",
                                                "character","character","character","character",
                                                "character","character"))
x = "some text in a string"

substrRight = function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

holdings[1:ncol(tblated_raw_bnd_hldgs),8] = substrRight(tblated_raw_bnd_hldgs,5)

count4 = 1
for (bleh in holdings[,8]){
  if (suppressWarnings(is.na(as.numeric(bleh)) == TRUE)){
    pref = substr(bleh,1,1)
    holdings[count4,8] = as.numeric(str_split(bleh,pref)[[1]][2])
    count4 = count4 + 1
  }else{
    count4 = count4 + 1
  }
}

count5 = 1
for (bleh1 in holdings[,6]){
  if (suppressWarnings(is.na(as.numeric(bleh1)) == TRUE)){
    pref1 = substrRight(bleh1,1)
    holdings[count5,6] = as.numeric(str_split(bleh1,pref1)[[1]][1])
    count5 = count5 + 1
  }else{
    count5 = count5 + 1
  }
}

count6 = 1
for (bleh2 in holdings[,4]){
  if (suppressWarnings(is.na(as.numeric(bleh2)) == TRUE)){
    pref2 = substrRight(bleh2,1)
    holdings[count6,4] = as.numeric(str_split(bleh2,pref2)[[1]][1])
    count6 = count6 + 1
  }else{
    count6 = count6 + 1
  }
}

count7 = 1
for (bleh3 in holdings[,2]){
  if (suppressWarnings(is.na(as.numeric(bleh3)) == TRUE)){
    pref3 = substrRight(bleh3,1)
    holdings[count7,2] = as.numeric(str_split(bleh3,pref3)[[1]][1])
    count7 = count7 + 1
  }else{
    count7 = count7 + 1
  }
}

holdings[1,9] = top_ten

holdings[is.na(holdings)] = ""

#write.csv(holdings,'location')
