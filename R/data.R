#' top level asset returns
#' data are download from finance.sina.com.cn through tushare
#'
#' @format An xts
#' \describle{
#' \item{stock}{hs300, index of 300 large cap stock in Shanghai and Shenzhen stock exchange market, code ='000300'}
#' \item{treasure}{index of treasure bonds in the exchange market. code ='000012'}
#' \item{corporate.bond}{index of corporate bonds in the exchange market. code='000013'}
#' \item{convertable}{index of convertable bond, code='000832'}
#' \item{commodity}{index of commodity futures code='H30009'}
#' }
#'
"asset_top_daily"

#' weekly return of top assets
#' @seealso asset_top_daily
"asset_top_weekly"

#' monthly return of top assets
#' @seealso asset_top_daily
"asset_top_monthly"

#' quarterly return of top assets
#' @seealso asset_top_daily
"asset_top_quarterly"



#' high level asset returns
#' @format An xts
#' \describle{
#' \item {large.cap.value}{large capital growth corporate, code=399373}
#' \item {large.cap.growth}{large capital growth corporate, code=399382}
#' \item {small.cap.value}{small capital value corporate, code=399377}
#' \item {small.cap.growth}{small capital growth corporate, code=399376}
#' \item {middle.cap.value}{middle capital value corportate, code=399375}
#' \item {midle.cap.growth}{middle capital corporate with growth opportunities, code=399374}
#' \item {commodity.stock}{cmmodity index in stock market, code=000979}
#' \item {commodity.futures}{commodity index in futures market, code=H30009}
#' \item {convertiable}{convertable bond code=000832}
#' \item {bond.5yr}{credit bond with maturity almost equals to 5 years,code=000101}
#' \item {szAApup}{credit bond with grade geq AA+,code=399298}
#' \item {szAApdown}{credit bond with grade less AA+, code=399299}
#' \item {treasure}{teasure bond index code=000012}
#'}
"asset_high_daily"

#' weekly return of high assets
#' @seealso asset_high_daily
"asset_high_weekly"

#' monthly return of high assets
#' @seealso asset_high_daily
"asset_high_monthly"

#' quarterly return of high assets
#' @seealso asset_high_daily
"asset_high_quarterly"



#' stock indexes daily return
#'
#' @format an xts
#' \describle{
#' \item {large.cap.value}{large capital growth corporate, code=399373}
#' \item {large.cap.growth}{large capital growth corporate, code=399382}
#' \item {small.cap.value}{small capital value corporate, code=399377}
#' \item {small.cap.growth}{small capital growth corporate, code=399376}
#' \item {middle.cap.value}{middle capital value corportate, code=399375}
#' \item {midle.cap.growth}{middle capital corporate with growth opportunities, code=399374}
#' \item{stock}{hs300, index of 300 large cap stock in Shanghai and Shenzhen stock exchange market, code ='000300'}
#' \item{middle.small.cap}{index of middle or small corporates, code=399101}
#' \item{growth.enterprise}{index of growth.enterprise, code=399606}
#' }
#'
"stock_index_daily"

#' stock indexes weekly return
#'
#' @format an xts
#' \describle{
#' \item {large.cap.value}{large capital growth corporate, code=399373}
#' \item {large.cap.growth}{large capital growth corporate, code=399382}
#' \item {small.cap.value}{small capital value corporate, code=399377}
#' \item {small.cap.growth}{small capital growth corporate, code=399376}
#' \item {middle.cap.value}{middle capital value corportate, code=399375}
#' \item {midle.cap.growth}{middle capital corporate with growth opportunities, code=399374}
#' \item{stock}{hs300, index of 300 large cap stock in Shanghai and Shenzhen stock exchange market, code ='000300'}
#' \item{middle.small.cap}{index of middle or small corporates, code=399101}
#' \item{growth.enterprise}{index of growth.enterprise, code=399606}
#' }
#'
"stock_index_weekly"

#' stock indexes monthly return
#'
#' @format an xts
#' \describle{
#' \item {large.cap.value}{large capital growth corporate, code=399373}
#' \item {large.cap.growth}{large capital growth corporate, code=399382}
#' \item {small.cap.value}{small capital value corporate, code=399377}
#' \item {small.cap.growth}{small capital growth corporate, code=399376}
#' \item {middle.cap.value}{middle capital value corportate, code=399375}
#' \item {midle.cap.growth}{middle capital corporate with growth opportunities, code=399374}
#' \item{stock}{hs300, index of 300 large cap stock in Shanghai and Shenzhen stock exchange market, code ='000300'}
#' \item{middle.small.cap}{index of middle or small corporates, code=399101}
#' \item{growth.enterprise}{index of growth.enterprise, code=399606}
#' }
#'
"stock_index_monthly"

#' stock indexes quarter return
#'
#' @format an xts
#' \describle{
#' \item {large.cap.value}{large capital growth corporate, code=399373}
#' \item {large.cap.growth}{large capital growth corporate, code=399382}
#' \item {small.cap.value}{small capital value corporate, code=399377}
#' \item {small.cap.growth}{small capital growth corporate, code=399376}
#' \item {middle.cap.value}{middle capital value corportate, code=399375}
#' \item {midle.cap.growth}{middle capital corporate with growth opportunities, code=399374}
#' \item{stock}{hs300, index of 300 large cap stock in Shanghai and Shenzhen stock exchange market, code ='000300'}
#' \item{middle.small.cap}{index of middle or small corporates, code=399101}
#' \item{growth.enterprise}{index of growth.enterprise, code=399606}
#' }
#'
"stock_index_quarterly"




#' individual stock daily returns. stocks are randomly sampled from each industries. At most 3 stock within each industry.
#' @format an xts
"stocks_daily"

#' individual stock weekly returns. stocks are randomly sampled from each industries. At most 3 stock within each industry.
#' @format an xts
"stocks_weekly"


#' individual stock monthly returns. stocks are randomly sampled from each industries. At most 3 stock within each industry.
#' @format an xts
"stocks_monthly"

#' individual stock quarter returns. stocks are randomly sampled from each industries. At most 3 stock within each industry.
#' @format an xts
"stocks_quarterly"

#' Shibor rate daily: shibor=Shanghai Inter Bank
"shibor_daily"

