# Hello, world!
# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' @title init function for the package
#' @description used to setup the connect with database
#' @param NA
#'
init<-function()
{
  library('RPostgreSQL')
  library('sqldf')
  library('quantmod')
  library('PerformanceAnalytics')
  library('scales')
  library('lubridate')
  library('mgcv')
  library('quadprog')
  library('plyr')
  library('doParallel')
  options(sqldf.RPostgreSQL.user='root',sqldf.RPostgreSQL.password='r00t',sqldf.RPostgreSQL.dbname='finance',sqldf.RPostgreSQL.host='localhost',sqldf.RPostgreSQL.port=5432)
}
top.level.assets=c('000300','000012','000013','000832','H30009')
top.level.asset.names=c('stock','treasure','corporate.bond','convertiable','commodity')

high.level.assets=c('399373',	'399372','399377','399376','399375','399374',
                    '000979','H30009','000832','000101','399298','399299','000012')
high.level.asset.names=c('large.cap.value','large.cap.growth','small.cap.value',
                         'small.cap.growth','middle.cap.value','middle.cap.growth',
                         'commodity.stock','commodity.futures','convertiable','bond.5yr','szAApup','szAApdown','treasure')

stock.indexes=c('399373',	'399372','399377','399376','399375','399374','000300','399101','399606')
stock.names=c('large.cap.value','large.cap.growth','small.cap.value',
              'small.cap.growth','middle.cap.value','middle.cap.growth',
              'hs300','middle.small.cap',' growth.enterprise')
stock.industry=c('801010', '801020', '801030', '801040', '801050', '801080', '801110', '801120', '801130', '801140', '801150',
                 '801160', '801170', '801180', '801200', '801210', '801230', '801710', '801720', '801730', '801740', '801750',
                 '801760', '801770', '801780', '801790', '801880', '801890')
stock.industry.cname=c('农林牧渔',
                       '采掘',
                       '化工',
                       '钢铁',
                       '有色金属',
                       '电子',
                       '家用电器',
                       '食品饮料',
                       '纺织服装',
                       '轻工制造',
                       '医药生物',
                       '公用事业',
                       '交通运输',
                       '房地产',
                       '商业贸易',
                       '休闲服务',
                       '综合',
                       '建筑材料',
                       '建筑装饰',
                       '电气设备',
                       '国防军工',
                       '计算机',
                       '传媒',
                       '通信',
                       '银行',
                       '非银金融',
                       '汽车',
                       '机械设备')
stock.industry.name=c('AFFF','Mining','Chem','Steel','NFMetal','Construction','Decro')

#' @title randomstocks: sampling random number of stock within each industry
#' @description randomly sampled stock code and names are returned and save as dataset in the data directory
#' @param n how many stock sampled from each industry
#' @param older.than a string of date in the format '20000101' only, which is a creteria for selected stocks whose "time to Market" must be older than it.
#' @examples
#' randomstocks(3,older.than='20000203')
#'
randomstocks<-function(n,older.than='20000101')
{
  sql_str = paste('select code,name from (select code,name,industry, rank() over (partition by industry order by random()) as num from stock_from_k where "timeToMarket">0 and "timeToMarket"<=',older.than,') X where X.num<=',n,';',sep='')
  stocks=sqldf(sql_str)
  save(stocks,file='data/stocks.rda')
  return(stocks)
}

#' @title saveReturns: retrieve data from related data table and save them into a rda file
#' @param assets asset codes ,e.g. '000300' is the code for index hs300
#' @param asset.names readable names for each asset
#' @param data.name the R variable name you want when load the data into memory. e.g. "stocks"
#' @param where the asset class of assets. now only "stock" and "index" are supported. "Futures", "funds" will be supported soon.
#' @param from the starting date of asset prices
#' @param to the ending date of asset prices
#' @examples
#' st=randomstocks(3)
#' saveReturns(st[,1],st[,2],'stocks',where='stock',from='2000-01-01')
#' @export
saveReturns<-function(assets,asset.names,data.name,where,from,to=NULL)
{
  if(is.null(assets))
    stop("assets can not be NULL")
  table.name<-switch(where,stock='stock_ohlc_day_from_k',index='index_ohlc_day')
  join.method<-switch(where,stock='left',index='inner')
  freq<-c('daily','weekly','monthly','quarterly')
  for(f in freq)
  {
    a<-getReturnFromDB(assets,from,to,f,table.name,asset.names,join.method=join.method)

    file.name = paste('data/',data.name,'_',f,'.rda',sep='')
    var.name = paste(data.name,'_',f,sep='')
    print(var.name)
    assign(var.name ,a)
    save(list=var.name,file=file.name)
  }
}


#' @title Function to generate all return data
#' @param NA
#' @description currently, top level assets return, high level assets return of general asset classes (stock,bond,commodity,cash,real estate). returns of representitive indexes of stock market, second level industrial indexes of stock (provided by ShenWan), randomly picked stocks are computed and stored under the directory of data. Only called when you want to update the data from database
#' @examples
#' init()
#' genrate_all()
#' @export
generate_all<-function()
{
  saveReturns(top.level.assets,top.level.asset.names,"asset_top","index",'2000-01-01')
  top_assets = cbind(top.level.assets,top.level.asset.names)
  colnames(top_assets)<-c('code','name')
  save(top_assets,file='data/top.assets.rda')
  saveReturns(high.level.assets,high.level.asset.names,"asset_high","index",'2000-01-01')
  high_assets = cbind(high.level.assets,high.level.asset.names)
  colnames(high_assets)<-c('code','name')
  save(high_assets,file='data/high.assets.rda')
  saveReturns(stock.indexes,stock.names,"stock_index","index",'2000-01-01')
  stock_indexes=cbind(stock.indexes,stock.names)
  colnames(stock_indexes)<-c('code','name')
  save(stock_indexes,file='data/stock.indexes.rda')
  saveReturns(stock.industry,stock.industry.cname,"stock_industry","index",'2000-01-01')
  stock_industries = cbind(stock.industry,stock.industry.cname)
  colnames(stock_industries)<-c('code','name')
  save(stock_industries,file='data/stock.industries.rda')
  st = randomstocks(3)
  saveReturns(st[,1],st[,2],"stocks","stock",'2000-01-01')
  getShibor()
}
#' @title Function to get shibor rate
#' @param from the starting date of shibor rates
#' @param frequency taking values as "daily","weekly","monthly","quarterly","semiannually". If other value are supplied, then the annually rate is returned.
#' @description each record of shibor rates contains one day, one week, two week, one month, three months, six months, nine months and 1 year shibor rates
#' @examples
#' init()
#' getRiskfree()
#' @export
getRiskfree<-function(from='2006-01-01',frequency='daily')
{
  early.date = sqldf('select min(date) from shibor')[1,1]
  repo = NULL
  if(as.Date(from)<early.date)
  {
    sql_str = sprintf("select * from repo where the_date>='%s' and the_date<'%s' order by the_date",from,as.character(early.date))
    repo = sqldf(sql_str)
    returns<-xts(repo[-1],repo$the_date)
    if(frequency=='daily')
      repo = returns[,'1day']
    else if(frequency=='weekly')
      repo = returns[,'7day']
    else
      repo = returns[,'30day']
    colnames(repo)<-'r_f'
  }
  sql_str = sprintf("select * from shibor where date>='%s'",from)
  shibor=sqldf(sql_str)
  returns<-xts(shibor[-1],shibor$date)
  if(frequency =="daily")
    ret = returns[,'ON']
  else if(frequency=="weekly")
    ret = returns[,'1W']
  else if(frequency=="monthly")
    ret = returns[,'1M']
  else if(frequency=="quarterly")
    ret = returns[,'3M']
  else if(frequency=="semiannually")
    ret = returns[,'6M']
  else
    ret = returns[,'1Y']
  colnames(ret)<-'r_f'
  if(!is.null(repo))
  {
    ret = c(repo,ret)
  }

  return(ret)
}
#' @title Function to get shibor rate
#' @param from the starting date of shibor rates
#' @description each record of shibor rates contains one day, one week, two week, one month, three months, six months, nine months and 1 year shibor rates
#' @examples
#' init()
#' getShibor()
#' @export
getShibor<-function(from='2006-01-01')
{
  sql_str = sprintf("select * from shibor where date>='%s'",from)
  shibor=sqldf(sql_str)
  returns<-xts(shibor[-1],as.Date(shibor$date))
  shibor_daily = returns[,'ON']
  colnames(shibor_daily)<-'r_f'
  save(shibor_daily,file='data/shibor_daily.rda')
  shibor_weekly = returns[,'1W']
  colnames(shibor_weekly)<-'r_f'
  save(shibor_weekly,file='data/shibor_weekly.rda')
  shibor_monthly = returns[,'1M']
  colnames(shibor_monthly)<-'r_f'
  save(shibor_monthly,file='data/shibor_monthly.rda')
  shibor_quarterly = returns[,'3M']
  colnames(shibor_quarterly)<-'r_f'
  save(shibor_quarterly,file='data/shibor_quarterly.rda')
}


#' @title Function to get fund return rate
#' @param codes a vector of fund codes
#' @param from the starting date of shibor rates
#' @description return fund rate based on the fund code and starting date
#' @examples
#' init()
#' getFunds()
#' @export
getFunds<-function(codes,from='2006-01-01',fund.names=NULL,to=NULL)
{
  if(length(codes)>length(fund.names))
    fund.names = sapply(codes,function(x) paste('F',x,sep=''))
  df_old = NULL
  for(code in codes)
  {
    if(is.null(to))
      sql_str = sprintf("select the_date as date,rate as F%s from fund_nav_day_long where code='%s' and the_date>='%s'",code,code,from)
    else
      sql_str = sprintf("select the_date as date,rate as F%s from fund_nav_day_long where code='%s' and the_date>='%s' and the_date<'%s'",code,code,from,to)

    df=sqldf(sql_str)
    if(!is.null(df_old)&&!is.null(df))
    {
      sql_str = sprintf('select df_old.*,df.F%s from df_old left join df on df_old.date=df.date',code)
      df_old = sqldf(sql_str)
    }
    else
    {
      if(!is.null(df))
        df_old = df
    }
  }
  #names(df_old)[-1]=asset.names
  returns<-xts(df_old[-1],df_old$date)
  colnames(returns)=fund.names
  return(returns)
}

fund_holding_style<-function(fund.code,from='2013-01-01',to=NULL,style=1,frequency='monthly',constraint=list(stock=c(0.3,0.95),bond=c(0,1),cash=c(0.05,1)))
{
  if(style==1)
  {
    styles<-c('399372','399373','399374','399375','399376','399377','CBA001')
    style.names<-c('大盘成长','大盘价值', '中盘成长', '中盘价值', '小盘成长', '小盘价值','债券财富指数')
    df = getReturnFromDB(styles,from=from,to=to,frequency=frequency,asset.names=style.names)

  }
  else if(style==2)
  {
    #the earliest data of styles is 2002-01-04, which is determined by CBA001.
    styles<-c('801811','801812','801813','801821','801822','801823','801831','801832','801833','801851','801852','801853','801863','CBA001')
    style.names<-c('大盘','中盘','小盘','高PE','中PE','低PE','高PB','中PB','低PB','亏损股','微利股','绩优股','次新股','债券财富指数')
    df = getReturnFromDB(styles,from=from,to=to,frequency = frequency,asset.names = style.names)
  }
  early.date = as.Date(from)-365
  r_f<-getRiskfree(as.character(early.date),frequency)
  f3mean<-function(x)
  {
    if(length(x)>3)
      l=3
    else
      l=length(x)
    return(mean(x[1:l,],na.rm=TRUE))
  }
  f9mean<-function(x)
  {
    if(length(x)>9)
      l=9
    else
      l=length(x)
    return(mean(x[1:l,],na.rm=TRUE))
  }


  if(frequency=='daily')
  {

    r_f<-lag(r_f)
    colnames(r_f)<-c('cash')
    r_f = r_f/254/100
    df=merge(df,r_f)
    df=as.data.frame(df)
  }
  else
    {
      if(frequency=='weekly')
      {
        r_f<-apply.weekly(r_f,f3mean)
        r_f<-lag(r_f)
        r_f<-na.locf(r_f)
        colnames(r_f)<-c('cash')
        r_f = r_f/54/100
        r_f$year = year(index(r_f))
        r_f$week = week(index(r_f))
        r_f = as.data.frame(r_f)
        df$year = year(index(df))
        df$week = week(index(df))
        df = as.data.frame(df)
        df = merge(df,r_f,c('year','week'),sort=FALSE)[,c(-1,-2)]
      }
      else
      {
        if(frequency=='monthly')
        {
          r_f<-apply.monthly(r_f,f9mean)
          r_f<-lag(r_f)
          r_f<-na.locf(r_f)
          colnames(r_f)<-c('cash')
          r_f = r_f/12/100
          r_f$year = year(index(r_f))
          r_f$month = month(index(r_f))
          r_f = as.data.frame(r_f)
          df$year = year(index(df))
          df$month = month(index(df))
          df = as.data.frame(df)
          df = merge(df,r_f,c('year','month'),sort=FALSE)[,c(-1,-2)]
        }
        else
        {
          if(frequency=='quarterly')
          {
            r_f<-apply.quarterly(r_f,f9mean)
            r_f<-lag(r_f)
            r_f<-na.locf(r_f)
            colnames(r_f)<-c('cash')
            r_f = r_f/4/100
            r_f$year = year(index(r_f))
            r_f$quarter = quarter(index(r_f))
            r_f = as.data.frame(r_f)
            df$year = year(index(df))
            df$quarter = quarter(index(df))
            df = as.data.frame(df)
            df = merge(df,r_f,c('year','quarter'),sort=FALSE)[,c(-1,-2)]
          }
          else
          {
            stop('frequency cannot take values except daily, weekly, monthly or quarterly')
          }
        }
      }
    }
  fund<-getFunds(fund.code,from=from,to=to)
  if(frequency=='weekly')
  {
    fund<-apply.weekly(fund,Return.cumulative)
  }
  else
    {
      if(frequency=='monthly')
      {
        fund<-apply.monthly(fund,Return.cumulative)
      }
      else
        {
          if(frequency=='quarterly')
          {
            fund<-apply.quarterly(fund,Return.cumulative)
          }
        }
    }

  C<-matrix(1,1,ncol(df))
  c<-1
  if(!is.null(constraint$cash))
  {
    cash.min=constraint$cash[1]
  }
  else
  {
    cash.min =0.05
  }

  w<-fund*0+1
  Ain<-matrix(0,5+ncol(df),ncol(df))
  bin<-rep(0,5+ncol(df))
  Ain[1,]=c(rep(1,length(styles)-1),0,0)
  bin[1]=ifelse(!is.null(constraint$stock),constraint$stock[1],0)
  Ain[2,]=c(rep(-1,length(styles)-1),0,0)
  bin[2]=ifelse(!is.null(constraint$stock),-constraint$stock[2],-1)
  Ain[3,]=c(rep(0,length(styles)-1),1,0)
  bin[3]=ifelse(!is.null(constraint$bond),constraint$bond[1],0)
  Ain[4,]=c(rep(0,length(styles)-1),-1,0)
  bin[4]=ifelse(!is.null(constraint$bond),-constraint$bond[2],-1)
  Ain[5,]=c(rep(0,length(styles)),1)
  bin[5]=cash.min
  Ain[c(-1,-2,-3,-4,-5),]=diag(1,ncol(df),ncol(df))
  p<-c(rep((1-cash.min-bin[3]-0.0002)/(length(styles)-1),length(styles)-1),bin[3]+0.0001,cash.min+0.0001)
  M<-list(X=as.matrix(df),y=fund,p=p,off=array(0,0),S=list(),w=w,c=c,C=C,Ain=Ain,bin=bin,sp=array(0,0))
  p=pcls(M)    ### cannot find the right solution
  yhat <- M$X %*% p
  ybar<-mean(fund)
  r_square =1- sum((fund-yhat)^2)/sum((fund-ybar)^2)
  return(list(factors=c(style.names,'货币'), coefficients=p,r_square=r_square))
#  Dmat = t(as.matrix(df)) %*% as.matrix(df)
#  dvec = t(as.matrix(df)) %*% fund
#  Amat<-matrix(0,ncol(df),6+ncol(df))
#  Amat[,1]=C
#  Amat[,c(2,3,4,5,6)]=t(Ain)
#  Amat[,c(-1,-2,-3,-4,-5,-6)]=diag(1,ncol(df),ncol(df))
#  bvec=c(c,bin,rep(0,ncol(df)))
#  meq = 1
#  solve.QP(Dmat,dvec,Amat,bvec,meq)
}
create_table<-function(df,table_name)
{
  sqlstr = sprintf('create table %s (',table_name)
  columns<-colnames(df)
  coltypes<-sapply(df,class)
  for(i in seq(1,ncol(df)))
  {
    if(coltypes[i]=="character")
      dbtypename='text'
    else
      if(coltypes[i]=="Date")
        dbtypename='date'
      else
        dbtypename=coltypes[i]
    sqlstr = paste(sqlstr, sprintf('"%s" %s',columns[i],dbtypename),sep='\n')
    if(i<ncol(df))
      sqlstr = paste(sqlstr,',',sep='')
  }
  sqlstr=paste(sqlstr,')',sep='')
  sqldf(sqlstr)
}


to_sql<-function(df,table_name,if_exists='append')
{
  stopifnot(is.data.frame(df))
  sqlstr=sprintf("select 1 from information_schema.tables where table_name='%s'",table_name)
  exist_df=sqldf(sqlstr)

  if(nrow(exist_df)==1)
  {
    if(if_exists=='replace')
    {
      sqlstr=sprintf("drop table %s",table_name)
      sqldf(sqlstr)
      create_table(df,table_name)
    }
  }
  else
  {
    create_table(df,table_name)
  }

  coltypes<-sapply(df,class)
  for(j in seq(1,nrow(df)))
  {
    sqlstr = sprintf('insert into %s ("%s") values(',table_name,paste(colnames(df),collapse='","'))
    for(i in seq(1,ncol(df)))
    {
      if(coltypes[i]=="character"||coltypes[i]=='Date')
        sp="'"
      else
        sp=''
      if(coltypes[i]=='numeric'&& is.na(df[j,i]))
        val_str = '0'
      else
        val_str = as.character(df[j,i])

      sqlstr = paste(sqlstr, sp,val_str,sp,sep='')
      if(i<ncol(df))
        sqlstr = paste(sqlstr,',',sep='')
    }
    sqlstr = paste(sqlstr,')',sep='')
    sqldf(sqlstr)
  }
}

generate_industry_returns<-function()
{
  df=sqldf('select * from industry_section where indicator=1 order by industry_code')
  for(i in seq(1,nrow(df)))
  {
    re<-getReturnFromDB(df[i,'index_code'],from='2000-01-01',to='2013-01-01',frequency='quarterly',asset.names=c('return'))
    if(is.null(re))
    {
      re<-getReturnFromDB(df[i,'backup_index'],from='2000-01-01',to='2013-01-01',frequency='quarterly',asset.names=c('return'))
    }
    if(!is.null(re))
    {

      re$year<-year(index(re))
      re$quarter<-quarter(index(re))
      re<-as.data.frame(re)
      re$industry_code<-df[i,'industry_code']
      to_sql(re,'industry_performance')
    }
  }
  df=sqldf('select * from industry_section where indicator=2 order by industry_code')
  for(i in seq(1,nrow(df)))
  {
    re<-getReturnFromDB(df[i,'index_code'],from='2013-01-01',frequency='quarterly',asset.names=c('return'))

    re$year<-year(index(re))
    re$quarter<-quarter(index(re))
    re<-as.data.frame(re)
    re$industry_code<-df[i,'industry_code']
    to_sql(re,'industry_performance')
  }
  sqldf('refresh materialized view fund_industry_performance')
  sqldf('refresh materialized view fund_industry_contribution')
  sqldf('refresh materialized view fund_industry_performance_adjusted')
  sqldf('refresh materialized view fund_industry_contribution_adjusted')
  sqldf('refresh materialized view fund_stock_performance')
  sqldf('refresh materialized view fund_stock_performance_adjusted')
  sqldf('refresh materialized view fund_industry_performance_with_selected_stocks')
}


generate_top_asset_returns<-function()
{
  df=sqldf('select * from top_assets')
  assets=c()
  asset.names=c()
  for(i in seq(1,nrow(df)))
  {
    if(df[i,'class']=='cash')
    {
      r_f<-getRiskfree(from='2006-10-01',frequency='quarterly')
      f10mean<-function(x)
      {
        if(length(x)>10)
          l=10
        else
          l=length(x)
        return(mean(x[1:l,],na.rm=TRUE))
      }
      r_f<-apply.quarterly(r_f,f10mean)
      r_f<-lag(r_f)
      colnames(r_f)<-c('cash')
    }
    else
    {
      assets=append(assets,df[i,'code'])
      asset.names = append(asset.names ,df[i,'class'])
    }
  }

  re<-getReturnFromDB(assets,from='2007-01-01',frequency='quarterly',asset.names=asset.names,join.method='left')
  re$year<-year(index(re))
  re$quarter<-quarter(index(re))
  re<-as.data.frame(re)
  r_f<-r_f/400;
  r_f$year<-year(index(r_f))
  r_f$quarter<-quarter(r_f)
  r_f = as.data.frame(r_f)
  re=merge(re,r_f,c('year','quarter'))
  to_sql(re,'top_asset_return')
}
#' @title One of the three contribution function. This is for asset allocation.
top_asset_contribution<-function(code,from='2013-01-01',to=NULL)
{
  if(is.null(to))
    sqlstr =  sprintf("select c.code,yearqtr(b.year,b.quarter),b.contribution_algo1,b.contribution_algo2,b.contribution_algo3,(c.stock*a.stock+c.bond*a.bond+c.cash*a.cash)*100 as bogey_return from top_asset_return a join fund_asset_performance_adjusted b on a.year=b.year and a.quarter=b.quarter join fund_bogey_allocation c on b.code=c.code where c.code='%s' and yearqtr(b.year,b.quarter)>='%s'",code,as.yearqtr(as.Date(from)))
  else
    sqlstr =  sprintf("select c.code,yearqtr(b.year,b.quarter),b.contribution_algo1,b.contribution_algo2,b.contribution_algo3,(c.stock*a.stock+c.bond*a.bond+c.cash*a.cash)*100 as bogey_return from top_asset_return a join fund_asset_performance_adjusted b on a.year=b.year and a.quarter=b.quarter join fund_bogey_allocation c on b.code=c.code where c.code='%s' and yearqtr(b.year,b.quarter)>='%s' and yearqtr(b.year,b.quarter)<='%s'",code,as.yearqtr(as.Date(from)),as.yearqtr(as.Date(to)))
  return(sqldf(sqlstr))
}
#' @title One of the three contribution function. This is for industry or say stock section.
industry_contribution<-function(code,from='2013-01-01',to=NULL)
{
  if(is.null(to))
    sqlstr = sprintf('select yearqtr,contribution_algo3 as contribution from fund_industry_contribution_adjusted where yearqtr>=\'%s\' and code=\'%s\'; ',as.yearqtr(as.Date(from)),code)
  else
    sqlstr = sprintf('select yearqtr,contribution_algo3 as contribution from fund_industry_contribution_adjusted where yearqtr>=\'%s\' and yearqtr<= \'%s\'and code=\'%s\'; ',as.yearqtr(as.Date(from)),as.yearqtr(as.Date(to)),code)
  return(sqldf(sqlstr))
}
#' @title One of the three contribution function. This is for stock selection
stock_contribution<-function(code,from='2013-01-01',to=NULL)
{
  if(is.null(to))
    sqlstr = sprintf('select report_date,industry_code,weight_algo3,industry_return_algo3,bogey_return from fund_industry_performance_with_selected_stocks where code=\'%s\' and report_date>=year_half(\'%s\') ' ,code,as.yearqtr(as.Date(from)))
  else
    sqlstr = sprintf('select report_date,industry_code,weight_algo3,industry_return_algo3,bogey_return from fund_industry_performance_with_selected_stocks where code=\'%s\' and report_date>=year_half(\'%s\') and report_date<=year_half(\'%s\') ' ,code,as.yearqtr(as.Date(from)),as.yearqtr(as.Date(to)))
  return(sqldf(sqlstr))
}
#' @title Contribution due to market timing
#' @param code fund code
#' @from the starting date
#'
timing_contribution<-function(code,from='2013-01-01',to=NULL)
{
  if(is.null(to))
    sqlstr = sprintf("select t.code,t.year_half,sum(t.est_equity_return) as est_equity_return from
      (
      select
       b.code,
       yearqtr(b.year,b.quarter),
       year_half(b.year,b.quarter),
       b.mid_equity,
       (b.mid_bond*a.bond+b.mid_cash*a.cash)/100 as other_return,
       c.rate,
       (c.rate*100-b.mid_bond*a.bond-b.mid_cash*a.cash)/mid_equity as est_equity_return
       from top_asset_return a join fund_asset_performance_adjusted b on a.year=b.year and a.quarter=b.quarter join fund_quarter_performance c on b.code=c.code and b.year=date_part('year',c.the_date) and b.quarter=date_part('quarter',c.the_date) where c.code='%s' and yearqtr(b.year,b.quarter)>='%s'
       ) t
       group by t.code,t.year_half order by t.code,t.year_half;
       ",code,as.yearqtr(as.Date(from)))
  else
    sqlstr = sprintf("select t.code,t.year_half,sum(t.est_equity_return) as est_equity_return from
      (
      select
       b.code,
       yearqtr(b.year,b.quarter),
       year_half(b.year,b.quarter),
       b.mid_equity,
       (b.mid_bond*a.bond+b.mid_cash*a.cash)/100 as other_return,
       c.rate,
       (c.rate*100-b.mid_bond*a.bond-b.mid_cash*a.cash)/mid_equity as est_equity_return
       from top_asset_return a join fund_asset_performance_adjusted b on a.year=b.year and a.quarter=b.quarter join fund_quarter_performance c on b.code=c.code and b.year=date_part('year',c.the_date) and b.quarter=date_part('quarter',c.the_date) where c.code='%s' and yearqtr(b.year,b.quarter)>='%s' and yearqtr(b.year,b.quarter)<='%s'
       ) t
       group by t.code,t.year_half order by t.code,t.year_half;
       ",code,as.yearqtr(as.Date(from)),as.yearqtr(as.Date(to)))
  dfA=sqldf(sqlstr)
  dfB=stock_contribution(code,from)
  dfB=aggregate(weight_algo3*industry_return_algo3~report_date,dfB,sum)
  colnames(dfB)<-c('date','all_other_contribution')
  dfC = merge(dfA,dfB,by.x='year_half',by.y='date')
  dfC$timing_contribution = dfC[,3]*100-dfC[,4]
  return(dfC[,c(1,5)])
}

#' @title Function to generate fund style data
#' @param code fund code
#' @param from starting date to analysis, usually it is the date when the studied fund manager took place initially.
#' @description return a data frame which contains style data of the studied fund or fund manager
#' @examples
#' init()
#' fund.style('540006','2014-09-16')
#' @export
fund.style<-function(code,from='2013-01-01',to=NULL)
{
  if(is.null(to))
    sqlstr = sprintf('select * from fund_style_report where yearqtr>=\'%s\' and code=\'%s\'; ',as.yearqtr(as.Date(from)),code)
  else
    sqlstr = sprintf('select * from fund_style_report where yearqtr>=\'%s\' and yearqtr<=\'%s\' and code=\'%s\'; ',as.yearqtr(as.Date(from)),as.yearqtr(as.Date(to)),code)
  return(sqldf(sqlstr))
}

#' @title Function to generate fund performance report table
#' @param  a vector of fund returns
#' @description return table
#' @examples
#' init()
#' returns = getFunds('540006')
#' fund.performance(returns)
#' @export
fund.performance<-function(returns,flag = 1)
{
  if(flag==1)
  {
    names <-colnames(returns)
    d<-Sys.Date()
    halfy<-tail(returns,127)
    hd<-d-182
    oney<-tail(returns,254)
    od<-d-365
    twoy<-tail(returns,508)
    wd<-d-730
    threey<-tail(returns,762)
    td<-d-1095
    risk.free<-c()
    sql_str = sprintf("select avg(close) as rate from bond_yield_ohlc_day where the_date>='%s' and the_date<='%s' and code='TBIY'",hd-10,hd+10)
    bond=sqldf(sql_str)
    risk.free<-cbind(risk.free,bond$rate)
    sql_str = sprintf("select avg(close) as rate from bond_yield_ohlc_day where the_date>='%s' and the_date<='%s' and code='TBIY'",od-10,od+10)
    bond=sqldf(sql_str)
    risk.free<-cbind(risk.free,bond$rate)
    sql_str = sprintf("select avg(close) as rate from bond_yield_ohlc_day where the_date>='%s' and the_date<='%s' and code='TB2Y'",wd-10,wd+10)
    bond=sqldf(sql_str)
    risk.free<-cbind(risk.free,bond$rate)
    sql_str = sprintf("select avg(close) as rate from bond_yield_ohlc_day where the_date>='%s' and the_date<='%s' and code='TB3Y'",td-10,td+10)
    bond=sqldf(sql_str)
    risk.free<-cbind(risk.free,bond$rate)
    risk.free[is.na(risk.free)]=mean(risk.free,na.rm=TRUE)
    half.ave<-mean(halfy,na.rm=TRUE)
    half.sharpe<-(half.ave-risk.free[1])/sd(halfy,na.rm=TRUE)
    one.ave<-mean(oney,na.rm=TRUE)
    one.sharpe<-(one.ave-risk.free[2])/sd(oney,na.rm=TRUE)
    two.ave<-mean(twoy,na.rm=TRUE)
    two.sharpe<-(two.ave-risk.free[2])/sd(twoy,na.rm=TRUE)
    three.ave<-mean(threey,na.rm=TRUE)
    three.sharpe<-(three.ave-risk.free[2])/sd(threey,na.rm=TRUE)
    table<-data.frame(name=names,half_return=percent(as.vector(Return.cumulative(halfy))),half_sharpe=as.vector(SharpeRatio.annualized(halfy,risk.free[1]/254/100)),one_return=percent(as.vector(Return.cumulative(oney))),one_sharpe=as.vector(SharpeRatio.annualized(oney,risk.free[2]/254/100)),two_return=percent(as.vector(Return.cumulative(twoy))),two_sharpe=as.vector(SharpeRatio.annualized(twoy,risk.free[3]/254/100)),three_return=percent(as.vector(Return.cumulative(threey))),three_sharpe=as.vector(SharpeRatio.annualized(threey,risk.free[4]/254/100)))
    return(table)
    }
  else if(flag==2)
  {
    r_f <- getRiskfree()
    r_f = lag(r_f)
    res = matrix(0,ncol(returns),6)
    for(i in seq(1,ncol(returns)))
    {
      fund<-returns[,i]-r_f/254/100
      sp<-apply.quarterly(fund,SharpeRatio.annualized)
      res[i,1]=mean(sp,na.rm=TRUE)
      res[i,seq(2,6)]=quantile(sp,na.rm=TRUE)
    }
    res_df = cbind(colnames(returns),as.data.frame(res))

    return(res_df)
  }
  else
  {
    r_f <- getRiskfree()
    r_f = lag(r_f)
    col.n <-colnames(returns)
    returns.excess = Return.excess(returns,r_f/254/100)
    colnames(returns.excess)<-col.n
    op <- par(no.readonly = TRUE)
    layout(matrix(c(1, 2, 3)), heights = c(1, 0.75, 1), widths = 1)
    par(mar = c(1, 4, 4, 2))
    chart.TimeSeries(rollapply(returns,width=254,FUN='Return.cumulative',align='left'),main='定期(1yr)投资者收益的数据分析',xlim=c(1,nrow(returns)-254),xaxis = FALSE,ylab='累计收益率',legend.loc='topright')
    par(mar = c(1, 4, 0, 2))
    chart.TimeSeries(rollapply(returns,width=254,FUN='StdDev.annualized',align='left'),main='',xlim=c(1,nrow(returns)-254),xaxis = FALSE,ylab='日收益波动率')
    par(mar = c(5, 4, 0, 2))
    chart.TimeSeries(rollapply(returns.excess,width=254,FUN='SharpeRatio.annualized',align='left'),main='',xlim=c(1,nrow(returns)-254),xlab='投资起始日期',ylab='日收益年化夏普率')
    par(op)
    }
}

#' @title get returns from my postgresql database
#' @description do not call it if you do not have the same dataabse with you, this package is designed to supply data only.
#' @param assets list of asset codes
#' @param from the starting date in string format
#' @param to the ending date in string format, no limit if this is set to NULL (default)
#' @param frequency can only take values from 'daily','weekly','monthly' and 'quarterly'
#' @param table.name tablename in postgresql database. We will try to retrieve price information from the table
#' @param asset.names a readable name for each asset, in a list format
#' @param join.method take values from "inner", "outer", "left inner" and "right inner", but may not support by db driver.
#' @examples
#' getReturnFromDB(c('000300','000012'),'2000-01-01',to=NULL,table.name='index_ohlc_day',frequency='daily',asset.names=c('hs300','teasure.bond'))
#'
#'
getReturnFromDB<-function(assets,from,to=NULL,frequency='daily',table.name='index_ohlc_day',asset.names=NULL,join.method='inner')
{
  if(is.null(table.name))
    stop('table.name must be filled')
  if(is.null(join.method))
    join.method='inner'
  if(length(assets)>length(asset.names))
    asset.names = sapply(assets,function(x) paste('X',x,sep=''))
  df_old = NULL
  for(asset in assets)
  {
    if(is.null(to))
      sql_str = sprintf("select date,close as X%s from %s where code='%s' and date>='%s'",asset,table.name,asset,from)
    else
      sql_str = sprintf("select date,close as X%s from %s where code='%s' and date>='%s' and date<'%s'",asset,table.name,asset,from,to)

    df=sqldf(sql_str)
    if(!is.null(df_old)&&!is.null(df))
    {
      sql_str = sprintf('select df_old.*,df.X%s from df_old %s join df on df_old.date=df.date',asset, join.method)
      df_old = sqldf(sql_str)
    }
    else
    {
      if(!is.null(df))
        df_old = df
    }
  }
  #names(df_old)[-1]=asset.names
  if(nrow(df)==0)
    return(NULL)
  prices<-xts(df_old[-1],df_old$date)
  returns = NULL
  for(i in seq(ncol(prices)))
  {
    return <- periodReturn(prices[,i],period=frequency)
    if(!is.null(returns))
    {
      returns = merge(returns,return)
    }
    else
    {
      returns = return
    }
  }
  #asset.names = sapply(asset.names,function(x) paste(x,frequency,'return',sep='.'))
  colnames(returns)=asset.names
  return (returns)
}

#' @title get price from my postgresql database
#' @description do not call it if you do not have the same dataabse with you, this package is designed to supply data only.
#' @param assets list of asset codes
#' @param from the starting date in string format
#' @param to the ending date in string format, no limit if this is set to NULL (default)
#' @param frequency can only take values from 'daily','weekly','monthly' and 'quarterly'
#' @param table.name tablename in postgresql database. We will try to retrieve price information from the table
#' @param asset.names a readable name for each asset, in a list format
#' @param join.method take values from "inner", "outer", "left inner" and "right inner", but may not support by db driver.
#' @examples
#' getReturnFromDB(c('000300','000012'),'2000-01-01',to=NULL,table.name='index_ohlc_day',frequency='daily',asset.names=c('hs300','teasure.bond'))
#'
#'

getPriceFromDB<-function(assets,from,to=NULL,table.name='index_ohlc_day',asset.names=NULL,join.method='inner')
{
  if(is.null(table.name))
    stop('table.name must be filled')
  if(is.null(join.method))
    join.method='inner'
  if(length(assets)>length(asset.names))
    asset.names = sapply(assets,function(x) paste('X',x,sep=''))
  df_old = NULL
  for(asset in assets)
  {
    if(is.null(to))
      sql_str = sprintf("select date,close as X%s from %s where code='%s' and date>='%s'",asset,table.name,asset,from)
    else
      sql_str = sprintf("select date,close as X%s from %s where code='%s' and date>='%s' and date<'%s'",asset,table.name,asset,from,to)

    df=sqldf(sql_str)
    if(!is.null(df_old)&&!is.null(df))
    {
      sql_str = sprintf('select df_old.*,df.X%s from df_old %s join df on df_old.date=df.date',asset, join.method)
      df_old = sqldf(sql_str)
    }
    else
    {
      if(!is.null(df))
        df_old = df
    }
  }
  colnames(df_old)<-c('date',asset.names)
  return(df_old)
}

create_finance_table_with_filled_values<-function()
{
  df=sqldf(sprintf('select code,cast(to_char("timeToMarket",\'99999999\') as date) as early_date from stock_from_k where "timeToMarket" is not null and "timeToMarket">0 and cast(to_char("timeToMarket",\'99999999\') as date)<=\'%s\'',as.character(Sys.Date()-20)))
  registerDoParallel(cores=16)
  foreach(i=1:nrow(df)) %dopar%
  {
    sqlstr = sprintf("with x as (select * from stock_finance_weight_eastmoney where code='%s' order by date) select b.date as all_date,x.* from x right outer join stock_calendar b on x.date=b.date where b.date>='%s' order by b.date",df[i,'code'],df[i,'early_date'])
    dat = sqldf(sqlstr)
    if(dat$code[1]=='NA')
      next
    dat = ddply(dat,.(all_date),function(x)replace(x,TRUE,lapply(x,na.locf)))
    dat = dat[,-4]
    to_sql(dat,'stock_finance_weight_with_fill_eastmoney','append')
  }
}
