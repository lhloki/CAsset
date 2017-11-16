Linear Models
================
Hai Lan
2017-09-01

简单线性模型
------------

统计当中的线性模型在实际中有很多的应用。比如基金分析中有一个大的方向是基金风格分析（fund style analysis)。所谓基金风格，其实指的是基金管理人在选择投资对象时机中显示除来的对于资产类别、投资策略与时点的偏好。比如，是否偏重成长股，是否暴露比较多的系统性风险等等。很自然基金风格分析基本有两大类方法，基于持仓数据的或者基于净值变化的。基于持仓数据很好理解，基金的风格一定是通过持有的投资对象表现出来的，如果持有较大权重的成长股，那么该基金的风格就可能是成长型的。困难在两个方面：一是不是说每个资产的属性是单调的，一个股票可能是小盘的（从规模上）、高PB的（从估值上）和成长的（从增长性上）复合，甚至可能是成长和价值（看起来两个比较容易冲突的属性）的结合。另外一个困难是，持仓数据比较难以全面获得，或者获得的数据精确度不够。比如私募基金持仓数据，往往只对其投资者部分开放（前十持仓）；公募基金持仓数据则每半年更新一次，而很多公募基金的交易频率快于持仓数据更新频率。假设数据来源以及精度不成问题的情况下，那么基金风格分析，则通过分析所持有的每一个投资对象的风格，然后按照投资权重加和的方法来度量。
$$
\\mbox{Fund Return}\_t=\\sum\_{i}^n w\_{i,t} r\_{i,t} 
$$
 其中，*r*<sub>*i*, *t*</sub>为投资对象*i*在*t*时刻的回报率，其满足
$$
r\_{i,t}=\\alpha\_i+\\sum\_j^m\\beta\_{i,j}F\_{j,t}+\\epsilon\_{i,t}
$$
 此处*F*<sub>*j*, *t*</sub>, *j* = 1, ⋯, *m*为风格因子*j*在*t*时刻的表现。比如我们可以取规模（大、中、小）和类型（成长、价值）两个维度共6个因子。中国市场上已经有对应这6个因子的指数，指数的涨跌幅，就对应了公式中的*F*<sub>*j*, *t*</sub>。 上式对应的模型，就是一个最为简单的线性模型。统计学上*F*<sub>*j*</sub>又称为自变量，*r*<sub>*i*</sub>为因变量，*ϵ*<sub>*i*, *t*</sub>为残差项。我们建立模型的目标，总是使得自变量与因变量之间的解释性越强越好。就我们这个模型来说，就是要使得残差项越小越好。第一要紧的，就是残差项目的均值要为0，这通过选择适当的*α*<sub>*i*</sub>可以实现，紧接着的就是残差项的高阶动量（moment）。如果残差项服从正态分布，那么我们可以只要求最小化
*E*(*ϵ*<sub>*i*, *t*</sub><sup>2</sup>)
 如果，该残差项是随时间无关的，那么我们的目标进一步为：
$$
E(\\epsilon\_i^2)\\approx \\frac{1}{T}\[\\bar{r}\_i-\\bar{F}\\bar{\\beta}\_i\]^t\[\\bar{r}\_i-\\bar{F}\\bar{\\beta}\_i\]
$$
 此处
$$
\\bar{r}\_i=(r\_{i,1},\\cdots,r\_{i,T})^t \\\\
\\bar{\\beta}\_i=(\\alpha\_i,\\beta\_1,\\cdots,\\beta\_m)^t \\\\
\\bar{F}=\\mbox{行向量}\\bar{F}\_t,t=1\\cdots,T\\mbox{组成的矩阵}\\\\
\\bar{F}\_t=(1,F\_{1,t},\\cdots,F\_{m,t})
$$
 求解该问题，就是典型的二阶优化。由于是无约束的优化问题，取得最优值的参数$\\bar{\\beta}\_i$只需要满足
$$
\\bar{\\beta}\_i =\[\\bar{F}^t\\bar{F}\]^{-1}\\bar{F}^t  \\bar{r}\_i
$$
 这个结果依赖于矩阵$\\bar{F}^t\\bar{F}$可逆。

在残差项不是正态分布的情况，大多数时候最小化一个方差，也不算太坏。当然那些来自于正态分布的漂亮性质就不会再有了。 以下，我们用智度股份（000676）为例，看看该股收益偏向什么类型的风格。

``` r
library(CAsset)
library(dlm)
init()
```

    ## Loading required package: DBI

    ## Loading required package: gsubfn

    ## Loading required package: proto

    ## Loading required package: RSQLite

    ## sqldf will default to using PostgreSQL

    ## Loading required package: xts

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## Loading required package: TTR

    ## Version 0.4-0 included new data defaults. See ?getSymbols.

    ## 
    ## Attaching package: 'PerformanceAnalytics'

    ## The following object is masked from 'package:graphics':
    ## 
    ##     legend

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

    ## Loading required package: nlme

    ## This is mgcv 1.8-17. For overview type 'help("mgcv-package")'.

    ## 
    ## Attaching package: 'plyr'

    ## The following object is masked from 'package:lubridate':
    ## 
    ##     here

    ## Loading required package: foreach

    ## Loading required package: iterators

    ## Loading required package: parallel

``` r
data("stocks")
data("stock_indexes")
data("stocks_weekly")
r=stocks_weekly[,'智度股份']
data("stock_index_weekly")
F=stock_index_weekly[,1:6]
F=merge(r,F,all=FALSE,drop=TRUE)
F=F[rowSums(is.na(F))==0,]
data=as.data.frame(F)
colnames(data)<-c('r','f1','f2','f3','f4','f5','f6')
lm(r~1+f1+f2+f3+f4+f5+f6,data)
```

    ## 
    ## Call:
    ## lm(formula = r ~ 1 + f1 + f2 + f3 + f4 + f5 + f6, data = data)
    ## 
    ## Coefficients:
    ## (Intercept)           f1           f2           f3           f4  
    ##    0.003991     0.434964    -0.923427     1.016872     1.136682  
    ##          f5           f6  
    ##   -0.346668    -0.171909

``` r
r=F[,1]
F[,1]=1
solve(t(F)%*%F)%*%t(F)%*%r
```

    ##                       智度股份
    ## 智度股份           0.003991276
    ## large.cap.value    0.434963895
    ## large.cap.growth  -0.923426756
    ## small.cap.value    1.016871968
    ## small.cap.growth   1.136682297
    ## middle.cap.value  -0.346668120
    ## middle.cap.growth -0.171908557

你可以很清楚的看到，使用lm函数和我们自己按照上边的公式计算的结果是一致的。 那么智度股份是什么属性的股票呢？通常直接比较因子的系数是没有价值的，因为系数必然受到因子的计量单位影响。不过这里由于都是同一市场的股票指数的变化率，因此对照系数的大小正负，可以看出一些有用的信息来。上边的计算中，我们得到的系数有正有负。系数为正，表明跟该因子正相关，反之，负相关。我们选用的6个因子，实际上相互之间不是独立的（大多数实际问题都是如此），这种因子之间的相关性，带来两个问题：一是参数估计的误差放大，极端的例子是，如果因子1就是因子2，那么它们两的参数和是有意义的，但是各自的系数实际上可以取任意值，只要保持和一定；另一个问题是独立的看待某个因子的系数可能不正确。比如智度股份，其小盘的特征很明，最大的系数也是小盘成长，可是小盘价值的系数差得不远。再则所有的成长系数和远远小于所有的价值系数和。究竟智度股份是小盘成长，还是小盘价值，就需要更进一步的分析了。这里的简单的线性模型没有办法说的很清楚了。在准备了足够的知识后，我们会回到这个问题。

非正态分布模型
--------------

残差项目如果不是正态分布，则我们以上使用的最新二乘法以及后续对于系数估计的t检验和F检验都没有了理论基础。比如因变量是一个Bernoulli分布（取0或1的二值分布）。此时坚持使用线性模型，多少有些不合时宜。毕竟线性模型取值范围可以是−∞到+∞。不过作为一个尝试，我们看看用一个线性模型来描述Bernoulli分布的概率应该是可以的。
*p*<sub>*i*</sub> = *α* + *β**x*<sub>*i*</sub>    *i* = 1, ⋯, *T*
 因变量*y*<sub>*i*</sub>, *i* = 1, ⋯, *T*，自变量*x*<sub>*i*</sub>, *i* = 1, ⋯, *T*，这可以看做系统可以被观察到的输出和输入。模型中*α*, *β*是需要估计的系数。金融中有很多相关的问题，比如我们想要根据小盘成长股指数昨日的表现来预测今日智度股份的涨跌。

如何估计模型参数呢？我们可以选择参数*α*和*β*使得输入为*x*<sub>*i*</sub>, *i* = 1, ⋯, *T*时观察到输出*y*<sub>*i*</sub>, *i* = 1, ⋯, *T*的概率最大化。当然，只是一个最大似然估计问题。其对数似然率为：
$$
l(y\_i|\\alpha,\\beta,x\_i)=\\sum\_{i}^T \[y\_ilog(p\_i)+(1-y\_i)log(1-p\_i)  \]
$$
 在无约束情况下，其最大值在一阶导数为零时取得。所以
$$
\\frac{\\partial l(\\alpha,\\beta)}{\\partial \\alpha}=\\sum\_i^T\[\\frac{y\_i}{p\_i}-\\frac{1-y\_i}{1-p\_i}\] \\\\
\\frac{\\partial l(\\alpha,\\beta)}{\\partial \\beta}=\\sum\_i^T\[\\frac{y\_i}{p\_i}-\\frac{1-y\_i}{1-p\_i}\]x\_i
$$
 我们利用R，实现这一想法。

``` r
y=stocks_weekly[,'智度股份']
x=lag(stock_index_weekly[,4])
F=merge(x,y,all=FALSE,drop=TRUE)
F=F[rowSums(is.na(F))==0,]
F[,1]=F[,1]>0
data=as.data.frame(F)

colnames(data)<-c('y','x')
rm(x)
rm(y)
attach(data)
summary(lm(y~1+x,data))
```

    ## 
    ## Call:
    ## lm(formula = y ~ 1 + x, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.7152 -0.5351  0.3838  0.4649  0.5858 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.53508    0.03077  17.392   <2e-16 ***
    ## x            0.80733    0.36380   2.219   0.0273 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4956 on 259 degrees of freedom
    ## Multiple R-squared:  0.01866,    Adjusted R-squared:  0.01487 
    ## F-statistic: 4.925 on 1 and 259 DF,  p-value: 0.02734

``` r
ber<-glm(y~x,family=quasi(variance="mu(1-mu)"),start=c(0.1,0))
summary(ber,dispersion=1)
```

    ## 
    ## Call:
    ## glm(formula = y ~ x, family = quasi(variance = "mu(1-mu)"), start = c(0.1, 
    ##     0))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.5604  -1.2363   0.9931   1.1196   1.3164  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  0.53432    0.03079   17.36   <2e-16 ***
    ## x            0.76032    0.05027   15.13   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for quasi family taken to be 1)
    ## 
    ##     Null deviance: 360.13  on 260  degrees of freedom
    ## Residual deviance: 354.43  on 259  degrees of freedom
    ## AIC: NA
    ## 
    ## Number of Fisher Scoring iterations: 21

第一个使用最小二乘的结果是不可靠的，第二个回归的结果稍好一些。但是也好不到那里去，看看下图。

``` r
plot(as.numeric(x),as.numeric(y))
points(as.numeric(x),0.53432+0.76032*x,col='red')
```

![](linear_regression_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-3-1.png) 首先是，这两组数据本就不太支持我们的模型。其次，模型描述的发生概率，不是因变量自身。

一个可能的改进是把线性函数( − ∞, +∞)的取值空间映射到概率空间\[0, 1\]上。这样做，只需要寻找一个满足一定性质的函数就可以，具体来说： 1. 光滑 2. 单调 这是两个比较好的性质。合在一起可以保证函数的逆函数也很漂亮，这是重要的。

比如：
$$
h\_i = \\alpha + \\beta x\_i \\\\
g(p\_i)=h\_i
$$
 其中*g*(⋅)称为连接函数——将因变量与线性模型的输出连接起来的函数——可能的表达式为：
$$
h\_i=g(p\_i)=\\log(\\frac{p\_i}{1-p\_i})
$$
 该函数的逆函数为：
$$
p\_i = \\frac{1}{1+e^{-h\_i}}
$$
 我们依旧可以使用最大似然估计来建立这个回归模型。以下的R代码使用了glm函数来实现这一过程。

``` r
ber.logit<-glm(y~x,family=binomial())
summary(ber.logit)
```

    ## 
    ## Call:
    ## glm(formula = y ~ x, family = binomial())
    ## 
    ## Deviance Residuals: 
    ##    Min      1Q  Median      3Q     Max  
    ## -1.623  -1.240   0.961   1.116   1.365  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)   0.1456     0.1254   1.161   0.2458  
    ## x             3.8546     1.8220   2.116   0.0344 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 360.13  on 260  degrees of freedom
    ## Residual deviance: 354.79  on 259  degrees of freedom
    ## AIC: 358.79
    ## 
    ## Number of Fisher Scoring iterations: 3

这样的连接函数多种多样，除了已经见到的（称为logit函数），我们还有逆正太累计分布函数，又称为probit函数。

``` r
ber.probit<-glm(y~x,family=binomial(link='probit'))
summary(ber.probit)
```

    ## 
    ## Call:
    ## glm(formula = y ~ x, family = binomial(link = "probit"))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6303  -1.2393   0.9606   1.1167   1.3662  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)  0.09049    0.07828   1.156   0.2477  
    ## x            2.41217    1.08673   2.220   0.0264 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 360.13  on 260  degrees of freedom
    ## Residual deviance: 354.71  on 259  degrees of freedom
    ## AIC: 358.71
    ## 
    ## Number of Fisher Scoring iterations: 4

我们总的来比较一下三个模型的结果

``` r
plot(x,y)
curve(predict(ber, data.frame(x = x)),add=TRUE,lty=1)
curve(predict(ber.logit, data.frame(x = x), type = "response"),add=TRUE,lty=2)
curve(predict(ber.probit, data.frame(x = x), type = "response"),add=TRUE,lty=3)
```

![](linear_regression_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-6-1.png)

动态线性模型
------------

通常动态线性模型包括4个重要的可变参数{*F*<sub>*t*</sub>, *G*<sub>*t*</sub>, *V*<sub>*t*</sub>, *W*<sub>*t*</sub>}： \* 观测约束： *Y*<sub>*t*</sub> = *F*<sub>*t*</sub>*θ*<sub>*t*</sub> + *v*<sub>*t*</sub>，其中*v*<sub>*t*</sub> ∼ *N*(0, *V*<sub>*t*</sub>) \* 系统约束： *θ*<sub>*t*</sub> = *G*<sub>*t*</sub>*θ*<sub>*t* − 1</sub> + *ω*<sub>*t*</sub>，其中*ω*<sub>*t*</sub> ∼ *N*(0, *W*<sub>*t*</sub>) \* 先验概率： *θ*<sub>0</sub> ∼ *N*(*m*<sub>0</sub>, *C*<sub>0</sub>) 其中，误差序列*v*<sub>*t*</sub>与系统游动序列*ω*<sub>*t*</sub>是相互独立的，其各自也是独立的随机序列。系统如果存在任何的自相关性，都应该被*F*<sub>*t*</sub>与*G*<sub>*t*</sub>所蕴含的线性关系表达。当*G*<sub>*t*</sub> = *G*时，系统约束显示出了了“差分不变”+随机噪声的特性，是介于简单线性模型和复杂线性模型之间的桥梁，往往成为我们有力的工具之一。

在文件pmi.csv中记录了中国自2008年以来每月报告的采购经理人指数，数据来自于中国物流与采购联合会网站。对于该数据的分析以及预测，使用一个模型参数为{1, 1, *V*, *W*<sub>*t*</sub>}的动态线性模型是可以的。数学上， \* 观测约束：*P**M**I*<sub>*t*</sub> = *θ*<sub>*t*</sub> + *v*<sub>*t*</sub>，其中*v*<sub>*t*</sub> ∼ *N*(0, *V*<sub>*t*</sub>) \* 系统约束：*θ*<sub>*t*</sub> = *θ*<sub>*t* − 1</sub> + *w*，其中*w* ∼ *N*(0, *W*) 可见*θ*<sub>*t*</sub>实际是PMI指数在t时刻的均值，在短期内我们认为该均值近似不变的，而长期则没有明确方向性的预测。因此，我们使用了一个随机游走过程来对*θ*<sub>*t*</sub>进行建模。当然不同的时期PMI的波动性不一样，这可能更多的来自于受调查对象的主观情绪，所以我们保持了*V*<sub>*t*</sub>的时变性。

``` r
library(purrr)
```

    ## 
    ## Attaching package: 'purrr'

    ## The following objects are masked from 'package:foreach':
    ## 
    ##     accumulate, when

    ## The following object is masked from 'package:plyr':
    ## 
    ##     compact

    ## The following object is masked from 'package:scales':
    ## 
    ##     discard

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:plyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## The following object is masked from 'package:nlme':
    ## 
    ##     collapse

    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     intersect, setdiff, union

    ## The following objects are masked from 'package:xts':
    ## 
    ##     first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
p=read.csv('./data/pmi.csv',header=FALSE)
ad.year<-function(x){as.yearmon(paste(substr(as.character(x),1,4),substr(as.character(x),6,7),sep='-'))}
p$V1<-unlist(p$V1 %>% map(ad.year))
p=arrange(p,V1)
myPMI<-function(b)
{
  m<-dlmModReg(p$V2,FALSE,dW=b[1]^2,m0=50)
  m$JFF<-matrix(0,nr=1,nc=1)
  return(m)
}
fit<-dlmMLE(p$V2,parm=0.1,build=myPMI)
mod<-myPMI(fit$par)
filted<-dlmFilter(p$V2,mod)
plot(cbind(p$V1,filted$m[-1]),col=c('black','red'),xlab='日期',ylab='制造业PMI')
```

![](linear_regression_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-1.png) 这显然出现了过分拟合的情况，也就是对于PMI实际上不需要那么复杂的模型。模型的自由度太多，以至于根本不是拟合。降低维度，将*V*<sub>*t*</sub>确定为*V*。

``` r
myPMI2<-function(b)
{
  m<-dlmModReg(p$V2,FALSE,dW=b[1]^2,dV=b[2]^2,m0=50)
  m$JFF<-matrix(0,nr=1,nc=1)
  return(m)
}
fit<-dlmMLE(p$V2,parm=c(0.1,0.1),build=myPMI2)
mod<-myPMI(fit$par)
filted<-dlmFilter(p$V2,mod)
plot(cbind(p$V1,filted$m[-1]),col=c('black','red'),xlab='日期',ylab='制造业PMI')
```

![](linear_regression_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-1.png)

``` r
require(dlm)
hs300=getReturnFromDB('000300',from='2000-01-06',frequency='weekly')
```

    ## Loading required package: tcltk

``` r
r_f=getRiskfree(from='2005-04-01',frequency='weekly')/52/100
y=stocks_weekly[,'智度股份']
x=hs300-r_f
y=y-r_f
F=merge(x,y,all=FALSE,drop=TRUE)
F=F[rowSums(is.na(F))==0,]
data=as.data.frame(F)
colnames(data)<-c('x','y')
rm(x)
rm(y)
attach(data)
```

    ## The following objects are masked from data (pos = 6):
    ## 
    ##     x, y

``` r
## faked data
##x=rnorm(200)
##y=0.2+4*x+0.1*rnorm(200)
mymod<-function(b)
{
  m<-dlmModReg(x, TRUE, dV = b[1]^2,dW=c(b[2]^2,b[3]^2),
                          m0 = c(0,1))
  m$JFF<-matrix(c(0,1),nr=1,nc=2)
  return(m)
}
fit <- dlmMLE(y, parm = c(0.1, 0.01,0.02),
              build = mymod)
mod<-mymod(fit$par)
filted<-dlmFilter(y,mod)
```
