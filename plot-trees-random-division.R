# niezbedne biblioteki i ustawienia
library("rpart.plot")
library(rpart)
options(na.action="na.omit")
options(digits=4) # to match earlier output
set.seed(1234)
# import zbioru danych
life_expectancy <- read.csv(file = 'LifeExpectancyData.csv')
names(life_expectancy) <- c("country","year","status","life_expectancy","adult_mortality","infant_deaths","alcohol","expenditure","hepatitis_b","measles","bmi","under_five_deaths","polio","total_expenditure","diphtheria","hiv","gdp","population","thinness1","thinness2","incomes_resources","schooling")
# funkcja oceny - wywolywana 1 raz na kazdy wezel
temp1 <- function(y, wt, parms) {
  wmean <- sum(y*wt)/sum(wt)
  rss <- sum(wt*(y-wmean)^2)
  list(label= wmean, deviance=rss)
}
# funkcja split - będzie wywoływana raz dla każdej współzmiennej przy każdym potencjalnym podziale na podstawie losowania podzbioru atrybutow
temp2 <- function(y, wt, x, parms, continuous) {
  n <- length(y)
  y <- y- sum(y*wt)/sum(wt)
  if (continuous) {
    # dla zmiennych ciaglych
    temp <- cumsum(y*wt)[-n]
    left.wt  <- cumsum(wt)[-n]
    right.wt <- sum(wt) - left.wt
    lmean <- temp/left.wt
    rmean <- -temp/right.wt
    goodness <- (left.wt*lmean^2 + right.wt*rmean^2)/sum(wt*y^2)
    #print(typeof(goodness))
    #print(goodness)
    f <- runif(n = 1, min = 0.0000001, max = 0.0010000)
    f <- runif(n = 1, min = 1, max = 10000)
    goodness <- runif(n = n-1, min = f+0.000010, max = f+0.010000)
    #print(typeof(goodness))
    #print(goodness)
    list(goodness= goodness, direction=sign(lmean))
  }
  else {
    # dla zmiennych dyskretnych
    ux <- sort(unique(x))
    wtsum <- tapply(wt, x, sum)
    ysum  <- tapply(y*wt, x, sum)
    means <- ysum/wtsum
    
    ord <- order(means)
    n <- length(ord)
    temp <- cumsum(ysum[ord])[-n]
    left.wt  <- cumsum(wtsum[ord])[-n]
    right.wt <- sum(wt) - left.wt
    lmean <- temp/left.wt
    rmean <- -temp/right.wt
    list(goodness= (left.wt*lmean^2 + right.wt*rmean^2)/sum(wt*y^2),direction = ux[ord])
  }
}
# funkcja inicjalizacji
temp3 <- function(y, offset, parms, wt) {
  if (!is.null(offset)) y <- y-offset
  list(y=y, parms=0, numresp=1, numy=1,
       summary= function(yval, dev, wt, ylevel, digits ) {
         paste("  mean=", format(signif(yval, digits)),
               ", MSE=" , format(signif(dev/wt, digits)),
               sep='')
       })
}


#zdefiniowanie metody alist i listy funkcji dla budowania drzewa decyzyjnego 
alist <- list(eval=temp1, split=temp2, init=temp3)

fit2 <- rpart(life_expectancy ~alcohol +gdp + schooling,
              life_expectancy, control=rpart.control(minsplit=10, xval=0),
              method=alist)
rpart.plot(fit2)

#zdefiniowanie metody alist i listy funkcji dla budowania drzewa decyzyjnego 
alist <- list(eval=temp1, split=temp2, init=temp3)

fit2 <- rpart(life_expectancy ~alcohol +gdp + schooling,
              life_expectancy, control=rpart.control(minsplit=10, xval=0),
              method=alist)
rpart.plot(fit2)

#zdefiniowanie metody alist i listy funkcji dla budowania drzewa decyzyjnego 
alist <- list(eval=temp1, split=temp2, init=temp3)

fit2 <- rpart(life_expectancy ~alcohol +gdp + schooling,
              life_expectancy, control=rpart.control(minsplit=10, xval=0),
              method=alist)
rpart.plot(fit2)















