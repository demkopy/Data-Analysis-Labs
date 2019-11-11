library(moments)
library(MASS)

data = read.csv("sof.csv");
temp = data$temperature

temp.iqr = IQR(temp)
temp.med = median(temp);

temp = temp[temp> (temp.med - 1.5 * temp.iqr) & temp < (temp.med + 1.5 * temp.iqr) ]

getmode = function(x){
  uniqv = unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}

temp.mean = mean(temp);
temp.mode = getmode(temp);
temp.gm_mean = prod(temp ^ (1 / length(temp)));
temp.hm_mean = 1/mean(1/temp);
temp.var = var(temp);
temp.sd = sd(temp);
temp.kvar = temp.sd / temp.mean
temp.range = diff(range(temp))

temp.1st_qu = quantile(temp, c(0.25), names=FALSE)
temp.3st_qu = quantile(temp, c(0.75), names=FALSE)
temp.min = min(temp)
temp.max = max(temp)

b = boxplot(temp, 
        main = "Temperature distribution in Sofia in April of 2016", 
        xlab = "Degrees in Celsius", 
        ylab = "Temperature", 
        notch = TRUE, 
        col = "greenyellow", 
        border = "green4",
        horizontal = TRUE,
        outline = FALSE)


axis(1, 
     at = b$stats, 
     labels = b$stats, 
     col.ticks = "blue", 
     col.axis = "blue", 
     tck = 0.05, 
     cex.axis = 0.7)



# print(paste(format("Minimal:", width=15), temp.min))
# print(paste(format("1st Quartile:", width=15), temp.1st_qu))
# print(paste(format("Median:", width=15), temp.med))
# print(paste(format("3nd Quartile:", width=15), temp.3st_qu))
# print(paste(format("Maximal:", width=15), temp.max))
print(summary(temp)[c(1,2,3,5,6)])

temp.1st_dec = quantile(temp, c(0.1), names=FALSE)
temp.9th_dec = quantile(temp, c(0.9), names=FALSE)

temp.kurt = kurtosis(temp) - 3
temp.skew = skewness(temp)


hist(temp, breaks = "Scott", prob=T, main = "Temperature in Sophia (Scot hist)", xlab="Temperature(Celsius)")
hist(temp, breaks = "FD", prob=T, main = "Temperature in Sophia (Freedman-Diaconis)", xlab="Temperature(Celsius)")
hist(temp, breaks = "Sturges", prob=T, main = "Temperature in Sophia (Sturges)", xlab="Temperature(Celsius)")

fit = fitdistr(temp, densfun = "lognormal")

curve(dlnorm(x, meanlog = fit$estimate[1], sdlog = fit$estimate[2]), 
      from = temp.min,
      to = temp.max,
      add=T,
      col="red")

temp.cdf = function(x){
  length(which(temp < x))/length(temp)
}

temp.cdf_v = function(x){
  v = c()
  for (i in 1:length(x)){
    v[i] = temp.cdf(x[i])}
  v
}

#p-p plot
plot(temp.cdf_v(seq(temp.min, temp.max, 0.2)), 
     plnorm(seq(temp.min, temp.max, 0.2), 
            meanlog = fit$estimate[1], 
            sdlog = fit$estimate[2]),
     main = "P-P plot",
     ylab="Theoratical lognormal distribution",
     xlab="Emperical distribution")


curve((x), from = 0, to = 1, col="red", add=T)


temp.qu = function(q){
  quantile(temp, q, names = FALSE)
}

temp.qu_v = function(q){
  v = c()
  for (i in 1:length(q)){
    v[i] = temp.qu(q[i])
  }
  v
}

#q-q plot
plot(temp.qu_v(seq(0, 1, 0.01)), 
     qlnorm(seq(0,1,0.01), 
            meanlog=fit$estimate[1], 
            sdlog=fit$estimate[2]),
     main = "Q-Q plot",
     ylab="Theoratical lognormal distribution",
     xlab="Emperical distribution")

curve((x), from = temp.min, to = temp.max, col="red", add=T)
