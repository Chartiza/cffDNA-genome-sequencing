##покрытие и вейвлет
# NP.F.10  NP.F.40  NP.M.15  Pr.Nr.1  Pr.Nr.2  Pr.Nr.3  Pr.T21.NA1  Pr.T21.True
# clusters: GoodDHS  HeCh21DHS
############################
rm(list=ls())
#нужные пакеты
library(WaveletComp) 
library(TTR) #для скользящего среднего(SMA)

results<-cbind(rep(0,19), rep(0,19), rep(0,19), rep(0,19), rep(0,19))
colnames(results)<-c('smpls', 'Period.max1', 'Power.max1', 'Period.max2', 'Power.max2')
i<-1

#читаем файлы
files<-list.files(path = "data/", pattern = NULL)
smpls<-"GoodDHS_NP.F.40.txt"

for (smpls in files[1:19]) {
  #ЧИТАЕМ ДАННЫЕ
  data<-read.table(paste('data/',smpls, sep = ""), header = T)
  #пишем покрытие в файл
  jpeg(paste('results/Scr1.Cov_', smpls, '.jpg', sep=''), width = 900, height = 500) 
  
  plot(data[8000:12000,2],type='l', xlab='DHS region', ylab='cov', main= smpls)
  pad=round(1-sum(data[9900:10100,2])/sum(data[8000:8200,2]),2)
  legend('bottomright', legend = pad, bty='n')
  abline(v = c(0, 200, 1900, 2100), col='grey')
  dev.off()
  
  #пишем картинку вейвлета в файл
  my.a = data.frame(x = data[8000:12000,2])
  my.wa = analyze.wavelet(my.a, "x", loess.span = 1, dt = 1, dj = 1/250,lowerPeriod = 50, 
                          upperPeriod = 515, make.pval = T, n.sim = 10)
  #jpeg(paste('results/Scr1.Wavelet_', smpls, '.jpg', sep=''), width = 900, height = 500)
  wt.image(my.wa, color.key = "quantile", n.levels = 250, main=smpls,
           legend.params = list(lab = "wavelet power levels", mar = 4.7))
  dev.off()
  
  #рисуем силу колебаний
  pw<-(my.wa$Power.avg)
  per<-(my.wa$Period)
  jpeg(paste('results/Scr1.Power_', smpls, '.jpg', sep=''), width = 900, height = 500)
  
  plot(per,pw, main=smpls, xlab='period', ylab='power')
  m1pw<-which.max(pw)
  max1<-per[m1pw]
  abline(v=max1)
  m2pw<-which.max(pw[600:800])+600
  max2<-per[m2pw]
  abline(v=max2)
  n1<-paste('Period max1:', round(max1,0))
  n2<-paste('Power max1:', round(pw[m1pw],2))
  n3<-paste('Period max2:', round(max2,0))
  n4<-paste('Power max2:', round(pw[m2pw],2))
  legend('topright', legend = c(n1,n2,n3,n4), bty='n')
  dev.off()
  
  #значения в файл
  results[i,1]<-smpls
  results[i,2]<-round(max1,0)
  results[i,3]<-round(pw[m1pw],2)
  results[i,4]<-round(max2,0)
  results[i,5]<-round(pw[m2pw],2)
  i<-i+1
}

jpeg(paste('results/Scr1.PlotPower','.jpg', sep=''), width = 300, height = 300)

plot(results[1:9,3], ylim = c(0,1.2), col='blue', xaxt='n', xlab = '', 
     pch=c(19,19,19, 24,24,24, 13, 13,13), ylab = 'power') 
points(results[1:9,5], col='red', pch=c(19,19,19, 24,24,24, 13, 13,13))
axis(1, at=c(1:9), labels=c('NP.F.10', 'NP.F.40', 'NP.M.15',
                            'Pr.Nr.1', 'Pr.Nr.2', 'Pr.Nr.3',
                            'Pr.T21.NA1', 'Pr.T21.NA2', 'Pr.T21.True'), las=2)
dev.off()

boxplot(as.numeric(results[1:3,3]), as.numeric(results[4:6,3]),
        as.numeric(results[7:9,3]), xaxty='n', main='170 period')
axis(1,at=c(1,2,3), labels = c('NP', 'Pr.N', 'Pr.T21'))

boxplot(as.numeric(results[1:3,5]), as.numeric(results[4:6,5]),
        as.numeric(results[7:9,5]), xaxty='n', main='425 period')
axis(1,at=c(1,2,3), labels = c('NP', 'Pr.N', 'Pr.T21'))