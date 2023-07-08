x <- seq(0,2*pi,pi/500)
f.0 = 1/(2*pi)
w <- 2*pi*f.0

f = sin(5*w*x) + 2*sin(3*w*x)+ 0.5*sin(12*w*x) + 20*sin(30*w*x)
#f= sin(x)
plot(x,f)

ft = fft(f)

plot.data  <- cbind(0:(length(ft)-1), Mod(ft))

plot(plot.data, xlim=c(0,50), t="h", lwd=2)

plot(Arg(ft),xlim=c(0,15))

plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
  plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
  
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
  
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Hz)", ylab="Strength", 
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}

plot.frequency.spectrum(ft, xlimits=c(0,20))


