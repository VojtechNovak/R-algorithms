t = c(1,2,3)
y = c(2,3,3)
y= as.integer(y)
t = as.integer(t)

plot(t,y,xlim=c(1, 3), ylim=c(0, 3))  
scale_x_continuous(breaks = 1:3)


plot(t , y , xaxt = "n",yaxt = "n",xlim=c(0, 3.5), ylim=c(0, 3.5))
axis(side = 1, at = seq(0, 3, 1))
axis(side = 2, at = seq(0, 3, 1))


fit <- lm(y ~ x)

abline(fit, col = "blue")

# Calculate residuals
resid <- y - predict(fit)

p = predict(fit)
# Plot the residuals in red
segments(x, y, x, y - resid, col = "red")

text(x+0.3, y - resid, labels = paste0("e", 1:3))

points(x, p, col='red')
text(x+0.3, p, labels = paste0("p", 1:3))
