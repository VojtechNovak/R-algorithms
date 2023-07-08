## ---- myrcode4
library(rpart)
library(ggplot2)
# Simulace dat
set.seed(123)
n <- 100
x <- runif(n, min = 0, max = 1)
y <- 10*sin(4*pi*x) + rnorm(n, mean = 0, sd = 0.7)

# 1. Inicializace predikce jako průměr, počet stromů a learning rate
f <- rep(mean(y), n); M <- 200; learning_rate <- 0.1

# Matice pro predikce a vektor pro chybu
predictions <- matrix(0, nrow = n, ncol = M); train_errors <- rep(0, M)

# Boosting model
for (m in 1:M) {
  # 2. Rezidua
  residuals <- y - f
  
  # 3. Strom – (CART) je vytvořen na základě reziduí
  tree <- rpart(residuals ~ x, method = "anova", 
                control = rpart.control(maxdepth = 1))
  
  # 4 Predikce stromu
  prediction <- predict(tree, newdata = data.frame(x = x))
  predictions[, m] <- prediction
  
  # Kombinace predikce stromů 
  f <- f + learning_rate * prediction
  
  # 5. Kvadratická cenová funkce
  train_errors[m] <- mean((y - f)^2)
  
}
