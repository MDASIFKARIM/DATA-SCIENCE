data <- read.csv("D:/ds f/sales.csv")
data



hist(data$total_price, 
     main = "Histogram of Total Price", 
     xlab = "Total Price", 
     ylab = "Frequency", 
     col = "skyblue", 
     border = "black", 
     probability = TRUE)

mean_val <- mean(data$total_price, na.rm = TRUE)
sd_val <- sd(data$total_price, na.rm = TRUE)

curve(dnorm(x, mean = mean_val, sd = sd_val), 
      col = "orange", 
      lwd = 2,  
      add = TRUE)

library(e1071)

skew_value <- skewness(data$total_price, na.rm = TRUE)
cat("Skewness of Total Price: ", skew_value, "\n")

if (skew_value > 0) {
  cat("The distribution is positively skewed (right skewed).\n")
} else if (skew_value < 0) {
  cat("The distribution is negatively skewed (left skewed).\n")
}

boxplot(data$total_price,
        main = "Box Plot of Total Price",
        ylab = "Total Price",
        col = "lightgreen",
        border = "black",
        horizontal = FALSE)

outliers <- boxplot.stats(data$total_price)$out
cat("Outliers in Total Price: ", outliers, "\n")




plot(data$quantity, data$total_price, 
     main="Scatterplot of Quantity vs Total Price", 
     xlab="Quantity", 
     ylab="Total Price", 
     pch=16, 
     col="black")




plot(data$quantity, data$total_price, 
     type = "l",  
     main = "Line Graph of Total Price and Quantity", 
     xlab = "Quantity", 
     ylab = "Total Price", 
     col = "blue", 
     lwd = 2)




ggplot(data, aes(x = factor(1), y = quantity)) +
  geom_violin(trim = FALSE, fill = "skyblue", color = "black") +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Violin Plot of Quantity",
       x = "Quantity",
       y = "Value") +
  theme_minimal()

ggplot(data, aes(x = factor(1), y = total_price)) +
  geom_violin(trim = FALSE, fill = "lightgreen", color = "black") +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Violin Plot of Total Price",
       x = "Total Price",
       y = "Value") +
  theme_minimal()

