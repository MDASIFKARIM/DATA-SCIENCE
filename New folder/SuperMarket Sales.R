data <- read.csv("D:/ds f/sales.csv")

data

data$branch <- as.factor(data$branch)
data$branch
data$city <- as.factor(data$city)
data$city
data$customer_type <- as.factor(data$customer_type)
data$customer_type
data$gender <- as.factor(data$gender)
data$gender
data$product_name <- as.factor(data$product_name)
data$product_name
data$product_category <- as.factor(data$product_category)
data$product_category

summary(data)

sapply(data, function(x) sum(is.na(x)))



correlation_matrix <- cor(data[, c("total_price", "unit_price", "quantity", "tax", "reward_points")])

print(correlation_matrix)


chi_branch_city <- chisq.test(table(data$branch, data$city))
print("Chi-Squared Test for Branch and City:")
print(chi_branch_city)


chi_branch_customer_type <- chisq.test(table(data$branch, data$customer_type))
print("Chi-Squared Test for Branch and Customer Type:")
print(chi_branch_customer_type)


chi_branch_gender <- chisq.test(table(data$branch, data$gender))
print("Chi-Squared Test for Branch and Gender:")
print(chi_branch_gender)


chi_branch_product_name <- chisq.test(table(data$branch, data$product_name))
print("Chi-Squared Test for Branch and Product Name:")
print(chi_branch_product_name)


chi_branch_product_category <- chisq.test(table(data$branch, data$product_category))
print("Chi-Squared Test for Branch and Product Category:")
print(chi_branch_product_category)


chi_city_customer_type <- chisq.test(table(data$city, data$customer_type))
print("Chi-Squared Test for City and Customer Type:")
print(chi_city_customer_type)


chi_city_gender <- chisq.test(table(data$city, data$gender))
print("Chi-Squared Test for City and Gender:")
print(chi_city_gender)


chi_city_product_name <- chisq.test(table(data$city, data$product_name))
print("Chi-Squared Test for City and Product Name:")
print(chi_city_product_name)


chi_city_product_category <- chisq.test(table(data$city, data$product_category))
print("Chi-Squared Test for City and Product Category:")
print(chi_city_product_category)


chi_customer_type_gender <- chisq.test(table(data$customer_type, data$gender))
print("Chi-Squared Test for Customer Type and Gender:")
print(chi_customer_type_gender)


chi_customer_type_product_name <- chisq.test(table(data$customer_type, data$product_name))
print("Chi-Squared Test for Customer Type and Product Name:")
print(chi_customer_type_product_name)


chi_customer_type_product_category <- chisq.test(table(data$customer_type, data$product_category))
print("Chi-Squared Test for Customer Type and Product Category:")
print(chi_customer_type_product_category)


chi_gender_product_name <- chisq.test(table(data$gender, data$product_name))
print("Chi-Squared Test for Gender and Product Name:")
print(chi_gender_product_name)


chi_gender_product_category <- chisq.test(table(data$gender, data$product_category))
print("Chi-Squared Test for Gender and Product Category:")
print(chi_gender_product_category)


chi_product_name_product_category <- chisq.test(table(data$product_name, data$product_category))
print("Chi-Squared Test for Product Name and Product Category:")
print(chi_product_name_product_category)






library(infotheo)

mutual_information <- mutinformation(discretize(data$tax), discretize(data$total_price))
mutual_information

mutual_information <- mutinformation(discretize(data$quantity), discretize(data$total_price))
mutual_information

mutual_information <- mutinformation(discretize(data$reward_points), discretize(data$total_price))
mutual_information

mutual_information <- mutinformation(discretize(data$product_category), discretize(data$product_name))
mutual_information

