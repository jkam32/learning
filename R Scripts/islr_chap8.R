# require R version 3.6.0 and above
library(tree)
library(ISLR)

df <- Carseats

High <- ifelse(df$Sales <= 8, "No", "Yes")
df <- data.frame(df, High)

# classification tree
tree_df <- tree(High ~ . - Sales, data = df)

