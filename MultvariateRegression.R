# Load data
df <- read.csv(file.choose())
# summary(df)
head(df)
class(df)

#############################################
# Correlations
# create matrix without date for correlation
df.nodate <- subset( df, select = - DATE )

# correlation matrix

df.cor = cor(df.nodate)
df.cor

# Visualizing the correlation matrix

library(corrplot)
corrplot(df.cor)

# heatmap
palette = colorRampPalette(c("red", "white", "green")) (20)
heatmap(x = df.cor, col = palette, symm = TRUE)


#######################################################
# Multiple Linear Regression
fit <- lm(NPU ~ Apple + Bing + Facebook + Google_Brand 
          + Google_Performance + Influencer + Pinterest 
          + Snapchat + Appstore + Organic, data=df)

summary(fit) # show results


# predicted values
data.prediction <- predict(fit)

# plot prediction
plot(data.prediction, type = "l")

# plot actual values
plot(df$NPU, type = "l")

# plot both
plot(data.prediction,type="l",col="red")
lines(df$NPU,col="blue")

########################################################
# Other useful functions
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table
vcov(fit) # covariance matrix for model parameters
influence(fit) # regression diagnostics

# diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit)

# compare models
# fit1 <- lm(sales ~ facebook + youtube + newspaper, data=df)
# fit2 <- lm(sales ~ facebook + youtube, data=df)
# anova(fit1, fit2)
