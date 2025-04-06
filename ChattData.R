# Daniel Carr
# 4/2/2025

# Analysis of the Chatt foundation meal data.

rm(list = ls())

df <- read.csv(file = 'C:/Users/Daniel M Carr/Documents/Chatt Foundation/MealDataAnalysis.csv')
colnames(df) <- gsub(pattern = '\\.', replacement = '', x = colnames(df))
df$t <- 1:nrow(df)

# df$date <- as.Date(paste0(df$year, '-', df$month, '-', df$day))

# ---- Significance testing ----
# Significance of yesterday
# Testing weekday significance
# wkdayBfast <- lm(formula = Breakfast ~ isSun + isMon + isTue + isWed + isThu + isFri, data = df)
# summary(wkdayBfast)
# 
# wkdayLunch <- lm(formula = Lunch ~ isSun + isMon + isTue + isWed + isThu + isFri, data = df)
# summary(wkdayLunch)
# 
# wkdayDinner <- lm(formula = Dinner ~ isSun + isMon + isTue + isWed + isThu + isFri, data = df)
# summary(wkdayDinner)


# Testing month significance
# mthBfast <- lm(formula = Breakfast ~ isJan + isFeb + isMar + isApr + isMay + isJun + isJul + isAug + isSep + isOct + isNov, data = df)
# summary(mthBfast)
# 
# mthLunch <- lm(formula = Lunch ~ isJan + isFeb + isMar + isApr + isMay + isJun + isJul + isAug + isSep + isOct + isNov, data = df)
# summary(mthLunch)
# 
# mthDinner <- lm(formula = Dinner ~ isJan + isFeb + isMar + isApr + isMay + isJun + isJul + isAug + isSep + isOct + isNov, data = df)
# summary(mthDinner)


# testing time significance
# tBfast <- lm(formula = Breakfast ~ t, data = df)
# summary(tBfast)
# 
# tLunch <- lm(formula = Lunch ~ t, data = df)
# summary(tLunch)
# 
# tDinner <- lm(formula = Dinner ~ t, data = df)
# summary(tDinner)


# Testing day-of-week significance
# dayBfast <- lm(formula = Breakfast ~ isWeek1 + isWeek2 + isWeek3, data = df)
# summary(dayBfast)
# 
# dayLunch <- lm(formula = Lunch ~ isWeek1 + isWeek2 + isWeek3, data = df)
# summary(dayLunch)
# 
# dayDinner <- lm(formula = Dinner ~ isWeek1 + isWeek2 + isWeek3, data = df)
# summary(dayDinner)

# Meat and potatoes ----

strFormula1 <- as.formula(paste0('Breakfast ~ ', paste(colnames(df)[13:ncol(df)], collapse = ' + ')))
strFormula2 <- as.formula(paste0('Lunch ~ ', paste(colnames(df)[13:ncol(df)], collapse = ' + ')))
strFormula3 <- as.formula(paste0('Dinner ~ ', paste(colnames(df)[13:ncol(df)], collapse = ' + ')))

regBfast <- lm(formula = strFormula1, data = df[df$isXmas == 0 & df$isTgiving == 0,])
regLunch <- lm(formula = strFormula2, data = df[df$isXmas == 0 & df$isTgiving == 0,])
regDinner <- lm(formula = strFormula3, data = df[df$isXmas == 0 & df$isTgiving == 0,])

# hist(residuals(regBfast))
# hist(residuals(regLunch))
# hist(residuals(regDinner))
# 
# plot(fitted(regBfast), residuals(regBfast))
# plot(fitted(regLunch), residuals(regLunch))
# plot(fitted(regDinner), residuals(regDinner))

summary(regBfast)
summary(regLunch)
summary(regDinner)

# Looking deeper at residuals ----
# Taking a look at the residuals of Breakfast
dfB <- df
dfB <- dfB[!is.na(dfB$Breakfast),!(colnames(dfB) %in% c('Lunch', 'LunchPlus', 'TotalLunch', 'Dinner', 'DinnerPlus', 'TotalDinner'))]
dfB$resid <- residuals(regBfast)
dfB$resid2 <- (residuals(regBfast))^2


# Now let's take a look at lunch
dfL <- df
dfL <- dfL[!is.na(dfL$Lunch),!(colnames(dfL) %in% c('Breakfast', 'Bfastplus', 'TotalBreakfast', 'Dinner', 'DinnerPlus', 'TotalDinner'))]
dfL$resid <- residuals(regLunch)
dfL$resid2 <- (residuals(regLunch))^2
dfL$fit <- fitted(regLunch)


# Finally Dinner
dfD <- df
dfD <- dfD[!is.na(dfD$Dinner) & df$isXmas == 0 & df$isTgiving == 0,!(colnames(dfD) %in% c('Breakfast', 'Bfastplus', 'TotalBreakfast', 'Lunch', 'LunchPlus', 'TotalLunch'))]
dfD$resid <- residuals(regDinner)
dfD$resid2 <- (residuals(regDinner))^2
dfD$fit <- fitted(regDinner)

regBfast$coefficients
sd(regBfast$residuals)

