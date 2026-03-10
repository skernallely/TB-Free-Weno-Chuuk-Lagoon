#imputing BMIs for adults

pacman::p_load(
  VIM,
  tidyverse,
  mice,
  DataExplorer,
  mitools,
  Publish
)

#get adults
screened_adults <- read_excel("Data/tbfc_analysis_dataset.xlsx",
                             guess_max = 20000, col_names = TRUE) %>%
  filter(screened_at_clinic == 1 & age >= 18) |>
  select(a1c,history_diabetes,abnormal_xray,prior_tb,current_smoker,bmi,height,weight,
         village,municipality,region, sex,age)

md.pattern(screened_adults) 

marginplot(screened_adults[, c("a1c", "bmi")])
marginplot(screened_adults[, c("age", "bmi")])
marginplot(screened_adults[, c("municipality", "bmi")])


#imputing with mi

imp <- mice(screened_adults, seed = 504, m = 100, print = FALSE)
lapply(imp,mean)

Stack.data <- mice::complete(imp, action="long")
fitx <- lm(bmi ~ a1c+height+weight+sex+age, data = Stack.data)
scope0 <- list(upper = ~ a1c+height+weight+sex+age, lower = ~1)
fity <- step(fitx, scope = scope0, trace = FALSE)
require(Publish)
publish(fity)


table1(~ bmi,
       render.continuous = render.NEW,
       render.categorical = \(x)  c("", sapply(stats.apply.rounding(stats.default(x)), 
                                               function(y) with(y,sprintf("%s (%s%%)", prettyNum(FREQ, big.mark=","), PCT)))), 
       overall=c(left="Total"),
       data=Stack.data)


#MCAR from the analysis, no changes made.