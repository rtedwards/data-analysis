library(ggplot2)
library(dplyr)
library(moderndive)
library(skimr)
library(tidyr)
library(kableExtra)
library(gridExtra)
library(plotly)
library(janitor)
library(infer)
library(broom)

insurance<-read.csv("insurance.csv")
insur<-insurance%>%
  filter(region=="southeast")%>%
  select(charges,age,bmi,smoker)

insurance%>%
  select(charges,age,bmi)%>%
  cor()
ggplot(insurance, aes(x = age, y=charges, color = smoker)) +
  geom_point() 
ggplot(insurance, aes(x = bmi, y=charges, color = smoker)) +
  geom_point() 



#full model

f.model<-lm(charges~bmi*age*smoker,data = insur)
get_regression_table(f.model) %>%
  kable(digits = 3, booktabs = TRUE, format = "latex") %>%
  kable_styling(font_size = 10, latex_options = 'HOLD_position')

#model1

model1<-lm(charges~bmi+age+smoker+bmi*age+bmi*smoker+age*smoker,data = insur)
get_regression_table(model1) %>%
  kable(digits = 3, booktabs = TRUE, format = "latex") %>%
  kable_styling(font_size = 10, latex_options = 'HOLD_position')

#model2
model2<-lm(charges~bmi+age+smoker+bmi*age+bmi*smoker,data = insur)
get_regression_table(model2) %>%
  kable(digits = 3, booktabs = TRUE, format = "latex") %>%
  kable_styling(font_size = 10, latex_options = 'HOLD_position')

#model3
model3<-lm(charges~bmi+age+smoker+bmi*smoker,data = insur)
get_regression_table(model3) %>%
  kable(digits = 3, booktabs = TRUE, format = "latex") %>%
  kable_styling(font_size = 10, latex_options = 'HOLD_position')

#model4
model4<-lm(charges~age+smoker,data = insur)
get_regression_table(model4) %>%
  kable(digits = 3, booktabs = TRUE, format = "latex") %>%
  kable_styling(font_size = 10, latex_options = 'HOLD_position')

#model selection

model.comp.values.full<-glance(lm(charges~bmi*age*smoker,data=insur))
model.comp.values.model1<-glance(lm(charges~bmi+age+smoker+bmi*age+bmi*smoker+age*smoker,data=insur))
model.comp.values.model2<-glance(lm(charges~bmi+age+smoker+bmi*age+bmi*smoker,data=insur))
model.comp.values.model3<-glance(lm(charges~bmi+age+smoker+bmi*smoker,data=insur))
model.comp.values.model4<-glance(lm(charges~age+smoker,data=insur))

Mdels<-c('f.model','model1','model2','model3','model4')
bind_rows(model.comp.values.full,model.comp.values.m1,model.comp.values.m2,model.comp.values.m3,model.comp.values.m4)%>%
  select(Model,adj.r.squared,AIC,BIC) %>%
  kable(digits = 3, booktabs = TRUE, format = "latex") %>%
  kable_styling(font_size = 10, latex_options = 'HOLD_position')








