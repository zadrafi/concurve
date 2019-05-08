# install the package
install.packages("concurve")
library(concurve)

# load data
data()
(orange<-datasets::Orange)
View(orange)

library(ggplot2)

ggplot(data=orange, aes(x=age, y=circumference)) +
  geom_point(color="#f38181") +
  theme_light() +
  labs(x="Age)",
       y="Circumference")

(model<-lm(circumference ~ age, data=orange))

summary(model)

confint(model, "age", 0.95)

ggplot(data=orange, aes(x=age, y=circumference)) +
  geom_point(color="#f38181") +
  geom_smooth(method="lm", se=TRUE, color="#b0dedb", size=1.3)+
  theme_light() +
  labs(x="Age)",
       y="Circumference")

curvedf<-genintervals(model, "age", steps=1000)

plotp<-plotpint(curvedf)
plotp

plots<-plotsint(curvedf)
plots
