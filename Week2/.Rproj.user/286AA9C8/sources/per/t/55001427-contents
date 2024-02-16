ggplot(iris,aes(Sepal.Length, Sepal.Width))+
  geom_point()+ 
  facet_wrap(~Species) +
  stat_smooth(method="lm", se=FALSE) +
  coord_cartesian(xlim = c(4,8), ylim = c(2, 5))+
  theme(axis.text = element_text(color = "red", size=14))
