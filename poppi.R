x <- readxl::read_xlsx("Poppi_results.xlsx")
head(x)
x$duration=as.numeric((x$t2-x$t1)/60)
x$duration[x$duration <= 0]=NA
x=x[,c(-1,-11)]


questions='The sale of Ecstasy should be legalised (pre)'
questions[2]='I find the issue of the regulation of Ec-
stasy important (pre)'
questions[3]='Permitted with a medical subscription (pre)'
questions[4]='Permitted in a pharmacy (pre)'
questions[5]='Permitted in a smart shop (pre)'
questions[6]='Permitted in bars/clubs (pre)'
questions[7]='Permitted for people of legal age (pre)'
questions[8]='Permitted with a valid ID (pre)'
questions[9]='and marketing can be permitted (pre)'
questions[10]='The sale of Ecstasy should be legalised (post)'
questions[11]='I find the issue of the regulation of Ec-
stasy important (post)'
questions[12]='Permitted with a medical subscription (post)'
questions[13]='Permitted in a pharmacy (post)'
questions[14]='Permitted in a smart shop (post)'
questions[15]='Permitted in bars/clubs (post)'
questions[16]='Permitted for people of legal age (post)'
questions[17]='Permitted with a valid ID (post)'
questions[18]='and marketing can be permitted (post)'
questions[19]='I feel more informed about the regulation of Ecstasy'
questions[20]='Gender'
questions[21]='Age'
questions[22]='I have used Ecstasy'

pdf('poppi.pdf',paper='a4',height=10)
layout(matrix(1:18,9,2))
par(mar=c(3,3,1,1))
for(i in 1:22)
barplot(table(x[,i]), main=questions[i],cex.main=.8,col='lightblue')
dev.off()

y=as.matrix(x)
for(i in 1:9)
{
  print(questions[i])
  t=t.test(y[,i+9],y[,i],paired=T)
  print(round(t$estimate,3))
  print(round(t$p.value,5))
} 

model <- aov(Response ~ used + Time + Error(ID/Time), data = data)
summary(model)

data_selection=as.data.frame(x)
names(data_selection)
data_selection=data_selection[,c('used',"sale1","sale2","imp1","imp2")]
data_selection$used[data_selection$used==4]=NA


data_selection=na.omit(data_selection)
w=reshape(data_selection,
        varying = list(c("sale1", "sale2"),c("imp1","imp2")),  # Columns to pivot
        v.names = c("sale","imp"),                   # Name for values
        timevar = "Pre_Post",                    # Name for the time variable
        times = c("Pre", "Post"),         # New variable levels
        direction = "long"
)
head(w)
model1 <- aov(sale ~ Pre_Post + Error(as.factor(id)/Pre_Post), data = w)
summary(model1)
t.test(w$sale[w$Pre_Post=='Pre'],w$sale[w$Pre_Post=='Post'],paired=T)
t.test(data_selection[,2],data_selection[,3],paired=T)

model2 <- aov(sale ~ used + Pre_Post + Error(as.factor(id)/Pre_Post), data = w)
summary(model2)

interaction.plot(
  x.factor = w$Pre_Post, 
  trace.factor = w$used, 
  response = w$sale,
  fun = mean,
  type = "b",
  col = c("blue", "red"),
  pch = c(19, 17),
  xlab = "Pre/Post",
  ylab = "Mean Sale",
  trace.label = "Used",
  main = "Interaction Plot: Sale by Used and Pre/Post",
  bty='n'
)

library(ggplot2)
library(tidyverse)
summary_data <- w %>%
  group_by(used, Pre_Post) %>%
  summarise(
    mean_sale = mean(sale),
    se = sd(sale) / sqrt(n()),
    .groups = "drop"
  )

ggplot(summary_data, aes(x = Pre_Post, y = mean_sale, group = used, color = used)) +
  geom_line() +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_sale - se, ymax = mean_sale + se), width = 0.1) +
  labs(
    title = "Mean Sale by Pre/Post and Used",
    x = "Pre/Post",
    y = "Mean Sale",
    color = "Used"
  ) +
  theme_minimal()


# chisq.test(x[,1],x[,10])
# chisq.test(cbind(table(x[,1]),table(x[,10])))
