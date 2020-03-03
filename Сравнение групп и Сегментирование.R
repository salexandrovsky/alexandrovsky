
# Загрузим данные
seg.df <- read.csv("http://bit.ly/3caGxXG")

# Сводка по данным
summary(seg.df)


### СРАВНЕНИЕ СРЕДНИХ ЗНАЧЕНИЙ МЕЖДУ ГРУППАМИ

# Средний доход у покупателей из сегмента 4
mean(seg.df$income[seg.df$Segment == "Segment 4"])

# Средний доход у покупателей из сегмента 4, у которых нет подписки
mean(seg.df$income[seg.df$Segment == "Segment 4" &
                     seg.df$subscribe == "subNo"])

# Средний доход по сегментам
aggregate(income ~ Segment, data = seg.df, mean)

# Средний доход по сегментам и типу жилья
aggregate(income ~ Segment + ownHome, data = seg.df, mean)


aggregate(Segment ~ subscribe, data = seg.df, mean)


# Диаграммы числа подписчиков в каждом сегменте
library(lattice)
histogram( ~ subscribe | Segment, data = seg.df)

# Столбиковая диаграмма для среднего дохода по сегментам
seg.mean <- aggregate(income ~ Segment, data = seg.df, mean)
install.packages ("lattice")
library(lattice)
barchart(income ~ Segment, data = seg.mean, col = "grey")

# Диаграмма размаха («ящик с усами») для среднего дохода по сегментам
boxplot(income ~ Segment,
        data = seg.df,
        yaxt = "n",
        ylab = "Income (rub)")
ax.seq <- seq(from = 0, to = 120000, by = 20000)
axis(
  side = 2,
  at = ax.seq,
  labels = paste(ax.seq / 1000, "k", sep = ""),
  las = 1
)

# Диаграмма размаха («ящик с усами») с помощью функции из пакета lattice
library(lattice)
bwplot(Segment ~ income,
       data = seg.df,
       horizontal = TRUE,
       xlab = "Income")

# Доходы сегментов с дополнительным условием «тип жилья»
bwplot(
  Segment ~ income | ownHome,
  data = seg.df,
  horizontal = TRUE,
  xlab = "Income"
)

# Частота ответов по сегментам и тест Хи-вадрат
table(seg.df$Segment)
chisq.test(table(seg.df$Segment))

# Частота ответов между группами с разным типом жилья
table(seg.df$subscribe, seg.df$ownHome)
chisq.test(table(seg.df$subscribe, seg.df$gender))

# Если переменная принимает только два значения (биномиальная переменная)
binom.test(12, 20, p = 0.5)
binom.test(120, 200, p = 0.5)

# Распределение данных
hist(seg.df$income)

# Диаграмма размаха для среднего дохода по типу жилья
library(lattice)
bwplot(income ~ ownHome, data = seg.df)

# Как связан тип жилья с доходами
t.test(income ~ ownHome, data = seg.df)

# Как связан тип жилья с доходами внутри сегмента 3
t.test(income ~ ownHome, data = subset(seg.df, Segment == "Segment 3"))

# ANOVA для двух групп
seg.aov.own <- aov(income ~ ownHome, data = seg.df)
anova(seg.aov.own)

# ANOVA для всех сегментов
seg.aov.seg <- aov(income ~ Segment, data = seg.df)
anova(seg.aov.seg)

# ANOVA для всех сегментов и типа жилья
anova(aov(income ~ Segment + ownHome, data=seg.df))

# Cредний дохода сегментов на графике вместе с доверительными интервалами
install.packages("multcomp")
library(multcomp)
seg.aov <- aov(income ~ -1 + Segment, data = seg.df)
by.seg  <- glht(seg.aov) # средние и доверительные интервалы
plot(by.seg, xlab = "Income", main = "Mean Income by Segment (95% CI)")

# Задания 
aggregate(income ~ gender, data = seg.df, mean)
anova(aov(income ~ gender, data = seg.df))

library(multcomp)
income.aov <- aov(income ~ -1 + gender, data = seg.df)
by.gender  <- glht(income.aov)
plot(by.gender, xlab = "Income", main = "Mean Income with 95% CI")
