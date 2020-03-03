# Загрузим данные
sat.df <- read.csv("http://bit.ly/38fbmHv")

# Сводка по данным
summary(sat.df)

# Оценка данных на пригодность для модели
library(car)
scatterplotMatrix(sat.df)

# Распределение значений переменной «расстояние до парка»
hist(sat.df$distance)

# Преобразуем переменную «расстояние до парка» и построим диаграмму распределения значений
sat.df$logdist <- log(sat.df$distance)
hist(sat.df$logdist)

# Матрица корреляций
library(corrplot)
corrplot.mixed(cor(sat.df[ , c(2, 4:9)]), upper="ellipse")

# График для оценки связи переменных overall и rides
plot(overall~rides , data=sat.df, 
     xlab="Satisfaction with Rides", ylab="Overall Satisfaction")

# Линейная модель для overall и rides
lm(overall ~ rides, data=sat.df)

# Как работает модель (пример), если вручную подставлять новые значения удовлетворенности в формулу
-94.962 + 1.703*75

# Сохраним модель в отдельный объект m1
m1 <- lm(overall ~ rides, data=sat.df)

# Добавим линию тренда на график
plot(overall ~ rides, data=sat.df,
     xlab="Satisfaction with Rides", ylab="Overall Satisfaction")
abline(m1, col='blue')

# Отчет по модели
summary(m1)

# Линейная модель для нескольких переменных
m2 <- lm(overall ~ rides + games + wait + clean, data=sat.df)
summary(m2)

# Сравним модели с помощью ANOVA
anova(m1, m2)


predict(m2, sat.df[1:10, ]) # прогноз по данным из sat.df
fitted(m2)[1:10]  # прогнозные значения по модели. дает результат аналогичный predict()


