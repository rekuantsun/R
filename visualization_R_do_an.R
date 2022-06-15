library(readxl)
GDP_VN <- read_excel("GDP_VN.xlsx")

colnames(GDP_VN)

dim(GDP_VN)

View(GDP_VN)

head(GDP_VN)

plot(x = GDP_VN$'2000',
     xlab = colnames(GDP_VN),
     main = "Chi tieu nam 2000"
)

barplot(GDP_VN$'2000',
        main = 'Chi tieu nam 2000',
        xlab = colnames(GDP_VN), horiz = TRUE)


barplot(GDP_VN$'2000',
        main = 'Chi tieu nam 2000',
        xlab = colnames(GDP_VN), horiz = FALSE)
