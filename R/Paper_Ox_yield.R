dir <- "/home/benny/PHD_DATA/MISC/OX_yield.txt"

data <- read.csv(dir, sep = "\t")
data <- data[order(data$E), ]

barplot(height = data$yield,
        names.arg = data$OX,
        ylab = expression("Yield KPF"[6]*" [%]"),
        ylim = c(0, 100),
        las = 2,
        family = "Montserrat",
        col = scales::alpha("maroon", 0.3),)
box()
