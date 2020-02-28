# Script to analyze data of female representativity in science
# generating basic graphics and exporting list of women in science
# by Sara Mortara, first version 2020-02-27

## loading packages
library(ggplot2)
library(ggpubr)
library(dplyr)
library(stringr)
library(wesanderson)
library(echarts4r)

## reading data
res <- read.csv("data/form_responses.csv")
pq <- read.csv("data/question_why.csv",
               header = FALSE,
               stringsAsFactors = FALSE)

## color pallete for graphics
cores <-  wes_palette("Darjeeling1", 2)[2:1]
ver <- rgb(255, 0, 0, maxColorValue = 255, alpha = 100)
cin <- rgb(106, 126, 133, maxColorValue = 255)

# altering column names
names(res) <- c("sexo", "area_pesquisa",
                "porque", "genero_artigo",
                "mulher_area", "mulher_ciencia",
                "comentarios")

# 1. Sexo ####
sex <- as.data.frame(table(res$sexo))

sex$total <- c(20, 16)
sex$total_fraction <- sex$total / sum(sex$total)
# Compute percentages
sex$fraction <- sex$Freq / sum(sex$Freq)
# Compute the cumulative percentages (top of each rectangle)
sex$ymax <- cumsum(sex$fraction)
sex$total_ymax <- cumsum(sex$total_fraction)
# Compute the bottom of each rectangle
sex$ymin <- c(0, head(sex$ymax, n = -1))
sex$total_ymin <- c(0, head(sex$total_ymax, n = -1))
# Compute label position
sex$labelPosition <- (sex$ymax + sex$ymin) / 2
sex$total_labelPosition <- (sex$total_ymax + sex$total_ymin) / 2
# Compute a good label
sex$label <- paste0(c("mulheres", "homens"), ": ", sex$Freq)
sex$total_label <- paste0(c("mulheres", "homens"), ": ", sex$total)

sex
# Make the plot
p1 <- ggplot(sex,
             aes(ymax = ymax, ymin = ymin,
                 xmax = 4, xmin = 3, fill = Var1)) +
  geom_rect() +
  geom_label(x = 3.5, aes(y = labelPosition, label = label), size = 6) +
  scale_fill_manual(values = cores) +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")


p1b <- ggplot(sex,
              aes(ymax = total_ymax, ymin = total_ymin,
                  xmax = 4, xmin = 3, fill = Var1)) +
  geom_rect() +
  geom_label(x = 3.5, aes(y = total_labelPosition, label = total_label),
             size = 6) +
  scale_fill_manual(values = cores) +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")


png("figs/figure01.png", res = 300,
    width = 2400,
    height = 1200)
ggarrange(p1, p1b,
          ncol = 2, nrow = 1,
          labels = c("A. respostas aos questionários",
                     "B. integrantes do PCI"))
dev.off()

# fazendo um infografico da proporcao de bolsistas que responderam ao questionario
category <- data.frame(category = c("mulheres", "homens"),
                       value = round(c(13/17, 10/16) * 100, 0),
                       path = c("path://M21 9c0-4.97-4.03-9-9-9s-9 4.03-9 9c0 4.632 3.501 8.443 8 8.941v2.059h-3v2h3v2h2v-2h3v-2h-3v-2.059c4.499-.498 8-4.309 8-8.941zm-16 0c0-3.86 3.14-7 7-7s7 3.14 7 7-3.14 7-7 7-7-3.14-7-7z",
                                "path://M16 2v2h3.586l-3.972 3.972c-1.54-1.231-3.489-1.972-5.614-1.972-4.97 0-9 4.03-9 9s4.03 9 9 9 9-4.03 9-9c0-2.125-.741-4.074-1.972-5.614l3.972-3.972v3.586h2v-7h-7zm-6 20c-3.86 0-7-3.14-7-7s3.14-7 7-7 7 3.14 7 7-3.14 7-7 7z"))

my_plot <- category %>%
  e_charts(category) %>%
  e_x_axis(splitLine = list(show = FALSE),
           axisTick = list(show = FALSE),
           axisLine = list(show = FALSE),
           axisLabel = list(show = FALSE)) %>%
  e_y_axis(max = 100,
           splitLine = list(show = FALSE),
           axisTick = list(show = FALSE),
           axisLine = list(show = FALSE),
           axisLabel = list(show = FALSE)) %>%
  e_color(color = c(cores[2], ver)) %>%
  e_pictorial(value, symbol = path, z = 10, name = 'realValue',
              symbolBoundingData = 100, symbolClip = TRUE) %>%
  e_pictorial(value, symbol = path, name = 'background',
              symbolBoundingData = 100) %>%
  e_labels(position = "bottom", offset = c(0, 10),
           textStyle = list(fontSize = 42,
                            fontFamily = 'Arial',
                            fontWeight = 'bold',
                            color = cores[2]),
           formatter = "{@[1]}%") %>%
  e_legend(show = FALSE) %>%
  e_theme("westeros")

# saved using rstudio interface 462 x 256
my_plot

# 2. Area de pesquisa ####
area <- as.data.frame(table(res$area_pesquisa))
area$area <- as.character(area$Var1)
area$area[area$area
          %in% c("História", "Patologia animal", "Silvicultura Clonal")] <- "outra"

area.df <- aggregate(Freq ~ area, FUN = sum, data = area)
area.df$perc <- round(area.df$Freq / sum(area.df$Freq), 2) * 100
area.df$label <- paste(area.df$perc, "%")

p2 <- ggplot(data = area.df, aes(x = reorder(area, perc), y = perc)) +
  geom_bar(stat = "identity", fill = cin) +
  labs(x = "", y = "Respostas %") +
  coord_flip() +
  theme_void() +
  geom_text(aes(label = label),
            color = "black",
            hjust = 1.2,
            #position = position_dodge(0.9),
            size = 3.5) +
  theme(axis.text.y = element_text(hjust = 1.2))

png("figs/figure02.png", res = 300,
    width = 1600,
    height = 1200)
p2
dev.off()

# 3. Por que você acha que mulheres têm menor representatividade do que homens na ciência? (marque uma ou mais opções) ####

res_pq <- unlist(str_split(res$porque, ","))

res_pq
# funcao para contar quantas x cada resposta aparece
pq_count <- function(x) {
count <- str_detect(res_pq, x)
n <- sum(count)
}

pq$n <- sapply(pq$V1, pq_count)

pq

p3 <- ggplot(data = pq, aes(x = reorder(V2, n), y = n)) +
  geom_histogram(stat = "identity", fill = "grey", alpha = 0.6) +
  labs(x = "", y = "Respostas %") +
  coord_flip() +
  theme_void() +
  geom_text(aes(label = n),
            color = "black",
            hjust = 1.2,
            #position = position_dodge(0.9),
            size = 7.5) +
  theme(axis.text.y = element_text(hjust = 1.1,
                                   size = 20))

p3

png("figs/figure03.png", res = 300,
    width = 2600,
    height = 600)
p3
dev.off()

# 4. Três mulheres da sua área ####

res_ma <- str_split(res$mulher_area, ",") %>%
  unlist() %>%
  str_trim()

ma_df <- as.data.frame(table(res_ma))
ma_df$res_ma <- as.character(ma_df$res_ma)
ma_df$nomes <- ma_df$res_ma
ma_df$nomes[str_detect(ma_df$nomes, "Graziel")] <- "Graziela Barroso"
ma_df$nomes[str_detect(ma_df$nomes, "Leonora")] <- "Leonora Pires Costa"
ma_df$nomes[str_detect(ma_df$nomes, "Rafaela")] <- "Rafaela Forzza"
ma_df$nomes[str_detect(ma_df$nomes, "Nanuza")] <- "Nanuza Luiza de Menezes"
ma_df$nomes[str_detect(ma_df$nomes, "Lenore")] <- "Leonore Fahrig"
ma_df$nomes[str_detect(ma_df$nomes, "não")] <- NA
ma_df$nomes[str_detect(ma_df$nomes, "Não")] <- NA

ma_df2 <- aggregate(Freq ~ nomes, data = ma_df, FUN = sum)
ma_df2 <- ma_df2[order(ma_df2$Freq, decreasing = TRUE), ]

#dir.create("outputs")
# write.csv(ma_df2, "outputs/female_area.csv",
#           row.names = FALSE)


# 5. Três mulheres na ciência ####
res_mc <- str_split(res$mulher_ciencia, ",") %>%
  unlist() %>%
  str_trim()

mc_df <- as.data.frame(table(res_mc))
mc_df$res_mc <- as.character(mc_df$res_mc)
mc_df$nomes <- mc_df$res_mc
mc_df$nomes[str_detect(mc_df$nomes, "Marie")] <- "Marie Curie"
mc_df$nomes[str_detect(mc_df$nomes, "Jane")] <- "Jane Goodall"
mc_df$nomes[str_detect(mc_df$nomes, "Primavesi")] <- "Ana Maria Primavesi"
mc_df$nomes[str_detect(mc_df$nomes, "Rosalind")] <- "Rosalind Franklin"

mc_df2 <- aggregate(Freq ~ nomes, data = mc_df, FUN = sum)
mc_df2 <- mc_df2[order(mc_df2$Freq, decreasing = TRUE), ]

write.csv(mc_df2, "outputs/female_science.csv",
           row.names = FALSE)

## 6. genero no artigo
gen <- as.data.frame(table(res$genero_artigo))
gen$fraction <- gen$Freq/nrow(res)

gen
