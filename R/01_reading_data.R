# Script to analyze data of female representativity in science

## loading packages
library(ggplot2)
library(dplyr)
library(stringr)
library(wesanderson)

## reading data
res <- read.csv("data/form_responses.csv")
pq <- read.csv("data/question_why.csv",
               header = FALSE,
               stringsAsFactors = FALSE)

## color pallete for graphics
cores <-  wes_palette("Darjeeling1", 2)[2:1]

# altering column names
names(res) <- c("sexo", "area_pesquisa",
                "porque", "genero_artigo",
                "mulher_area", "mulher_ciencia",
                "comentarios")

# 1. Sexo ####
sex <- as.data.frame(table(res$sexo))

# Compute percentages
sex$fraction <- sex$Freq / sum(sex$Freq)
# Compute the cumulative percentages (top of each rectangle)
sex$ymax <- cumsum(sex$fraction)
# Compute the bottom of each rectangle
sex$ymin <- c(0, head(sex$ymax, n = -1))
# Compute label position
sex$labelPosition <- (sex$ymax + sex$ymin) / 2
# Compute a good label
sex$label <- paste0(c("mulheres", "homens"), ": ", sex$Freq)

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

p1

# 2. Area de pesquisa ####
area <- as.data.frame(table(res$area_pesquisa))
area$area <- as.character(area$Var1)
area$area[area$area
          %in% c("História", "Patologia animal", "Silvicultura Clonal")] <- "outra"

area.df <- aggregate(Freq ~ area, FUN = sum, data = area)
area.df$perc <- round(area.df$Freq / sum(area.df$Freq), 2) * 100
area.df$label <- paste(area.df$perc, "%")
ama <- wes_palette("Darjeeling1", 5)[3]

p2 <- ggplot(data = area.df, aes(x = reorder(area, perc), y = perc)) +
  geom_bar(stat = "identity", fill = ama) +
  labs(x = "", y = "Respostas %") +
  coord_flip() +
  theme_void() +
  geom_text(aes(label = label),
            color = "black",
            hjust = 1.2,
            #position = position_dodge(0.9),
            size = 3.5) +
  theme(axis.text.y = element_text(hjust = 1.2))

p2

# 3. Por que você acha que mulheres têm menor representatividade do que homens na ciência? (marque uma ou mais opções) ####

res_pq <- unlist(str_split(res$porque, ","))

res_pq
# funcao para contar quantas x cada resposta aparece
pq_count <- function(x) {
count <- str_detect(res_pq, x)
n <- sum(count)
}

pq$n <- sapply(pq$V1, pq_count)


p3 <- ggplot(data = pq, aes(x = reorder(V1, n), y = n)) +
  geom_bar(stat = "identity", fill = ama) +
  labs(x = "", y = "Respostas %") +
  coord_flip() +
  theme_void() +
  geom_text(aes(label = n),
            color = "black",
            hjust = 1.2,
            #position = position_dodge(0.9),
            size = 4) +
  theme(axis.text.y = element_text(hjust = 1))

p3

# 4. Três mulheres da sua área

res_ma <- str_split(res$mulher_area, ",") %>%
  unlist() %>%
  str_trim()

ma_df <- as.data.frame(table(res_ma))
ma_df$res_ma <- as.character(ma_df$res_ma)
ma_df$nomes <- ma_df$res_ma
ma_df$nomes[str_detect(ma_df$nomes, "Graziel")] <- "Graziela Barroso"
ma_df$nomes[str_detect(ma_df$nomes, "Leonora")] <- "Leonora Pires Costa"
ma_df$nomes[str_detect(ma_df$nomes, "Rafaela")] <- "Rafaela Forzza"
ma_df$nomes[str_detect(ma_df$nomes, "não")] <- NA

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

mc_df2 <- aggregate(Freq ~ nomes, data = mc_df, FUN = sum)
mc_df2 <- mc_df2[order(mc_df2$Freq, decreasing = TRUE), ]

write.csv(mc_df2, "outputs/female_science.csv",
          row.names = FALSE)
