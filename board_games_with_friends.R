library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(RColorBrewer)
library(patchwork)
library(egg)
library(XML)
library(rvest)
library(stringr)
library(magick)
library(ggtext)
library(ggimage)
library(jpeg)
library(png)

# read excel file's categories sheet
board_games_bgg <- read_excel("BoardGames.xlsx", sheet = "Categories")

## Get image from a boardgamegeek link if it wasn't already gotten before # 

for (i in 1:nrow(board_games_bgg)) {

image_name <- paste0(gsub(" ","_",str_to_lower(gsub("[^[:alnum:][:space:]]","",board_games_bgg$Name[i]))),".jpg")

if (file.exists(image_name)) {next}

url <- board_games_bgg$BGG_url[i]

download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
content <- read_html("scrapedpage.html")

image_url <- content %>%
      html_nodes(xpath = '//meta[@property="og:image"]') %>%
      html_attr("content")

name <- content %>%
      html_nodes(xpath = '//meta[@property="og:title"]') %>%
      html_attr("content")

download.file(image_url, destfile = image_name ,mode = "wb")

img <- image_read(image_name)

img_resized <- image_scale(img, "25x25!")
image_write(img_resized, image_name)

if (str_detect(image_url, "png"))  {
img_png <- readPNG(image_name)
writeJPEG(img_png,image_name,quality = 1)
}

board_games_bgg$image_name[i] <- image_name

}

# create smaller dataframe for images
board_games_image <- board_games_bgg %>% select(Name, image_name)

# read Games sheet from excel
board_games <- read_excel("BoardGames.xlsx", sheet = "Games")

# combine with images
board_games <- board_games %>% left_join(board_games_image, by = c("Game" = "Name"))

# create a dataset where Erdem was one of the players and define winners 
games_with_erdem <- board_games %>% filter(str_detect(Group, "E") &
                                    Type == "Competitive" & 
                                    No_of_players >= 3) %>%
             mutate(rank = row_number(),
                    Game = paste0(Game," (",BGG_Weight,")")) %>%
             group_by(Game) %>%
             arrange(Game,desc(rank)) %>% 
             mutate(played_count=rank(rank)
                   ) %>% ungroup() %>%
             select(Group,Date,Game,Erdem,Lasse,Torben,
                    Jakob,Henrik,Soren,Marcin,
                    played_count,rank,BGG_Weight, image_name, No_of_players) %>% 
             #define letters for different winner combinations
             mutate(winner = case_when(
               # Erdem == 1 & Lasse == 1 & Torben ~ "ELT",
               # Erdem == 1 & Lasse == 1 & Jakob ~ "ELJ",
               # Jakob == 1 & Lasse == 1 & Torben ~ "JLT",
               # Erdem == 1 & Lasse == 1 ~ "EL",
               # Erdem == 1 & Torben == 1 ~ "ET",
               Torben == 1 & Lasse == 1 ~ "TL",
               # Erdem == 1 & Jakob == 1 ~ "EJ",
               # Torben == 1 & Jakob == 1 ~ "TJ",
               # Lasse == 1 & Jakob == 1 ~ "LJ",
               Soren == 1 ~ "S", 
               Jakob == 1 ~ "J",
               Erdem == 1 ~ "E",
               Lasse == 1 ~ "L",
               Torben == 1 ~ "T",
               Henrik == 1 ~ "H",
               Marcin == 1 ~ "M",
               TRUE ~ "O"
             )
             )

# assign colours to winners
games_with_erdem$winner <- as.factor(games_with_erdem$winner)
colourCount <- length(unique(games_with_erdem$winner))
getPalette <- colorRampPalette(brewer.pal(9, "Paired"))

myColors <- getPalette(colourCount)
names(myColors) <- levels(games_with_erdem$winner)

#create winning numbers (not used in ggplot later, only for stats)
games_with_erdem_wins <- games_with_erdem %>% 
  select(Group,Date,Game,Erdem,Lasse,Torben,
         Jakob,Henrik,Soren,Marcin,
         BGG_Weight) %>% pivot_longer(
  cols = c("Erdem","Torben","Lasse"
           ,"Jakob","Henrik","Soren","Marcin"
           ),
  names_to = "Player",
  values_to = "Place"
) %>% group_by(Player) %>% mutate(TotalPlays = sum(!is.na(Place))) %>% 
  filter(Place == 1) %>% group_by(Player) %>% 
  summarise(TotalWins = n(),
             TotalPlays = mean(TotalPlays),
             avg_weight_in_W = round(mean(BGG_Weight),2)) %>%
  mutate(WinPercentage = round(TotalWins/TotalPlays,2)) %>%
  ungroup() %>% arrange(desc(TotalWins),desc(WinPercentage))


# create labels with images
games_with_erdem_games_labels <- games_with_erdem %>% group_by(Game, image_name) %>% 
                                 summarise(played_count = max(played_count)) %>% 
                                 select(Game,played_count, image_name) %>% arrange(played_count, Game)

labels_for_y <- c(paste0(games_with_erdem_games_labels$Game, ": <img src='",games_with_erdem_games_labels$image_name,"'/>"))

# Create a new column 'shape' in the dataset using the 'No_of_players' column
games_with_erdem <- games_with_erdem %>%
  mutate(shape = if_else(No_of_players == 3, 24,
                         if_else(No_of_players == 4, 22,
                                 if_else(No_of_players == 5, 21, 1)))) %>%
  mutate(shape = factor(shape, levels = c("24", "22", "21")))

# Use the 'shape' column in the shape aesthetic within geom_point to differentiate shape based on no_of_players
p1 <- ggplot(data = games_with_erdem, aes(x = played_count, y = Game)) + 
  aes(y = reorder(Game, played_count)) + 
  geom_point(aes(fill = winner, shape = shape), color = "black", size = 8, show.legend = c(fill = FALSE, shape = TRUE)) + 
  geom_text(aes(label = winner)) + 
  scale_fill_manual(name = "Winner", values = myColors) +
  scale_shape_manual(
    name = "No. of Players",
    values = c("24" = 24, "22" = 22, "21" = 21), # Map factor levels to shape codes
    labels = c("3 Players", "4 Players", "5 Players", "Other") # Custom legend labels
  ) +
  theme_classic() + 
  scale_x_continuous(breaks = seq(1, max(games_with_erdem$played_count), 1), position = "top",
                     labels = scales::ordinal_format()) + 
  scale_y_discrete(name = NULL, labels = labels_for_y) +
  theme(axis.text.y = ggtext::element_markdown()) +
  labs(y = "Game", x = "Time Played") + 
  theme(panel.background = element_rect(fill = NA, color = "black")) +
  theme(legend.position = c(0.8, 0.8))

png_name <- paste0("board_games_with_erdem_", sub("-", "_", Sys.Date()),".png")

png(filename = png_name, width = 1000, height = 2000, res = 120)
print(p1)
dev.off()
