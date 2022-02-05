library(tidyverse)
library(tidytuesdayR)
library(ggforce)
library(ggbump)
library(janitor)
library(ggrepel)

# ---- read data ----

breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

# ---- prep data ----
aus_all <- breed_rank_all %>%
  filter(Breed == 'Australian Cattle Dogs')

aus_traits <- breed_traits %>%
  slice(54)

breed_long <- breed_rank_all %>%
  mutate(change = `2013 Rank` - `2020 Rank`) %>%
  pivot_longer(., cols = `2013 Rank`:`2020 Rank`, names_to = 'year', values_to = 'rank') %>%
  mutate(year = as.numeric(str_remove_all(year, " Rank")))

aus_long <- aus_all%>%
  mutate(change = `2013 Rank` - `2020 Rank`) %>%
  pivot_longer(., cols = `2013 Rank`:`2020 Rank`, names_to = 'year', values_to = 'rank') %>%
  mutate(year = as.numeric(str_remove_all(year, " Rank")))

# ---- build plot ----

# bluey color palette
# https://www.color-hex.com/color-palette/86511

# chilli color palette
# https://www.color-hex.com/color-palette/86515

plot <- breed_long %>%
  ggplot(aes(x = year, y = rank, group = Breed, color = change))+
  geom_bump(smooth = 6, size = 1, alpha = .2) +
  geom_bump(data = aus_long, aes(x = year, y = rank, group = Breed),
            smooth = 6, size = 1.5, color = '#46362a') +
  geom_label(data = aus_long, aes(label = rank, y = rank), 
                   fill = '#ffd08d', color = '#2b2c41')+
  scale_x_continuous(breaks = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020))+
  scale_color_gradientn(colors = c('#d2ebff', '#88cafc', '#404066','#2b2c14'),
                        name = "Rank change\n2013-2022")+
  theme(panel.background = element_rect(fill = '#fffad8'),
        plot.background = element_rect(fill = '#fffad8'),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = '#46362a'),
        title = element_text(colour = '#46362a'),
        legend.background = element_rect(fill = 'transparent'),
        legend.position = 'top',
        legend.box.margin=margin(10, 0, -15, 0)) +
  guides(color = guide_colourbar(barwidth = 25, barheight = .5,
                                 label.position = 'top',
                                 ticks = FALSE))+
  xlab('') +
  ylab('') +
  ggtitle('Australian Cattle Dogs')

ggsave('week-5/aus-cattle.png', plot, width = 7, height = 4, units = 'in')

