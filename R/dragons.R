library(tidyverse)
library(ggsankey)
library(showtext)
monsters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-27/monsters.csv')

monsters[grep("Dragon", monsters$name),] -> dragons

dragons |> select(Category=category, Alignment=alignment, Size=size) -> dragons
dragons |> make_long(Category, Alignment, Size) -> df

dcolors <- c("black", "steelblue3", "#b5a642", "#cd7f32", "#BD0026", "#F03B20", "darkorange", "darkgreen", "gold3" ,"green3", "orange","#FD8D3C",
             "#FECC5C", "#FFFFB2", "firebrick1", "ivory4", "antiquewhite", "#CE1256", "#DF65B0", "#D7B5D8", "#F1EEF6")

font_add_google("Kings", "tuesday")
showtext_auto()

ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_sankey(flow.alpha = .6,
              node.color = "gray30") +
  geom_sankey_label(size = 7, color = "white", fill = "gray40", family='tuesday') +
  scale_fill_manual(values=dcolors) +
  theme_sankey(base_size = 40, base_family = 'tuesday') +
  theme(legend.position = "none",
        plot.title = element_text(size = 40, hjust=0.5)) +
  labs(title="Dragons of D&D", x=NULL, alt = 'Sankey plot of dragon categories')
