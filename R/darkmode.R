# https://jofrhwld.github.io/blog/posts/2025/09/2025-09-11_ggplot-4-0-and-dark-mode/



penguins |>
  ggplot(
    aes(bill_len, bill_dep)
  )+
  geom_point() ->
  plot2

theme_set(
  theme_minimal(base_size = 16) +
    theme(
      text = element_text(family = "Public Sans")
    ) +
    theme_sub_panel(
      grid = element_blank(),
    ) +
    theme_sub_legend(
      key = element_blank(),
      background = element_blank()
    ) +
    theme_sub_axis(
      ticks = element_blank(),
      line = element_line(color = "grey60", linewidth = 0.2)
    )
)


theme_darkmode <- function(){
  theme_minimal(
    base_size = 16,
    paper = "#222",
    ink = "white"
  ) +
    theme(
      text = element_text(family = "Public Sans")
    ) +
    theme_sub_panel(
      background = element_rect(
        fill = "#424952", color = NA
      ),
      grid = element_blank()
    ) +
    theme_sub_legend(
      key = element_blank(),
      background = element_blank()
    ) +
    theme_sub_axis(
      line = element_line(
        color = "grey60", linewidth = 0.2
      )
    )
}

plot2
plot2 + theme_darkmode()
plot2 + theme_dark()
