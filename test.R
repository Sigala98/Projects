library(knitr)
library(tidyverse)
library(ggrepel)
library(colorspace)
library(palmerpenguins)


Cristian Sigala Crs4565 


# replace "#FFFFFF" with your own colors
colors <- c("#5626B4", "#26b4a8", "#26b42f", "#b42f26")

swatchplot(colors)


penguin_labels <- tibble(
  species = c("Adelie", "Chinstrap", "Gentoo"),
  bill_length_mm = c(30, 55, 52),
  body_mass_g = c(4100, 3500, 6250),
  label = c("Adelie", "Chinstrap", "Gentoo"),
  hjust = c(0, 0.5, 0),
  vjust = c(0, 0.5, 1)
)



ggplot(penguins, aes(bill_length_mm, body_mass_g, color = species)) +
  geom_point(size = 2, na.rm = TRUE) + 
  theme_bw()+ 
  scale_color_manual(values = c("#5626B4", "#26b4a8", "#26b42f", "#b42f26")) +
  xlab("Bill Length (mm)") + 
  ylab("Body Mass (g)") + 
  geom_text(
    data = penguin_labels,
    aes(
      label = label,
      hjust = hjust, vjust = vjust
    ),
    size = 14/.pt 
  ) +
  guides(color = "none", shape = "none")


tx_census <- read_csv("https://wilkelab.org/SDS375/datasets/US_census.csv") %>% 
  filter(state == "Texas") %>%
  select(county = name, pop2010, per_capita_income)




tx_census %>%
  mutate(
    label = ifelse(county %in% 
                     c("Travis County", "Tarrant County", 
                       "Harris County","Anderson County" , 
                      "Baylor County"), county, "")) %>% 
  ggplot(aes(pop2010, per_capita_income)) +
  geom_point(size = 1) +
  scale_x_log10() + 
  geom_text_repel(
    aes(label = label),
    max.overlaps = Inf,
    box.padding = 3) + 
  xlab("Population") +
  ylab("Per Capita Income") +
  ggtitle("Per Capita Income and Pop of Texas Counties") +
  theme_classic()





