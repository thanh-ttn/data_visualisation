# load packages -----------------------------------------------------------------


library(readxl)
library(tidyverse)
library(janitor)
library(geofacet)
library(ggtext)
library(glue)

library(showtext)
library(sysfonts)
library(systemfonts)


# read data ---------------------------------------------------------------


url <- "https://www.abs.gov.au/statistics/health/health-conditions-and-risks/national-health-survey/2022/NHSDC02.xlsx"
NHS <- tempfile(fileext = ".xlsx")
download.file(url, NHS, mode = "wb")


df <- readxl::read_excel(NHS,
  sheet = "Table 2.3_Proportions",
  range = "A5:S92"
)

unlink(NHS)


# tidy data ---------------------------------------------------------------


obesity <- df |>
  slice(52:61) |>
  select(1, `Proportion (age standardised)(a)`:last_col())

obesity_names <- c("Class", "NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT", "TotalAu")
names(obesity) <- obesity_names

# get state names from geofacet to join with abbreviated names from the main data
state_grid <- as_tibble(geofacet::aus_grid1)

# tidy data
obesity <- obesity |>
  select(-TotalAu) |>
  mutate(across(NSW:ACT, as.numeric)) |>
  separate_wider_delim(Class,
    delim = " (", names = c("category", "BMI"),
    too_many = "drop"
  ) |>
  filter(category %in% c("Underweight", "Total Normal range", "Overweight", "Total Obese")) |>
  mutate(
    BMI = str_remove(BMI, "\\)"),
    category = case_when(
      category == "Total Normal range" ~ "Normal",
      category == "Total Obese" ~ "Obese",
      TRUE ~ category
    )
  ) |>
  mutate(
    category = factor(category, levels = c("Underweight", "Normal", "Overweight", "Obese"), ordered = TRUE),
    BMI = factor(BMI, levels = c("less than 18.50", "18.50–24.99", "25.00–29.99", "30.00 or more"), ordered = TRUE)
  ) |>
  pivot_longer(
    cols = -c(category, BMI),
    names_to = "State",
    values_to = "percent"
  ) |>
  left_join(state_grid, by = join_by(State == code))


# set plot parameters -------------------------------------------------------


col_bg <- "#F5F4EF"
col_palette <- c("#d7301f", "#fc8d59", "#bdbdbd", "#fff7bc")

names(col_palette) <- c("Obese", "Overweight", "Normal", "Underweight")

col_text_main <- "#252525"
col_text_sub <- "#525252"
col_text_axis <- "#969696"

title <- glue::glue("Over *half* of Australians aged 18 and older are either
 <span style='color:{col_palette[[2]]}'>**overweight**</span> or
 <span style='color:{col_palette[[1]]}'>**obese**</span>,
    <br>with approx. half of that group classified as obese, consistently across all states and territories.
")
cap <- "**Source:** Australian Bureau of Statistics. National Health Survey 2022."


# set fonts ----------------------------------------------------------------


sysfonts::font_add_google("Fira Sans", "Fira")
body_font <- "Fira"

showtext::showtext_auto(enable = TRUE)
showtext::showtext_opts(dpi = 300)


# create plot ---------------------------------------------------------------


plt <- obesity |>
  # base plot
  ggplot(aes(x = factor(1), y = percent, fill = fct_rev(category))) +
  geom_col() +
  geom_text(
    data = obesity |>
      mutate(percent_non = case_when(
        category %in% c("Underweight", "Normal") ~ NA,
        TRUE ~ percent
      )) |>
      mutate(percent_non = janitor::round_half_up(percent_non)),
    mapping = aes(label = percent_non),
    position = position_stack(vjust = 0.5),
    colour = "white",
    fontface = "bold"
  ) +
  # scale_y_continuous(labels = scales::percent) +
  geofacet::facet_geo(~name, grid = "aus_grid1") +
  coord_flip() +
  # formatting on the base plot
  scale_fill_manual(
    values = col_palette,
    labels = c(
      "Obese\n(30.00 or more)",
      "Overweight\n(25.00–29.99)",
      "Normal\n(18.50–24.99)",
      "Underweight\n(less than 18.5)"
    )
  ) +
  labs(
    title = title,
    caption = cap,
    fill = "Classification based on Body Mass Index"
  ) +
  guides(
    fill = guide_legend(
      reverse = TRUE,
      ncol = 4
    )
  ) +
  theme_void(base_family = body_font) +
  theme(
    element_text(colour = col_text_main),
    plot.title = element_textbox_simple(
      hjust = 0,
      halign = 0,
      margin = margin(b = 30, t = 5),
      lineheight = 1.2,
      # family = body_font,
      # size = rel(1.3)
      size = 18
    ),
    plot.caption = element_textbox_simple(
      colour = col_text_sub,
      hjust = 1,
      halign = 1,
      margin = margin(b = 0, t = 10),
    ),
    legend.position = "bottom",
    legend.justification = "left",
    legend.margin = margin(b = -20, t = 0),
    legend.title = element_text(
      colour = col_text_sub,
      margin = margin(b = 10, t = 20)
    ),
    legend.title.position = "top",
    legend.text = element_text(
      colour = col_text_sub,
      lineheight = 1.1
    ),
    strip.text = element_text(
      size = 11,
      face = "bold",
      margin = margin(b = 2)
    ),
    axis.text.x.bottom = element_text(colour = col_text_axis, size = 10),
    axis.ticks.x = element_line(colour = col_text_axis),
    axis.ticks.length.x = unit(0.1, "cm"),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    plot.background = element_rect(
      fill = col_bg,
      colour = NA
    )
  )


plt


# save plot ---------------------------------------------------------------


ggsave(
  filename = paste0("Obesity-NHS-Aus-2022/Plot-Obesity-NHS-Aus-2022", ".jpeg"),
  device = jpeg,
  width = 12,
  height = 8,
  units = "in",
  dpi = 300
)
