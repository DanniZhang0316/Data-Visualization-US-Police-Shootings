library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

data_crime <- read.csv("2026-01-03-washington-post-police-shootings-export.csv", stringsAsFactors = FALSE)

data_crime <- data_crime %>%
  mutate(
    date = as.Date(date),
    race_label = case_when(
      tolower(race) == "black" ~ "Must",
      tolower(race) == "hispanic" ~ "Hispaanlane",
      tolower(race) == "white" ~ "Valge",
      tolower(race) == "asian" ~ "Aasia",
      TRUE ~ "Muu"
    ),
    gun_armed = ifelse(grepl("gun|replica", armed, ignore.case = TRUE), "Relvaga", "Ei"),
    escaped = ifelse(flee != "not", "Põgenes", "Ei"),
    mental_illness = ifelse(signs_of_mental_illness == "true", "Vaimne haigus", "Ei"),
    body_camera_label = ifelse(body_camera == "true", "Kaamera oli", "Ei")
  ) %>%
  filter(race_label %in% c("Must", "Hispaanlane", "Valge", "Aasia"))


race_colors <- c(
  "Must"        = "#1F4E79",  
  "Hispaanlane" = "#C00000",  
  "Valge"       = "#FFD21F",  
  "Aasia"       = "#548235"   
)



# --- Population (millions) ---
population_data <- data.frame(
  race_label = c("Valge", "Must", "Hispaanlane", "Aasia"),
  population_millions = c(331 * 0.593, 331 * 0.126, 331 * 0.189, 331 * 0.061)
)

# --- Years covered ---
date_rng <- range(data_crime$date, na.rm = TRUE)
n_years <- as.numeric(difftime(date_rng[2], date_rng[1], units = "days")) / 365.25
if (!is.finite(n_years) || n_years <= 0) n_years <- 1

# --- Summary + rates ---
summary_df <- data_crime %>%
  count(race_label, name = "killed_total") %>%
  left_join(population_data, by = "race_label") %>%
  mutate(
    rate_per_million_per_year =
      (killed_total / (population_millions * 1e6)) / n_years * 1e6
  ) %>%
  arrange(desc(rate_per_million_per_year))

# --- Build variable-width bars (rectangles) ---
plot_df <- summary_df %>%
  mutate(
    pop = population_millions,
    xmin = lag(cumsum(pop), default = 0),
    xmax = cumsum(pop),
    y0 = 0,
    y1 = rate_per_million_per_year,
    label_top = paste0(
      race_label, "\n",
      round(rate_per_million_per_year, 1), " juhtumit miljoni\nelaniku kohta aastas"
    ),
    label_mid = paste0(
      format(killed_total, big.mark = " "), "\n",
      "juhtumit"
    )
  )

# offset for top labels
offset <- 0.015 * max(plot_df$y1, na.rm = TRUE)

p1 <- ggplot(plot_df) +
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = y0, ymax = y1, fill = race_label),
    color = "white", linewidth = 1
  ) +
  geom_text(
    aes(x = (xmin + xmax) / 2, y = y1 + offset, label = label_top),
    vjust = 0, fontface = "bold", size = 4, lineheight = 0.95
  ) +
  geom_text(
    aes(x = (xmin + xmax) / 2, y = y1 * 0.5, label = label_mid),
    fontface = "bold", size = 4, lineheight = 0.95
  ) +
  scale_fill_manual(values = race_colors, guide = "none") +
  scale_x_continuous(
    breaks = c(0, cumsum(plot_df$pop)),
    labels = c("0M", paste0(round(cumsum(plot_df$pop)), "M")),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "USA rahvastik",
    y = "Määr (miljoni elaniku kohta aastas) →"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 2)),
    plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 6)),
    plot.margin = margin(8, 35, 8, 10)
  )

# === PLOT 2: Four characteristics ===

make_plot <- function(df_sum, title_text, show_x_labels = FALSE) {
  ggplot(df_sum, aes(x = race_label, y = percentage, fill = race_label)) +
    geom_bar(stat = "identity", width = 0.6, color = "white", linewidth = 0.6) +
    geom_text(aes(label = paste0(percentage, "%")),
              vjust = -0.3, fontface = "bold", size = 4) +
    scale_fill_manual(values = race_colors, guide = "none") +
    scale_y_continuous(
      limits = c(0, 100),
      expand = expansion(mult = c(0, 0.08))
    ) +
    labs(title = title_text, x = "", y = "") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        vjust = 1.4,
        margin = margin(b = -12),
        face = "bold",
        size = 12
      ),
      plot.margin = margin(t = 0, r = 5, b = 5, l = 5),
      axis.text.x = if (show_x_labels)
        element_text(face = "bold", size = 11, color = "black")
      else
        element_blank(),
      axis.text.y = element_text(size = 10, color = "black"),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
}

# --- Summaries ---
armed_data <- data_crime %>%
  group_by(race_label) %>%
  summarise(total = n(),
            yes = sum(gun_armed == "Relvaga"),
            percentage = round(yes / total * 100, 1),
            .groups = "drop")

flee_data <- data_crime %>%
  group_by(race_label) %>%
  summarise(total = n(),
            yes = sum(escaped == "Põgenes"),
            percentage = round(yes / total * 100, 1),
            .groups = "drop")

mental_data <- data_crime %>%
  group_by(race_label) %>%
  summarise(total = n(),
            yes = sum(mental_illness == "Vaimne haigus"),
            percentage = round(yes / total * 100, 1),
            .groups = "drop")

camera_data <- data_crime %>%
  group_by(race_label) %>%
  summarise(total = n(),
            yes = sum(body_camera_label == "Kaamera oli"),
            percentage = round(yes / total * 100, 1),
            .groups = "drop")

# --- Plots ---
g1 <- make_plot(armed_data,  "TULIRELV VÕI KOOPIA", show_x_labels = FALSE)
g2 <- make_plot(flee_data,   "PÕGENES POLITSEI EEST",               show_x_labels = FALSE)
g3 <- make_plot(mental_data, "VAIMSE HAIGUSE TUNNUSED",  show_x_labels = TRUE)
g4 <- make_plot(camera_data, "KEHAKAAMERA OLEMASOLU",    show_x_labels = TRUE)

# --- Title block ---
top_title <- arrangeGrob(
  textGrob(
    "Politsei poolt surmavalt tulistatud isikute tunnused rassi järgi",
    x = 0.5, hjust = 0.5,
    gp = gpar(fontsize = 16, fontface = "bold")
  ),
  textGrob(
    "Protsendid arvutatud iga rassi sees eraldi (mitte kogu valimi peale)",
    x = 0.5, hjust = 0.5,
    gp = gpar(fontsize = 11)
  ),
  ncol = 1,
  heights = unit.c(unit(1.3, "lines"), unit(1.1, "lines"))
)

plots_block <- arrangeGrob(g1, g2, g3, g4, ncol = 2)

p2 <- arrangeGrob(
  top_title,
  plots_block,
  ncol = 1,
  heights = unit.c(unit(0.14, "npc"), unit(0.86, "npc"))
)

# === Save both plots ===
ggsave("politsei_tulistamised_rahvastik.png", p1, width = 14, height = 5, dpi = 300, bg = "white")

ggsave("politsei_tulistamised_rassi_jargi.png", p2, width = 14, height = 9, dpi = 300, bg = "white")

