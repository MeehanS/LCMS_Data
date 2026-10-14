df <- read_excel("C:/Users/sm92252/Documents/Customer Order Details/2026/September/11693/11693/CD Results/Plots/PF11693-5conc-LC.xlsx", col_types = "numeric")
colnames(df) <- c("RT_min","Thymidine", "2'-deoxyguanosine")

df <- df %>% filter(RT_min<6.5)%>%
  mutate(RT_min = round(RT_min, digits = 2))


data_long <- df %>%
  pivot_longer(
    cols = -RT_min,
    names_to = "Compound",
    values_to = "Intensity"
  )

label_df <- data_long %>%
  group_by(Compound) %>%
  slice_max(Intensity, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(RT = RT_min)



ggplot() +
  geom_line(
    data = data_long,
    aes(x = RT_min, y = Intensity, color = Compound),
    linewidth = 1
  ) +
  geom_text_repel(
    data = label_df,
    aes(x = RT, y = Intensity,
        label = paste0(Compound, "\n", RT, " min"),
        color = Compound),
    size = 5,
    fontface = "bold",
    show.legend = FALSE,
    nudge_y = 0.2 * max(label_df$Intensity),  # push labels above the peaks
    direction = "both",                        # or "x" to only shift horizontally
    segment.color = "grey50",                  # leader line from label to peak
    segment.size = 0.4,
    min.segment.length = 0,                    # always draw the leader line
    box.padding = 0.5,
    point.padding = 0.3,
    max.overlaps = Inf,                        # never drop a label
    seed = 42                                  # reproducible placement
  ) +
  scale_y_continuous(
    limits = c(0, 1e+7),
    labels = scales::label_scientific()
  ) +
  scale_color_manual(
    values = c(
      "Thymidine" = "#01CBAE",
      "2'-deoxyguanosine" = "#E4572E",
      "2'-deoxycytidine" = "#2E4057"
    )
  ) +
  labs(
    x = "Retention Time (min)",
    y = "Intensity",
    color = "Compound",
    title = "11693-5 LC Plot of Related Metabolites"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16)
  )

ggsave("C:/Users/sm92252/Documents/Customer Order Details/2026/September/11693/11693/CD Results/Plots/PF11693-5conc-LCplot_600dpi.tiff", plot = get_last_plot(), width = 12, height = 8, dpi = 600, units = "in", compression = "lzw")
