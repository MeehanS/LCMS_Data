
#dC MS1 Plot 

lines <- read_lines("C:/Users/sm92252/Documents/Customer Order Details/2026/September/11693/11693/CD Results/Plots/PF11693-5conc-Thymidine-MS1.txt")

data_lines <- lines[-c(1,2,3)]

spectraPoints <- tibble(raw= data_lines) %>%
  filter(raw != "") %>%
  mutate(
    mz = parse_number(raw),
    intensity = parse_number(str_remove(raw,"^[0-9.]+\\s+")),
    adduct = str_extract(raw, "\\[.*?\\]")
  )%>%
  select(mz,intensity,adduct)

# 
# spectraPoints <- spectraPoints %>% filter(between(mz,200,350))

ggplot(spectraPoints, aes(x=mz, y=intensity)) +
  geom_segment(aes(xend=mz, yend=0,
                   color = !is.na(adduct)),
               linewidth = 2
  ) +
  geom_text(
    data = spectraPoints %>% filter(!is.na(adduct)),
    aes(label=paste(adduct, substr(mz,1,7), sep = ":")),
    vjust = -0.5, hjust = 0.2, size=5, color ="#01CBAE", 
  )+
  scale_color_manual(
    values = c("FALSE" = "gray40", "TRUE" = "#01CBAE"),
    labels = c("Non-adduct", "Adduct"),
    name = "Peak Type"
  )+
  xlab("m/z") +
  ylab("Intensity") +
  labs(
    title = "Thymidine MS1 Plot - Positive"
  )+
  theme_minimal()+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        title = element_text(size = 16)
  )


ggsave("C:/Users/sm92252/Documents/Customer Order Details/2026/September/11693/11693/CD Results/Plots/Thymidine_MS1plot_600dpi.tiff", plot = get_last_plot(), width = 8, height = 8, dpi = 600, units = "in", compression = "lzw")




#Experimental vs Ref MS2 

lines <- read_lines("C:/Users/sm92252/Documents/Customer Order Details/2026/September/11693/11693/CD Results/Plots/PF11693-5conc-Thymidine-MS2.txt")

data_lines <- lines[-c(1,2,3)]

spectraPoints <- tibble(raw= data_lines) %>%
  filter(raw != "") %>%
  mutate(
    mz = parse_number(raw),
    intensity = parse_number(str_remove(raw,"^[0-9.]+\\s+")),
  )%>%
  select(mz, intensity) 


lib<- spectraPoints %>% filter(intensity<0)
exp <- spectraPoints %>% filter(intensity>0)

tol <- 0.002

matches <- lib%>%
  rowwise() %>%
  mutate(
    match_mz = list(exp$mz[abs(exp$mz-mz) <= tol])
  ) %>%
  unnest(match_mz, keep_empty = TRUE)

exp<- exp %>%
  mutate(
    is_match = mz %in% matches$match_mz
  )

spectraPoints2 <- bind_rows(
  lib %>% mutate(color_group = "library"),
  exp %>% mutate(
    color_group = if_else(is_match, "match", "other")
  )
)

ggplot(spectraPoints2, aes(x=mz, y=intensity)) +
  geom_segment(aes(xend=mz, yend=0, color = color_group),
               linewidth = 1
  ) +
  geom_text(
    data = spectraPoints2 %>% 
      filter(color_group != "other")%>%
      filter(abs(intensity)>50000),
    aes(
      label=substr(mz,1,7), 
      color = color_group,
      vjust = if_else(color_group == "library", 1.5, -0.5) 
    ),
    size=3.5,
    show.legend = FALSE  
  )+ 
  scale_color_manual(
    values = c("match" = "#01CBAE",
               "library" = "darkorange",
               "other" = "grey60"),
    labels = c("library" = "Reference Peak",
               "match" = "Experimental Match",
               "other" = "No Match"),
    name = ""
  )+
  xlab("m/z") +
  ylab("Intensity") +
  labs(
    title = "Thymidine - MS2 Library Match"
  )+
  theme_minimal()+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        title = element_text(size = 16))


ggsave("C:/Users/sm92252/Documents/Customer Order Details/2026/September/11693/11693/CD Results/Plots/Thymidine_MS2plot_600dpi.tiff", plot = get_last_plot(), width = 12, height = 8, dpi = 600, units = "in", compression = "lzw")

