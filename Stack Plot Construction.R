# This is the coding script to draw stacked plot of paper distributions

# Stacked bar plot where y is the number of papers, fill is race as predictor/ stratified/ predictor and stratified
# Stack bar plot where y is the number of papers, fill is the study region

############################## Description #################################3
#                                                                           #
# < CVD  General>                                                           #
#                                                                           #
#   17 papers total                                                         #
#       * 9 considered race                                                 #
#       * 5 multi-ethnic studies                                            #
#       * 4 using race as predictor                                         #
#       * 0 stratified                                                      #
#                                                                           #
# < CVD  subpop>                                                            #
#                                                                           #
#   6 papers total                                                          #
#       * 5 considered race                                                 #
#       * 3 multi-ethnic studies                                            #
#       * 2 using race as predictor                                         #
#       * 0 stratified                                                      #
#                                                                           #
# < COVID >                                                                 #
#                                                                           #
#   12 papers total (general population)                                    #
#       * 5 using race as predictor                                         #
#       * 5 multi-ethnic studies                                            #
#       * 0 stratified                                                      #
#                                                                           #
#   10 papers total (subgroup population)                                   #
#       * 5 using race as predictor                                         #
#       * 5 multi-ethnic studies                                            #
#       * 0 stratified                                                      #
#                                                                           #
############################################################################3

############################## Set up ##############################################

pkgs <- c("tidyverse", "patchwork", "RColorBrewer", "ggthemes", "cowplot",
          "xlsx", "ggokabeito", "ggh4x")
invisible(lapply(pkgs, library, character.only = T))

# read in the data
df_raw_xlsx <- list()

for(i in 1:4){
  df_raw_xlsx[[i]] <- read.xlsx("Data/Prediction Model Fairness Table.xlsx",
                                sheetIndex = i, startRow = 2)
  df_raw_xlsx[[i]] <- df_raw_xlsx[[i]] %>% 
    dplyr::filter(rowSums(is.na(df_raw_xlsx[[i]])) != ncol(df_raw_xlsx[[i]]))
}

names(df_raw_xlsx) <- c("CVD general", "CVD subPop", "COVID general", "COVID subPop")

# drop some of the columns that is not needed
df_raw_xlsx[[1]] %>% names 
# drop authors, type of model, criteria, and if fairness considered
# drop 2, 3, 8, 14, 15
for(i in 1:length(df_raw_xlsx)){
  df_raw_xlsx[[i]] <- df_raw_xlsx[[i]] %>% 
    dplyr::select(-c(2, 3, 8, 14, 15))
}

# delete the mysterious entry at the very last rows
# df_raw_xlsx[["CVD general"]] <- df_raw_xlsx[["CVD general"]][-18, ]
# df_raw_xlsx[["CVD subPop"]] <- df_raw_xlsx[["CVD subPop"]][-7, ]
# df_raw_xlsx[["COVID general"]] <- df_raw_xlsx[["COVID general"]][-13, ]
# df_raw_xlsx[["COVID subPop"]] <- df_raw_xlsx[["COVID subPop"]][-11, ]


## create column on how race is used: 0 not included, 1 is predictor, 2 is stratified
df_raw_xlsx[["CVD general"]]$race_group <- c(0, 1, rep(0, times = 8), 1, rep(0, times = 4), 1, 1) %>% as.factor()
df_raw_xlsx[["CVD subPop"]]$race_group <- c(0, 0, 1, 0, 1, 0) %>% as.factor()
df_raw_xlsx[["COVID general"]]$race_group <- c(1,1,0,0,0,1,1,1,0,0,0,0) %>% as.factor()
df_raw_xlsx[["COVID subPop"]]$race_group <- c(1,0,0,1,0,1,0, 0,1,1) %>% as.factor()


df_raw_xlsx[["CVD general"]]$race_tmp <- c(0, 1, rep(0, times = 8), 1, rep(0, times = 4), 1, 1)
df_raw_xlsx[["CVD subPop"]]$race_tmp <- c(0, 0, 1, 0, 1, 0)
df_raw_xlsx[["COVID general"]]$race_tmp <- c(1,1,0,0,0,1,1,1,0,0,0,0)
df_raw_xlsx[["COVID subPop"]]$race_tmp <- c(1,0,0,1,0,1,0, 0,1,1)

# df_raw_xlsx[["CVD general"]]$focus <- rep("CVD general", nrow(df_raw_xlsx[["CVD general"]])) %>% as.factor()
# df_raw_xlsx[["CVD subPop"]]$focus <- rep("CVD subPop", nrow(df_raw_xlsx[["CVD subPop"]])) %>% as.factor()
# df_raw_xlsx[["COVID general"]]$focus <- rep("General Population", nrow(df_raw_xlsx[["COVID general"]])) %>% as.factor()
# df_raw_xlsx[["COVID subPop"]]$focus <- rep("Sub Population", nrow(df_raw_xlsx[["COVID subPop"]])) %>% as.factor()

df_raw_xlsx[["CVD general"]]$source <- rep("CVD general", nrow(df_raw_xlsx[["CVD general"]])) %>% as.factor()
df_raw_xlsx[["CVD subPop"]]$source <- rep("CVD subPop", nrow(df_raw_xlsx[["CVD subPop"]])) %>% as.factor()
df_raw_xlsx[["COVID general"]]$source <- rep("COVID \n General Population", nrow(df_raw_xlsx[["COVID general"]])) %>% as.factor()
df_raw_xlsx[["COVID subPop"]]$source <- rep("COVID \n Sub Population", nrow(df_raw_xlsx[["COVID subPop"]])) %>% as.factor()

# df combining the covid papers
df_raw_xlsx[["COVID comb"]] <- rbind(df_raw_xlsx[["COVID general"]], df_raw_xlsx[["COVID subPop"]])
df_raw_xlsx[["CVD comb"]] <- rbind(df_raw_xlsx[["CVD general"]], df_raw_xlsx[["CVD subPop"]])

# df combining all the papers
df_raw_xlsx[["All papers"]] <- dplyr::bind_rows(df_raw_xlsx[["CVD general"]],
                                                df_raw_xlsx[["CVD subPop"]],
                                                df_raw_xlsx[["COVID general"]],
                                                df_raw_xlsx[["COVID subPop"]])


# Additional changes to plot

# df_raw_xlsx[["CVD"]]$Year <- as.character(df_raw_xlsx[["CVD"]]$Year)

df_raw_xlsx[["All papers"]] <- df_raw_xlsx[["All papers"]] %>% 
  group_by(source) %>% 
  dplyr::mutate(label_race = sum(race_tmp))

######################### Plot the Stacked Plots #####################



pic_stack_bar <- list()

# Construct of plotting dataset from the counts of papers of df_raw_xlsx
tmp_plot <- data.frame(source = c(rep("CVD \n General Population", 2), rep("CVD \n Sub Population", 2),
                                  rep("COVID \n General Population", 2),rep("COVID \n Sub Population", 2)),
                         race_group = c(rep(c("0", "1"), 4)) %>% as.factor(),
                         counts = c(13, 4, 4, 2, 7, 5, 5, 5)) %>% group_by(source) %>% 
  dplyr::mutate(label_y = rev(cumsum(rev(counts))))
tmp_plot$source <- factor(tmp_plot$source, levels = c("CVD \n General Population", "CVD \n Sub Population",
                                                      "COVID \n General Population", "COVID \n Sub Population"))

tmp_plot_sex <- data.frame(source = c(rep("CVD \n General Population", 3), rep("CVD \n Sub Population", 3),
                                      rep("COVID \n General Population", 3),rep("COVID \n Sub Population", 3)),
                           sex_group = c(rep(c("0", "1", "2"), 4)) %>% as.factor(),
                           counts = c(1, 11, 5, 2, 3, 1, 1, 10, 1, 0, 9, 1)) %>% group_by(source) %>% 
  dplyr::mutate(label_y = rev(cumsum(rev(counts))))

tmp_plot_sex <- tmp_plot_sex[tmp_plot_sex$counts != 0, ] #%>% group_by(source) %>% arrange(counts)
tmp_plot_sex$source <- factor(tmp_plot_sex$source, levels = c("CVD \n General Population", "CVD \n Sub Population",
                                                      "COVID \n General Population", "COVID \n Sub Population"))

tmp_plot <- tmp_plot %>% dplyr::mutate(source_num = case_when(source == "CVD \n General Population" ~ 1,
                                                              source == "CVD \n Sub Population" ~ 2,
                                                              source == "COVID \n General Population" ~ 3,
                                                              source == "COVID \n Sub Population" ~ 4))

tmp_plot_sex <- tmp_plot_sex %>% dplyr::mutate(source_num = case_when(source == "CVD \n General Population" ~ 1,
                                                                      source == "CVD \n Sub Population" ~ 2,
                                                                      source == "COVID \n General Population" ~ 3,
                                                                      source == "COVID \n Sub Population" ~ 4))


tmp_a <- tmp_plot %>% dplyr::mutate(flag = "Race") %>% dplyr::rename(group = race_group)
tmp_b <- tmp_plot_sex %>% dplyr::mutate(flag = "Sex") %>% dplyr::rename(group = sex_group)

tmp_facet <- rbind(tmp_a, tmp_b)



# preprocessing for the nested layer
split_source <- strsplit(tmp_facet$source %>% as.character(), "\n ") %>% unlist
nest_group <- split_source[!grepl("CVD|COVID", split_source)]
facet_head <- split_source[!grepl("Population", split_source)]

tmp_nested <- tmp_facet %>%
  as.data.frame() %>% 
  mutate(nest_group = nest_group,
         header = facet_head) %>% 
  mutate(header = ifelse(header == "COVID ",
                             "COVID-19", "CVD")) %>% 
  dplyr::mutate(header = factor(header, 
                                levels = c("CVD", "COVID-19"))) 

tmp_nested$nest_group <- ifelse(tmp_nested$nest_group == "Sub Population",
                                "Subpopulation", tmp_nested$nest_group)


pic_stack_bar[["All papers race and sex method (nested)"]] <- ggplot(tmp_nested, aes(x = flag, y = counts,
                                                                                  fill = group, label = counts)) + 
  geom_bar(stat="identity", 
           position = position_stack(reverse = T), 
           width = 0.75, col = "black") + 
  geom_text(position = position_stack(vjust = 0.5, reverse = T),
            size = 4.5) + 
  facet_nested(~header + nest_group) +
  scale_fill_manual(values = alpha(c("0" = "grey70",
                                     "1" = "burlywood1",
                                     "2" = "lightsteelblue1"), 0.95),
                    limits = c("0", "1", "2"),
                    breaks = c("0", "1", "2"),
                    labels = c("Not included", "Predictor", "Stratification Factor"),
                    name = "") +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 18)) +
  labs(y = "Counts", x = "",
       title = "") +
  theme_bw() + theme(legend.position = "top",
                     panel.grid.minor=element_blank(),
                     panel.grid.major=element_blank(),
                     axis.line = element_line(colour = "black",
                                              linetype = "solid"),
                     axis.text.x = element_text(size = 12),
                     axis.text.y = element_text(size = 12),
                     axis.title.y = element_text(size = 12),
                     strip.text.x = element_text(face = "bold", size = 12),
                     legend.text=element_text(size=12))

# ggsave("tmp_fairness.jpeg", width = 8, height = 6)


# plot for geo regions
# Construct data for plotting
tmp_plot2 <- data.frame(source = c(rep("CVD \n General Population", 6),
                                   rep("CVD \n Sub Population", 6), 
                                   rep("COVID-19 \n General Population", 6),
                                   rep("COVID-19 \n Sub Population", 6)),
                       geo_group = c(rep(c("North America", "Asia", "Europe",
                                            "South America", "Oceania", "Africa"), 4)) %>% as.factor(),
                       counts = c(8, 2, 7,0, 2, 0,
                                  1, 2, 3, 0, 3, 0,
                                  6,4,3,1,1,2,
                                  4,0,5,2,0,0)) %>%
  group_by(source) %>% 
  dplyr::mutate(label_y = rev(cumsum(rev(counts))))

tmp_plot2 <- tmp_plot2[tmp_plot2$counts != 0, ] %>% group_by(source) %>% arrange(counts)
tmp_plot2$source <- factor(tmp_plot2$source, levels = c("CVD \n General Population", "CVD \n Sub Population",
                                                        "COVID-19 \n General Population", "COVID-19 \n Sub Population"))

# some adjustments for facet_nested
split_source <- strsplit(tmp_plot2$source %>% as.character(), "\n ") %>% unlist
nest_group <- split_source[!grepl("CVD|COVID-19", split_source)]
facet_head <- split_source[!grepl("Population", split_source)]

tmp_nested2 <- tmp_plot2 %>%
  as.data.frame() %>% 
  mutate(nest_group = nest_group,
         header = facet_head) %>% 
  dplyr::mutate(header = factor(header, 
                                levels = c("CVD ", "COVID-19 ")))

tmp_nested2$nest_group <- ifelse(tmp_nested2$nest_group == "Sub Population",
                                "Subpopulation", tmp_nested2$nest_group)



## facet_nested
pic_stack_bar[["All papers Study Regions (nested)"]] <- ggplot(tmp_nested2, aes(x = nest_group, y = counts,
                                                                                fill = geo_group, label = counts)) +
  geom_bar(stat = "identity", col = "black") +
  geom_text(size = 4.5,
            position = position_stack(vjust = 0.5)) +
  facet_nested(~header) +
  scale_fill_manual(values = alpha(c("North America" = "thistle",
                                     "Asia" = "burlywood1",
                                     "Europe" = "lightsteelblue1",
                                     "South America" = "khaki2",
                                     "Oceania" = "darkseagreen3",
                                     "Africa" = "rosybrown"), 0.95),
                    limits = c("North America", "Asia", "Europe",
                               "South America", "Oceania", "Africa"),
                    breaks = c("North America", "Asia", "Europe",
                               "South America", "Oceania", "Africa"),
                    labels = c("North America", "Asia", "Europe",
                               "South America", "Oceania", "Africa"),
                    name = "") +
  scale_y_continuous(expand = expansion(mult = 0), limits = c(0, 20)) +
  labs(y = "Counts", x = "") +
  # title = "Study Regions of the Papers*",
  # subtitle = "*Some studies cover multiple study regions") +
  theme_bw() + theme(legend.position = "top",
                     panel.grid.minor=element_blank(),
                     panel.grid.major=element_blank(),
                     axis.line = element_line(colour = "black",
                                              linetype = "solid"),
                     axis.text.x = element_text(size = 12),
                     axis.text.y = element_text(size = 12),
                     axis.title.y = element_text(size = 12),
                     strip.text.x = element_text(face = "bold", size = 12),
                     legend.text=element_text(size=12))

# ggsave("stack bar - study regions.jpeg", width = 8, height = 6)


# Save the plots
tt <- paste0("~/maps/stack ", seq_along(pic_stack_bar), ".jpeg")
tt1 <- paste0("~/maps/stack ", seq_along(pic_stack_bar), ".pdf") 

for(i in 1:length(pic_stack_bar)){
  ggsave(tt[i], plot = pic_stack_bar[[i]],
         width = 8, height = 6)
  ggsave(tt1[i], plot = pic_stack_bar[[i]],
         width = 8, height = 6)
}
