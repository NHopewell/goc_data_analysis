### Decision by Noc_Type + Region 

#### Decision Config A

slice <- c("R205(a) Significant benefit exemption codes", 
           "R205(b) Reciprocal employment exemption codes", 
           "R204(a) Canada-international exemption codes", 
           "R204(c) Canada-provincial/territorial exemption codes", 
           "R205(d) Charitable or religious work exemption code")

noc_reg <- employer_data_comp %>%
  filter(!is.na(LMIA_Regulation_Subsection), 
         !is.na(Region)) %>%
  group_by(target, LMIA_Regulation_Subsection, Region) %>%
  summarize(n = n())

ggplot(employer_data_comp %>%
         filter(!is.na(target), !is.na(LMIA_Regulation_Subsection), !is.na(Region),
                LMIA_Regulation_Subsection %in% slice), 
       mapping = aes(target,fill = LMIA_Regulation_Subsection)) + 
  geom_bar(position='fill', color = "grey40") +
  labs(title="Decision Breakdown by LMIA_Subsection, Region",
       x = "",
       y = "",
       fill = "") + 
  scale_fill_manual(values = 
                      rev(c("#1f83b4", "#ff7f0e", "#d63a3a", "#12a2a8", "#2ca030")),
                    labels = rev(c("R204(a)", "R204(c)", "R205(a)", "R205(b)", "R205(d)")))+
  facet_wrap(~Region, ncol=1) + 
  coord_flip() + 
  scale_y_continuous(labels=percent) + 
  ylab('Percent') +
  theme_minimal() +
  theme(legend.position = "bottom") + 
  guides(fill = guide_legend(reverse=T))

#### Decision Config B 

ggplot(employer_data_comp %>%
         filter(!is.na(multi_target), !is.na(LMIA_Regulation_Subsection), !is.na(Region),
                LMIA_Regulation_Subsection %in% slice))+ 
  geom_bar(mapping = aes(multi_target,fill = LMIA_Regulation_Subsection), position='fill',
           color = "grey40") +
  labs(title="Decision Breakdown by LMIA_Subsection, Region", 
       x = "",
       y = "",
       fill = "") + 
  facet_wrap(~Region, ncol=1) + 
  coord_flip() + 
  scale_y_continuous(labels=percent) + 
  ylab('Percent') +
  scale_fill_manual(values = 
                      rev(c("#1f83b4", "#ff7f0e", "#d63a3a", "#12a2a8", "#2ca030")),
                    labels = rev(c("R204(a)", "R204(c)", "R205(a)", "R205(b)", "R205(d)")))+
  theme_minimal() +
  theme(legend.position = "bottom") + 
  guides(fill = guide_legend(reverse=T))

#### Decision Config C 

ggplot(employer_data_comp %>%
         filter(!is.na(target_three), !is.na(LMIA_Regulation_Subsection), !is.na(Region),
                LMIA_Regulation_Subsection %in% slice))+ 
  geom_bar(mapping = aes(target_three,fill = LMIA_Regulation_Subsection), position='fill',
           color = "grey40") +
  labs(title="Decision Breakdown by LMIA_Subsection, Region",
       x = "",
       y = "",
       fill = "") + 
  facet_wrap(~Region, ncol=1) + 
  coord_flip() + 
  scale_y_continuous(labels=percent) + 
  ylab('Percent') +
  scale_fill_manual(values = 
                      rev(c("#1f83b4", "#ff7f0e", "#d63a3a", "#12a2a8", "#2ca030")),
                    labels = rev(c("R204(a)", "R204(c)", "R205(a)", "R205(b)", "R205(d)")))+
  theme_minimal() +
  theme(legend.position = "bottom") + 
  guides(fill = guide_legend(reverse=T))


#### Decision Config D

ggplot(employer_data_comp %>%
         filter(!is.na(target_down), !is.na(LMIA_Regulation_Subsection), !is.na(Region),
                LMIA_Regulation_Subsection %in% slice))+ 
  geom_bar(mapping = aes(target_down,fill = LMIA_Regulation_Subsection), position='fill',
           color = "grey40") +
  labs(title="Decision Breakdown by LMIA_Subsection, Region",
       x = "",
       y = "",
       fill = "") + 
  facet_wrap(~Region, ncol=1) + 
  coord_flip() + 
  scale_y_continuous(labels=percent) + 
  ylab('Percent') +
  scale_fill_manual(values = 
                      rev(c("#1f83b4", "#ff7f0e", "#d63a3a", "#12a2a8", "#2ca030")),
                    labels = rev(c("R204(a)", "R204(c)", "R205(a)", "R205(b)", "R205(d)")))+
  theme_minimal() +
  theme(legend.position = "bottom") + 
  guides(fill = guide_legend(reverse=T))