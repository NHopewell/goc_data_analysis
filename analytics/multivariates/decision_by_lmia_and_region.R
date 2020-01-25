### Decision by LMIA_Section + Region 

#### Decision Config A

ggplot(employer_data_comp %>%
         filter(!is.na(target), !is.na(LMIA_Regulation_Section), !is.na(Region)), 
       mapping = aes(target,fill = LMIA_Regulation_Section)) + 
  geom_bar(position='fill', color = "grey40") +
  labs(title="Decision Breakdown by LMIA Section, Region", 
       x = "",
       y = "",
       fill = "") + 
  scale_fill_manual(values = 
                      rev(c("#1f83b4", "#ff7f0e", "#d63a3a")))+
  facet_wrap(~Region, ncol=1) + 
  coord_flip() + 
  scale_y_continuous(labels=percent) + 
  ylab('Percent') +
  theme_minimal() +
  theme(legend.position = "bottom") + 
  guides(fill = guide_legend(reverse=T))

#### Decision Config B

ggplot(employer_data_comp %>%
         filter(!is.na(multi_target), !is.na(LMIA_Regulation_Section), !is.na(Region)), 
       mapping = aes(multi_target,fill = LMIA_Regulation_Section)) + 
  geom_bar(position='fill', color = "grey40") +
  labs(title="Decision Breakdown by LMIA Section, Region", 
       x = "",
       y = "",
       fill = "") + 
  scale_fill_manual(values = 
                      rev(c("#1f83b4", "#ff7f0e", "#d63a3a")))+
  facet_wrap(~Region, ncol=1) + 
  coord_flip() + 
  scale_y_continuous(labels=percent) + 
  ylab('Percent') +
  theme_minimal() +
  theme(legend.position = "bottom") + 
  guides(fill = guide_legend(reverse=T))

#### Decision Config C  

ggplot(employer_data_comp %>%
         filter(!is.na(target_three), !is.na(LMIA_Regulation_Section), !is.na(Region)), 
       mapping = aes(target_three,fill = LMIA_Regulation_Section)) + 
  geom_bar(position='fill', color = "grey40") +
  labs(title="Decision Breakdown by LMIA Section, Region", 
       x = "",
       y = "",
       fill = "") + 
  scale_fill_manual(values = 
                      rev(c("#1f83b4", "#ff7f0e", "#d63a3a")))+
  facet_wrap(~Region, ncol=1) + 
  coord_flip() + 
  scale_y_continuous(labels=percent) + 
  ylab('Percent') +
  theme_minimal() +
  theme(legend.position = "bottom") + 
  guides(fill = guide_legend(reverse=T))


#### Decision Config D  

ggplot(employer_data_comp %>%
         filter(!is.na(target_down), !is.na(LMIA_Regulation_Section), !is.na(Region)), 
       mapping = aes(target_down,fill = LMIA_Regulation_Section)) + 
  geom_bar(position='fill', color = "grey40") +
  labs(title="Decision Breakdown by LMIA Section, Region", 
       x = "",
       y = "",
       fill = "") + 
  scale_fill_manual(values = 
                      rev(c("#1f83b4", "#ff7f0e", "#d63a3a")))+
  facet_wrap(~Region, ncol=1) + 
  coord_flip() + 
  scale_y_continuous(labels=percent) + 
  ylab('Percent') +
  theme_minimal() +
  theme(legend.position = "bottom") + 
  guides(fill = guide_legend(reverse=T))
