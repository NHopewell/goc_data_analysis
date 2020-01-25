### Decision by Noc_Type + Region 

#### Decision Config A

noc_reg <- employer_data_comp %>%
  filter(!is.na(NOC_Type), 
         !is.na(Region)) %>%
  group_by(target, NOC_Type, Region) %>%
  summarize(n = n())

ggplot(employer_data_comp %>%
         filter(!is.na(target), !is.na(NOC_Type), !is.na(Region)), 
       mapping = aes(target,fill = NOC_Type)) + 
  geom_bar(position='fill', color = "grey40") +
  labs(title="Decision Breakdown by NOC Type, Region",
       x = "",
       y = "",
       fill = "") + 
  scale_fill_manual(values = 
                      rev(c("#1f83b4", "#ff7f0e", "#d63a3a", "#12a2a8", "#2ca030")))+
  facet_wrap(~Region, ncol=1) + 
  coord_flip() + 
  scale_y_continuous(labels=percent) + 
  ylab('Percent') +
  theme_minimal() +
  theme(legend.position = "bottom") + 
  guides(fill = guide_legend(reverse=T))


#### Decision Config B

ggplot(employer_data_comp %>%
         filter(!is.na(multi_target), !is.na(NOC_Type), !is.na(Region)))+ 
  geom_bar(mapping = aes(multi_target,fill = NOC_Type), position='fill',
           color = "grey40") +
  labs(title="Decision Breakdown by NOC Type, Region", 
       x = "",
       y = "",
       fill = "") + 
  facet_wrap(~Region, ncol=1) + 
  coord_flip() + 
  scale_y_continuous(labels=percent) + 
  ylab('Percent') +
  scale_fill_manual(values = 
                      rev(c("#1f83b4", "#ff7f0e", "#d63a3a", "#12a2a8", "#2ca030")))+
  theme_minimal() +
  theme(legend.position = "bottom") + 
  guides(fill = guide_legend(reverse=T))


#### Decision Config C  

ggplot(employer_data_comp %>%
         filter(!is.na(target_three), !is.na(NOC_Type), !is.na(Region)))+ 
  geom_bar(mapping = aes(target_three,fill = NOC_Type), position='fill',
           color = "grey40") +
  labs(title="Decision Breakdown by NOC Type, Region",
       x = "",
       y = "",
       fill = "") + 
  facet_wrap(~Region, ncol=1) + 
  coord_flip() + 
  scale_y_continuous(labels=percent) + 
  ylab('Percent') +
  scale_fill_manual(values = 
                      rev(c("#1f83b4", "#ff7f0e", "#d63a3a", "#12a2a8", "#2ca030")))+
  theme_minimal() +
  theme(legend.position = "bottom") + 
  guides(fill = guide_legend(reverse=T))


#### Decision Config D  

ggplot(employer_data_comp %>%
         filter(!is.na(target_down), !is.na(NOC_Type), !is.na(Region)))+ 
  geom_bar(mapping = aes(target_down,fill = NOC_Type), position='fill',
           color = "grey40") +
  labs(title="Decision Breakdown by NOC Type, Region",
       x = "",
       y = "",
       fill = "") + 
  facet_wrap(~Region, ncol=1) + 
  coord_flip() + 
  scale_y_continuous(labels=percent) + 
  ylab('Percent') +
  scale_fill_manual(values = 
                      rev(c("#1f83b4", "#ff7f0e", "#d63a3a", "#12a2a8", "#2ca030")))+
  theme_minimal() +
  theme(legend.position = "bottom") + 
  guides(fill = guide_legend(reverse=T))