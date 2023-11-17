population_at_each_year <- readxl::read_excel(
  "C:\\GitHub\\Core-Segments-Over-Time\\outputs\\2023-11-07-2023-09-01-trans-matrix-option-b-ons-scaled-pop.xlsx",
  sheet="outputs"
)

inner_trans_matrix_list <- readxl::read_excel(
  "C:\\GitHub\\Core-Segments-Over-Time\\outputs\\2023-11-07-2023-09-01-trans-matrix-option-b-ons-scaled-pop.xlsx",
  sheet="inner_trans_matrix"
) %>% as.matrix

links_df <- make_links_df(population_at_each_year,
                          inner_trans_matrix_list)


links_df <- tribble(
  ~year, ~source_node,~source_cs_name, ~target_node, ~transition_prop, ~transition_amount,
  1,1,"CS1",3,0.5,100,
  1,1,"CS1",4,0.5,100,
  1,2,"CS2",3,0.2,200,
  1,2,"CS2",4,0.8,800,
  2,3,"CS1",5,0.1,30,
  2,3,"CS1",6,0.8,240,
  2,4,"CS2",5,0.6,540,
  2,4,"CS2",6,0.4,360,
  3,5,"CS1",7,0.5,285,
  3,5,"CS1",8,0.5,285,
  3,6,"CS2",7,0.01,6,
  3,6,"CS2",8,0.99,594)

end_pops <- links_df %>%
  group_by(target_node) %>%
  summarise(t=sum(transition_amount))

links_2 <-
  full_join(
    links_df %>% filter(year!=max(year)),
    links_df %>% filter(year!=min(year)) %>% mutate(year_m1 = year-1),
    by=c("target_node"="source_node","year"="year_m1"),
    suffix = c("_x","_y"),
    relationship="many-to-many") %>%
  mutate(transition_amount = transition_amount_x * transition_prop_y) %>%
  group_by(year,source_node,source_cs_name_x,target_node_y) %>%
  summarise(transition_amount = sum(transition_amount),.groups="drop") %>%
  select(year,
         source_node,
         source_cs_name = source_cs_name_x,
         target_node = target_node_y,
         transition_amount) %>%
  filter(year/2 != floor(year/2))


nodes_collapse <- c(pull(links_2,source_node),pull(links_2,target_node)) %>% unique()
names(nodes_collapse) <- 1:length(nodes_collapse)
nodes_collapse <- setNames(names(nodes_collapse), nodes_collapse)


links_2 <- links_2 %>% mutate(a= unname(nodes_collapse[source_node]),
                              b = unname(nodes_collapse[target_node]))

ggplot2::ggplot(links_2) +
  ggsankey::geom_sankey(
    mapping = aes(x = year,
                  next_x = year+1,
                  value = transition_amount,
                  node = source_node,
                  next_node = target_node,
                  fill = factor(source_cs_name),
                  label = year),
    flow.alpha = 0.6,
    node.color = "gray30",
    type = "alluvial") +
  # add first year
  bnssgtheme() +
  scale_fill_bnssg(reverse=T) +
  # don't show y limits as gaps between Core Segments make scale off
  scale_y_continuous(breaks = NULL) +
  labs(
    title = "Inner Transitions between Core Segment (CS) groups",
    subtitle = "NOT SHOWN - births / net_migration / deaths",
    x="Year into the future",
    y="Population",
    fill = "Core Segment (bigger number = higher CMS)",
    caption = paste0(Sys.Date(),"\n",
                     "dpm R package fn create_sankey")
  )
ggplot2::ggplot(links_df) +
  ggsankey::geom_sankey(
    mapping = aes(x = year,
                  next_x = year+1,
                  value = transition_amount,
                  node = source_node,
                  next_node = target_node,
                  fill = factor(source_cs_name),
                  label = year),
    flow.alpha = 0.6,
    node.color = "gray30",
    type = "alluvial") +
  # add first year
  bnssgtheme() +
  scale_fill_bnssg(reverse=T) +
  # don't show y limits as gaps between Core Segments make scale off
  scale_y_continuous(breaks = NULL) +
  labs(
    title = "Inner Transitions between Core Segment (CS) groups",
    subtitle = "NOT SHOWN - births / net_migration / deaths",
    x="Year into the future",
    y="Population",
    fill = "Core Segment (bigger number = higher CMS)",
    caption = paste0(Sys.Date(),"\n",
                     "dpm R package fn create_sankey")
  )

my_sankey_plot

if(save){
  filename <- here::here("figs",paste0(Sys.Date(),"-sankey.png"))
  ggsave(
    filename = filename,
    my_sankey_plot,
    width = 10,
    height = 5)
  print(paste0("plot saved at: ",file_name))
}

return(my_sankey_plot)




