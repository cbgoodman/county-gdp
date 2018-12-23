library(tidyverse)
library(urbnmapr)
library(viridis)

# Add Data
county.gdp <- readr::read_csv("bea_county_gdp.csv", quote="\'", col_names=T)

# Calculate per capita measures
county.gdp$pc.gdp.2012 <- county.gdp$allindustries_2012/county.gdp$pop_2012
county.gdp$pc.gdp.2013 <- county.gdp$allindustries_2013/county.gdp$pop_2013
county.gdp$pc.gdp.2014 <- county.gdp$allindustries_2014/county.gdp$pop_2014
county.gdp$pc.gdp.2015 <- county.gdp$allindustries_2015/county.gdp$pop_2015

# Create full names for future labels
county.gdp$full_name <- ifelse(county.gdp$state_name=="AK",
  str_c(county.gdp$county_name,", ", county.gdp$state_name),
  ifelse(county.gdp$state_name=="LA",
    str_c(county.gdp$county_name, " Parish",", ", county.gdp$state_name) ,str_c(county.gdp$county_name, " County",", ", county.gdp$state_name)))

county.gdp$int <- paste(county.gdp$specialize,county.gdp$metro, sep=".")
# Scatter Plot
p.2015 <- ggplot(county.gdp) +
  aes(x = log(pop_2015), y = log(pc.gdp.2015)) +
  geom_point(alpha = 0.75, size = 1.5, aes(shape = int, color = int)) +
  geom_text(aes(label=ifelse(log(pc.gdp.2015)>5.75,as.character(full_name),'')), hjust=0, vjust=0, nudge_x = 0.05, nudge_y = 0.05, size = 3, family="Open Sans Condensed Light") +
  geom_text(aes(label=ifelse(log(pc.gdp.2015)<2,as.character(full_name),'')), hjust=0, vjust=0, nudge_x = 0.05, nudge_y = 0.05, size = 3, family="Open Sans Condensed Light") +
  geom_smooth(method = "lm", se = FALSE, size=0.5, color="#2b2b2b", alpha=0.5) +
  # Shape and color
  scale_color_brewer(name = "Metro & Specialization",
    palette="Paired")+
  scale_shape_manual(name = "Metro & Specialization",
    values = rep(16:17,6)) +
  scale_x_continuous(breaks = c(6,8,10,12,14,16)) +
  guides(col = guide_legend(nrow = 2, title.position = "top")) +
  # Theming
  labs(
    title="County-level Gross Domestic Product",
    subtitle="Log per capita GDP & log population, All U.S. counties, 2015",
    caption="Author: Chris Goodman (@cbgoodman), Data: U.S. Bureau of Economic Analysis (experimental), U.S. Census Bureau & U.S. Department of Agricutlure Economic Research Service.",
    y="Log of Per Capita GDP",
    x="Log of Population") +
  theme_minimal(base_family="Open Sans Condensed Light") +
  theme(
    legend.position = "bottom",
    legend.title.align = 0.5,
    legend.text.align = 0) +
  # light, dotted major y-grid lines only
  theme(panel.grid=element_line())+
  theme(panel.grid.major.y=element_line(color="#2b2b2b", linetype="dotted", size=0.15))+
  theme(panel.grid.major.x=element_blank())+
  theme(panel.grid.minor.x=element_blank())+
  theme(panel.grid.minor.y=element_blank())+
  # light x-axis line only
  theme(axis.line=element_line())+
  theme(axis.line.y=element_blank())+
  theme(axis.line.x=element_blank())+
  # tick styling
  theme(axis.ticks=element_line())+
  theme(axis.ticks.x=element_blank())+
  theme(axis.ticks.y=element_blank())+
  theme(axis.ticks.length=unit(5, "pt"))+
  # x-axis labels
  theme(axis.text.x=element_text(size=10, hjust=0.95,vjust=0.2))+
  # breathing room for the plot
  theme(plot.margin=unit(rep(0.5, 4), "cm"))+
  # move the y-axis tick labels over a bit
  theme(axis.text.y=element_text(margin=margin(r=-5)))+
  theme(axis.text.x=element_text(margin=margin(r=-5)))+
  # make the plot title bold and modify the bottom margin a bit
  theme(plot.title=element_text(family="Open Sans Condensed Bold", margin=margin(b=15)))+
  # make the subtitle italic
  theme(plot.subtitle=element_text(family="Open Sans Condensed Light Italic"))+
  theme(plot.caption=element_text(size=8, hjust=0, margin=margin(t=15)))
ggsave(plot=p.2015, "gdp-2015-grouped.png", width=10, height=7, units="in", dpi="retina")

summary(lm(data=county.gdp, log(pc.gdp.2015)~log(pop_2015)))

# Mapping
county.gdp.data <- left_join(county.gdp, counties, by = "county_fips")

#################################################
# Pretty Breaks
pretty.breaks <- c(24,31,40,52)
# find min and max values of pop growth
minVal <- min(county.gdp.data$pc.gdp.2015, na.rm = T)
maxVal <- max(county.gdp.data$pc.gdp.2015, na.rm = T)
# compute pop growth labels
labels <- c()
brks <- c(minVal, pretty.breaks, maxVal)
# round the labels (actually, only the extremes)
labels <- c()
for(idx in 1:length(brks)){
  labels <- c(labels, paste0(round(brks[idx], 2),
  " – ",
  round(brks[idx + 1], 2)))
}
# Minus one label to remove the odd ending one
labels <- labels[1:length(labels)-1]

# Create new variable for fill
county.gdp.data$brks <- cut(county.gdp.data$pc.gdp.2015,
  breaks = brks,
  labels = labels,
  include.lowest = T)

p <- ggplot() +
  # County Map
  geom_polygon(data = county.gdp.data,
    mapping = aes(x = long, y = lat, group = group,
    fill = county.gdp.data$brks),
    #color = alpha("white", 1 / 2),
    #size = 0.2
  ) +
  # State Map
  geom_polygon(data = urbnmapr::states, mapping = aes(long, lat, group = group),
    fill = NA, color = "#ffffff", size = 0.25, alpha=0.5) +
  # Projection
  coord_map(projection = "polyconic") +
  scale_fill_viridis(
    option = "viridis",
    name = "Per capita GDP",
    discrete = T,
    direction = 1,
    begin=0.1,
    #end=0.9,
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = 'top',
      #reverse = F
  )) +
  # Theming
  theme_minimal(base_family = "Open Sans Condensed Light") +
  theme(
    legend.position = "bottom",
    legend.text.align = 0,
    legend.title.align = 0.5,
    plot.margin = unit(c(.5,.5,.2,.5), "cm")) +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    ) +
  theme(plot.title=element_text(family="Open Sans Condensed Bold", margin=margin(b=15))) +
  theme(plot.subtitle=element_text(family="Open Sans Condensed Light Italic")) +
  theme(plot.margin=unit(rep(0.5, 4), "cm")) +
  labs(x = "",
       y = "",
       title = "County-level Gross Domestic Product",
       subtitle = "Per capita GDP ($1,000s), U.S. counties, 2015",
       caption = "Author: Chris Goodman (@cbgoodman), Data: U.S. Bureau of Economic Analysis (experimental) & U.S. Census Bureau.")

ggsave(plot=p, "county-gdp-15.png", width=10, height=8, units="in", dpi="retina")
