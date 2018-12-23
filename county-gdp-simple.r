library(tidyverse)

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
  geom_point(alpha = 0.5, size = 1, color = "#E69F00") +
  geom_text(aes(label=ifelse(log(pc.gdp.2015)>5.75,as.character(full_name),'')), hjust=0, vjust=0, nudge_x = 0.05, nudge_y = 0.05, size = 3, family="Open Sans Condensed Light") +
  geom_text(aes(label=ifelse(log(pc.gdp.2015)<2,as.character(full_name),'')), hjust=0, vjust=0, nudge_x = 0.05, nudge_y = 0.05, size = 3, family="Open Sans Condensed Light") +
  geom_smooth(method = "lm", se = FALSE, size=0.5, color="#2b2b2b", alpha=0.5) +
  # Theming
  labs(
    title="County-level Gross Domestic Product",
    subtitle="Log per capita GDP & log population, All U.S. counties, 2015",
    caption="Author: Chris Goodman (@cbgoodman), Data: U.S. Bureau of Economic Analysis (experimental) & U.S. Census Bureau",
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
ggsave(plot=p.2015, "gdp-2015.png", width=10, height=7, units="in", dpi="retina")

summary(lm(data=county.gdp, log(pc.gdp.2015)~log(pop_2015)))
