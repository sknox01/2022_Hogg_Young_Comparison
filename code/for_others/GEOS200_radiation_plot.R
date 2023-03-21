# Radiation data Hogg - after running '0_load_data'

p <- ggplot() +
  geom_line(data = data.Hogg, aes(x = datetime, y = SWIN_1_1_1), size = 1) +
  geom_line(data = data.Hogg, aes(x = datetime, y = SWOUT_1_1_1),color = 'red', size = 1)
toWebGL(ggplotly(p))

# compute daily mean fluxes (using filled data for now)
data.daily <- data.Hogg %>%
  mutate(year = year(datetime),
         jday = yday(datetime)) %>%
  group_by(year,jday) %>%
  dplyr::summarize(SW_IN = mean(SWIN_1_1_1, na.rm = TRUE),
                   SW_OUT = mean(SWOUT_1_1_1, na.rm = TRUE),
                   LW_IN = mean(LWIN_1_1_1, na.rm = TRUE),
                   LW_OUT = mean(LWOUT_1_1_1, na.rm = TRUE),
                   date = first(datetime))

data.daily <- data.daily %>%
  mutate(NETRAD = (SW_IN-SW_OUT) + (LW_IN-LW_OUT),
         NETSW = SW_IN-SW_OUT,
         NETLW = LW_IN-LW_OUT,
         albedo = SW_OUT/SW_IN)

data.daily$date <- as.Date(data.daily$date)

p1 <- ggplot(data.daily, aes(x = date)) +
  geom_line(aes(y = albedo), size = 1)+
  scale_x_date(limits = as.Date(c("2022-03-07","2022-04-01"))) +
  ylim(0.2,0.8) + theme(axis.title.x=element_blank())
p1

ggsave("figures/albedo.pdf", width = 15, height = 6, units = "cm")

colors <- c("Q*" = "grey40", "K*" = "#E7B800","Kdown" = "#00AFBB", "L*" = "#FC4E07")

p2 <- ggplot(data.daily, aes(x = date)) +
  geom_line(aes(y = NETRAD,color = "Q*"), size = 1) +
  geom_line(aes(y = NETSW,color = "K*"), size = 1) +
  geom_line(aes(y = SW_IN,color = "Kdown"), size = 1)+
  geom_line(aes(y = NETLW,color = "L*"), size = 1) +
  scale_x_date(limits = as.Date(c("2022-03-07","2022-04-01"))) + 
  ylim(-120,300) + theme(axis.title.x=element_blank(),legend.position="bottom") +
  labs(y = "Energy flux density (W/m2)", color = "Legend") +
  scale_color_manual(values = colors)
p2

ggsave("figures/radiation.pdf", width = 15, height = 12, units = "cm")


figure <- ggarrange(p1, p2,
                    ncol = 1, nrow = 2)
figure

