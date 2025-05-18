# Exploratory Data Analysis 

# summary de Stores------------------------------------------------------------
summary(stores)
summary(features)
summary(train)

# tamanho médio, mínimo e máximo e quantidade de lojas por tipo ---------------
stores |> 
  group_by(Type) |> 
  summarise(mean = mean(Size),
            min = min(Size),
            max = max(Size),
            n = n())

# Ranking das maiores Lojas por tipo organizado de forma descendente------
stores |> 
  group_by(Store, Type) |> 
  arrange(desc(Size)) |>  
  bar_chart(x = Store, 
            y = Size,
            top_n = 10,
            facet = Type,
            fill = Type) +
  xlab("Loja") +
  ylab("Tamanho (Mil Ft²)") +
  scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3)) +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "darkgray"),
        legend.position = "none") +
  scale_fill_manual(values = c("#004c91", "#f47321", "#ffc220"))

# Ranking de Lojas por média de vendas organizado de forma descendente --------
train |> 
  group_by(Store, Type) |> 
  summarise(sales = mean(Weekly_Sales)) |> 
  arrange(desc(sales)) |> 
  bar_chart(x = Store, 
            y = sales, 
            fill = Type,
            top_n = 10) +
  labs(x = "Loja", y = "Média de Vendas (USD)", fill = "Tipo") +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "darkgray"),
        legend.position = "right") +
  scale_fill_manual(values = c("#004c91", "#f47321", "#ffc220"))


# Valor médio de vendas por tipo de loja --------------------------------------------
train |>
  group_by(Type) |> 
  summarise(sales = mean(Weekly_Sales)) |> 
  arrange(desc(sales)) |> 
  ggplot() +
  aes(x = Type, y = sales, fill = Type) +
  geom_bar(stat = "identity") +
  ylab("Média de Vendas (USD) / dia") +
  xlab("Tipo") +
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3)) +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "darkgray"),
        legend.position = "none")  +
  scale_fill_manual(values = c("#004c91", "#f47321", "#ffc220"))

# Ranking de departamento por média de vendas --------------------------------- 
train |> 
  group_by(Dept) |> 
  summarise(sales = mean(Weekly_Sales)) |> 
  arrange(desc(sales)) |> 
  bar_chart(x = Dept, 
            y = sales, 
            bar_color = "#004c91",
            top_n = 10) +
  xlab("Departamento") +
  ylab("Média de Vendas (USD) / dia") +
  scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3)) +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "darkgray"),
        legend.position = "none")  +
  scale_fill_manual(values = c("#004c91", "#f47321", "#ffc220"))

# Distribuição de vendas por tipo ---------------------------------------------
train |> 
  group_by(Type, Date) |> 
  summarise(sales = sum(Weekly_Sales)) |> 
  ggplot(aes(x=sales, fill = Type)) +
  geom_histogram() +
  facet_grid(Type~.) + 
  #scale_x_log10() +
  scale_x_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  xlab("Vendas (USD)") +
  ylab("Tipo") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "darkgray"),
        legend.position = "none")  +
  scale_fill_manual(values = c("#004c91", "#f47321", "#ffc220"))

# Boxplot de vendas por tipo --------------------------------------------------
train |> 
  group_by(Type, Date) |> 
  summarise(sales = sum(Weekly_Sales)) |> 
  ggplot(aes(x=sales, fill = Type)) +
  geom_boxplot() +
  facet_grid(Type~.) +
  scale_x_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  xlab("Vendas (USD)") +
  ylab("Tipo") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "darkgray"),
        legend.position = "none")  +
  scale_fill_manual(values = c("#004c91", "#f47321", "#ffc220"))


# Gráfico de linha vendas total -----------------------------------------------
train |> 
  group_by(Date) |> 
  summarise(sales = sum(Weekly_Sales)) |> 
  ggplot() +
  aes(x = Date, y = sales) |> 
  geom_line(color = "#004c91") +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  ylab("Vendas (USD)") +
  xlab("Data") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "darkgray"),
        legend.position = "none")

# Gráfico de linha vendas total por tipo --------------------------------------
train |>
  group_by(Date, Type) |> 
  summarise(sales = sum(Weekly_Sales)) |> 
  ggplot() +
  aes(x = Date, y = sales, color = Type) |> 
  geom_line() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "darkgray"),
        legend.position = "none") +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  ylab("Vendas (USD)") +
  xlab("Data") +
  scale_x_date(date_breaks = "4 weeks") +
  facet_grid(Type~.) +
  scale_color_manual(values = c("#004c91", "#f47321", "#ffc220"))

# Gráfico de sazonalidade mensal por tipo de loja -----------------------------
train |>
  select(Date, Type, Weekly_Sales) |>
  group_by(Date, Type) |> 
  summarise(sales = sum(Weekly_Sales)) |> 
  as_tsibble(index = Date,
             key = Type) |> 
  gg_season(sales) +
  xlab("Mês") +
  ylab("Total de Vendas") +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  scale_x_date(date_breaks = "1 month", minor_breaks = NULL, date_labels = "%B") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "darkgray"),
        legend.position = "bottom")



# Fig 9.Gráfico de decomposição da série de vendas -----------------------------
train |> 
  select(Date, Weekly_Sales) |>
  mutate(week = yearweek(Date)) |> 
  group_by(week) |>
  summarise(sales = sum(Weekly_Sales)) |> 
  as_tsibble(index = week) |> 
  model(
    stl = STL(sales)
  ) |> 
  components() |> 
  autoplot() +
  xlab("Semana") +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))
  

# Fig 10. Gráfico de auto-correlação da série de vendas ------------------------
train |> 
  select(Date, Weekly_Sales) |>
  mutate(week = yearweek(Date)) |> 
  group_by(week) |>
  summarise(sales = sum(Weekly_Sales)) |> 
  as_tsibble(index = week) |> 
  gg_tsdisplay(sales, plot_type = "partial", lag_max = 52) +
  xlab("Semana") +
  ylab("Vendas (USD)")
  

# fig 11. Gráfico de lags da série de vendas -----------------------------------
train |> 
  select(Date, Weekly_Sales) |> 
  group_by(Date) |>
  summarise(sales = sum(Weekly_Sales)) |> 
  as_tsibble(index = Date) |>
  gg_lag(sales, geom = "point", lags = c(1,2,4, 5,39,48,52)) +
  ylab("Vendas (USD)") +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  scale_x_continuous(labels = label_number(suffix = " M", scale = 1e-6))

# fig 12. Correlação entre as variáveis ---------------------------------------
train |> 
  group_by(Date) |>
  summarise(sales = mean(Weekly_Sales),
            size = mean(Size),
            temp = mean(Temperature),
            fuel = mean(Fuel_Price),
            mk1 = mean(MarkDown1),
            mk2 = mean(MarkDown2),
            mk3 = mean(MarkDown3),
            mk4 = mean(MarkDown4),
            mk5 = mean(MarkDown5),
            cpi = mean(CPI),
            unemp = mean(Unemployment)) |> 
  select(2:12) |> 
  GGally::ggpairs()

# no dias que são feriado as vendas são maiores em média que em dias que não são? 

train |> 
  #filter(id == "1_1") |>
  select(Weekly_Sales, IsHoliday) |>
  group_by(IsHoliday) |> 
  summarise(mean_sales = mean(Weekly_Sales)) |> 
  ggplot() +
    aes(x = IsHoliday, y = mean_sales) +
    geom_col()

# teste para toda a população

train %>% # precisa usar o pipe do tidyverse
  #filter(id == "12_3") %>%
  select(Weekly_Sales, IsHoliday) %>% 
  t.test(Weekly_Sales ~ IsHoliday, data = .)

# as vendas médias são diferentes entre o tipo de loja? 

train |> 
  select(Weekly_Sales, Type) |>
  group_by(Type) |> 
  summarise(mean_sales = mean(Weekly_Sales)) |> 
  ggplot() +
  aes(x = Type, y = mean_sales) +
  geom_col()

# teste para toda a população

train %>% # precisa usar o pipe do tidyverse
  filter(Type %in% c("A","B")) %>%
  select(Weekly_Sales, Type) %>% 
  t.test(Weekly_Sales ~ Type, data = .)

train %>% # precisa usar o pipe do tidyverse
  filter(Type %in% c("A","C")) %>%
  select(Weekly_Sales, Type) %>% 
  t.test(Weekly_Sales ~ Type, data = .)

train %>% # precisa usar o pipe do tidyverse
  filter(Type %in% c("B","C")) %>%
  select(Weekly_Sales, Type) %>% 
  t.test(Weekly_Sales ~ Type, data = .)

glimpse(train)

# A temperatura tem alguma correlação com o valor médio de vendas ?

train |>
  filter(id == "15_20") |> 
  #group_by(Date) |> 
  #summarise(mean_sales = mean(Weekly_Sales, na.rm = TRUE),
  #         mean_temperature = mean(Temperature, na.rm = TRUE)) |>
  ggplot() +
    aes(x = Weekly_Sales, y = Temperature) +
    geom_point()
  
sales <- train %>%
  filter(id == "15_20") %>%
  select(Weekly_Sales)

temp <- train %>%
  filter(id == "15_20") %>%
  select(Temperature)

cor(x = sales, y = temp)
