library("stringr")  # funções de manipulação de chars
library("dplyr")    # gramática de wrangling
library("ggplot2")  # apresentação gráfica
library("tibble")   # data sets melhorados
library("magrittr") # semantica serializada de instruções
library("readxl")   # leitor de arquivos excel
#library("purrr")    # programação funcional

ds_hrvctr_nb_csv <- as_tibble(read.csv("ApprovedTimeReport_000_20222024.csv", header = TRUE, sep = ";", dec = ".")) %>%
  mutate(Work.Date = as.Date(Work.Date,"%m/%d/%Y"), 
         Date.TimeSheet= as.Date(Date.TimeSheet,"%m/%d/%Y"),
         YM = substr(as.character(Work.Date),1,7),
         Cost = Time.Actual*CostRate,
         Rev = Time.Actual*BillingRate) 

ds_Parpinelli <- ds_hrvctr_nb_csv %>% 
  filter(BillingRate == 0 & Resource.Name == "Andre Parpinelli-vk" & Year == 2023, str_detect(FunnelID,"NB-")) %>% 
  group_by(YM, Resource.Name) %>% 
  summarise(HorasNB = sum(Time.Actual, na.rm = T),
            CostNB = sum(Cost, na.rm = T)) %>%   
  arrange(YM)

ds_Sildemar <- ds_hrvctr_nb_csv %>% 
  filter(BillingRate == 0 & Resource.Name == "Sildemar Sousa-vk" & Year == 2023, str_detect(FunnelID,"NB-")) %>% 
  group_by(YM, Resource.Name) %>% 
  summarise(HorasNB = sum(Time.Actual, na.rm = T),
            CostNB = sum(Cost, na.rm = T)) %>%   
  arrange(YM)

sum(ds_Sildemar$HorasNB)
sum(ds_Sildemar$CostNB)

sum(ds_Parpinelli$HorasNB)
sum(ds_Parpinelli$CostNB)

sum(ds_temp$Cost)

ds_nb <- as_tibble(ds_hrvctr_nb_csv) %>%
  filter(BillingRate == 0, Year == 2023,
         (Company == "DMS" | Company == "Web Working" | Company == "Sietech"),
         str_detect(FunnelID,"NB-")) %>% 
  group_by(Company, Resource.Name) %>% 
  summarise(HorasNB = sum(Time.Actual, na.rm = T),
            CostNB = sum(Cost, na.rm = T)) %>% 
  arrange(Company, desc(HorasNB))

ds_b <- as_tibble(ds_hrvctr_nb_csv) %>%   
  filter(BillingRate != 0, Year == 2023,
         (Company == "DMS" | Company == "Web Working" | Company == "Sietech")) %>% 
  group_by(Company, Resource.Name) %>% 
  summarise(Horas = sum(Time.Actual, na.rm = T),
            Cost = sum(Cost, na.rm = T),
            Rev = sum(Rev, na.rm = T)) %>% 
  arrange(Company, desc(Horas)) %>% 
  merge(., ds_nb, by = "Resource.Name", all.x = F) %>% 
  select(Company = Company.x,
         Resource.Name,
         Hours = Horas,
         HoursNB = HorasNB,
         Cost,
         CostNB,
         Rev) %>% 
  mutate(PercHNB = round(((HoursNB/(Hours+HoursNB))*100) ,2),
         PercCNB = round(((CostNB/(Cost+CostNB))*100) ,2),
         tot_h = sum(ds_b$Horas_B),
         tot_hnb = sum(ds_b$Horas_NB),
         tot_c = sum(ds_b$Custo_B),
         tot_cnb = sum(ds_b$Custo_NB),
         PercHT = round((Hours/tot_h)*100 ,2),
         PercHNBT = round((HoursNB/tot_hnb)*100 ,2),
         PercCT = round((Cost/tot_c)*100 ,2),
         PercCNBT = round((CostNB/tot_cnb)*100 ,2),
         LRev = round((Rev-(Cost+CostNB)) ,2),
         Cost = round(Cost, 2),
         CostNB = round(CostNB, 2),
         CRev = as.character(format(Rev, trim = T, nsmall = 2, big.mark = ".", big.interval = 3L, decimal.mark = ",")),
         CLRev = as.character(format(LRev, trim = T, nsmall = 2, big.mark = ".", big.interval = 3L, decimal.mark = ",")),
         Marg = round((LRev/Rev)*100, 2)
         ) %>% 
  select(Company,
         Resource.Name,
         Horas_NB = HoursNB,
         Pct_Horas_NB = PercHNB,
         Pct_Horas_NB_Tot = PercHNBT,
         Horas_B = Hours,
         Pct_Horas_B_Tot = PercHT,         
         Custo_NB = CostNB,
         Pct_Custo_NB = PercCNB,
         Pct_Custo_NB_Tot = PercCNBT,
         Custo_B = Cost,
         Pct_Custo_B_Tot = PercCT,         
         Receita = Rev,
         Receita_C = CRev,
         Receita_L = LRev,
         Receita_CL = CLRev,
         Marg)
  
ls(ds_b)

ds_nb_all <- as_tibble(ds_hrvctr_nb_csv) %>%
  filter(BillingRate == 0, Year == 2023,
         (Company != "DMS" & Company != "Web Working" & Company != "Sietech"),
         str_detect(FunnelID,"NB-")) %>% 
  group_by(Company, Resource.Name) %>% 
  summarise(HorasNB = sum(Time.Actual, na.rm = T),
            CostNB = sum(Cost, na.rm = T)) %>% 
  arrange(Company, desc(HorasNB))

ds_b_all <- as_tibble(ds_hrvctr_nb_csv) %>%   
  filter(BillingRate != 0, Year == 2023,
         (Company != "DMS" & Company != "Web Working" & Company != "Sietech")) %>% 
  group_by(Company, Resource.Name) %>% 
  summarise(Horas = sum(Time.Actual, na.rm = T),
            Cost = sum(Cost, na.rm = T),
            Rev = sum(Rev, na.rm = T)) %>% 
  arrange(Company, desc(Horas)) %>% 
  merge(., ds_nb_all, by = "Resource.Name", all.x = F) %>% 
  select(Company = Company.x,
         Resource.Name,
         Hours = Horas,
         HoursNB = HorasNB,
         Cost,
         CostNB,
         Rev) %>% 
  mutate(PercHNB = round(((HoursNB/(Hours+HoursNB))*100) ,2),
         PercCNB = round(((CostNB/(Cost+CostNB))*100) ,2),
         tot_h = sum(ds_b_all$Horas_B),
         tot_hnb = sum(ds_b_all$Horas_NB),
         tot_c = sum(ds_b_all$Custo_B),
         tot_cnb = sum(ds_b_all$Custo_NB),
         PercHT = round((Hours/tot_h)*100 ,2),
         PercHNBT = round((HoursNB/tot_hnb)*100 ,2),
         PercCT = round((Cost/tot_c)*100 ,2),
         PercCNBT = round((CostNB/tot_cnb)*100 ,2),
         LRev = round((Rev-(Cost+CostNB)) ,2),
         Cost = round(Cost, 2),
         CostNB = round(CostNB, 2),
         CRev = as.character(format(Rev, trim = T, nsmall = 2, big.mark = ".", big.interval = 3L, decimal.mark = ",")),
         CLRev = as.character(format(LRev, trim = T, nsmall = 2, big.mark = ".", big.interval = 3L, decimal.mark = ",")),
         Marg = round((LRev/Rev)*100, 2)
  ) %>% 
  select(Company,
         Resource.Name,
         Horas_NB = HoursNB,
         Pct_Horas_NB = PercHNB,
         Pct_Horas_NB_Tot = PercHNBT,
         Horas_B = Hours,
         Pct_Horas_B_Tot = PercHT,         
         Custo_NB = CostNB,
         Pct_Custo_NB = PercCNB,
         Pct_Custo_NB_Tot = PercCNBT,
         Custo_B = Cost,
         Pct_Custo_B_Tot = PercCT,         
         Receita = Rev,
         Receita_C = CRev,
         Receita_L = LRev,
         Receita_CL = CLRev,
         Marg)

v_Tot_Horas_NB <- sum(ds_b$Horas_NB) + sum(ds_b_all$Horas_NB)
v_Pct_Horas_NB_DT_All <- round(sum(ds_b$Horas_NB)/v_Tot_Horas_NB * 100, 2)
v_Tot_Horas_B <- sum(ds_b$Horas_B) + sum(ds_b_all$Horas_B)
v_Pct_Horas_B_DT_All <- round(sum(ds_b$Horas_B)/v_Tot_Horas_B * 100, 2)

v_Tot_Custo_NB <- sum(ds_b$Custo_NB) + sum(ds_b_all$Custo_NB)
v_Pct_Custo_NB_DT_All <- round(sum(ds_b$Custo_NB)/v_Tot_Custo_NB * 100, 2)
v_Tot_Custo_B <- sum(ds_b$Custo_B) + sum(ds_b_all$Custo_B)
v_Pct_Custo_B_DT_All <- round(sum(ds_b$Custo_B)/v_Tot_Custo_B * 100, 2)

v_Tot_Receita <- sum(ds_b$Receita) + sum(ds_b_all$Receita)
v_Pct_Receita_DT_All <- round(sum(ds_b$Receita)/v_Tot_Receita * 100, 2)

#rm(ds_hrvctr_nb_csv)

ls(ds_hrvctr_nb_csv)

#g_NB_all <- ggplot(data = ds_b_all, 
g_NB <- ggplot(data = ds_b, 
               aes(x = Resource.Name)) +
#  geom_bar(aes(y = Horas_B),
#           position = position_nudge(x = -0.225),
#           width = 0.35, alpha=0.25,
#           fill = "blue",
#           stat = "sum",
#           size = 1) +
  geom_bar(aes(y = Horas_NB, fill = ifelse(Marg <= 0, 0, Marg)),
#           position = position_nudge(x = 0.225),
#           width = 0.35, alpha=0.25,
#           fill = "red",
           stat = "sum",
           size = 1) +
  scale_fill_gradient(name="Margem",  
                      low = "red", 
                      high = "green",
                      guide = "colourbar") +
  geom_text(aes(y = Horas_NB+50,
                label = Receita_CL),
            col = "darkgreen",
            size = 3) +
  geom_text(aes(y = -50,
                label = Horas_NB),
            col = "darkred",
            size = 3) +  
  labs(title = 'Horas Non-bilable', x = 'Recurso', y = 'Hrs') +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   size = 9, 
                                   hjust = 1))

plot(g_NB)
plot(g_NB_all)

ds_marg <- c(ds_b$Resource.Name,ds_b$Marg)

ds_fatura <- as_tibble(ds_hrvctr_nb_csv) %>%
  filter(Year == 2023) %>%
  group_by(Company, Resource.Name, YM) %>%
  summarise(Horas = sum(Time.Actual, na.rm = T),
            Cost = sum(Cost, na.rm = T),
            Rev = sum(Rev, na.rm = T)) %>%
  arrange(Company, Resource.Name, YM) %>% 
  mutate(Delta = as.character(format(Rev - Cost, trim = T, nsmall = 2, big.mark = ".", big.interval = 3L, decimal.mark = ",")))
  

# Tech
# 24h Apoio a farming e hunting de novas oportunidades, incluindo QAD Tec - Parpinelli
# 4h  Apoio a outras áreas (VPN, .net) (pequenos volumes)                 - Todos
# 12h Suporte de VMs Product Center                                       - Diogo

## Projetos Tec
# Soluções para Vockan (Suporte Tec. a Desenvolvimento V-control...) 
# Soluções Cloud Vockan
# Certificações, estudos de novas tecnologias e nivelamento de conhecimento no time

# Dev
# 32h Apoio a farming e hunting de novas oportunidades                    - Sildemar
# 4h  Apoio a Tec.                                                        - Sildemar
# 4h  Gestão com áreas                                                    - Sildemar

# Projetos Dev
# Soluções para Vockan (API, V-control...)
# Certificações, estudos de novas tecnologias e nivelamento de conhecimento no time



