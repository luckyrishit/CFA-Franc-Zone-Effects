rm(list = ls())

library(readxl)
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(car)

data1 <- read_xlsx("/Users/rishitpothu/Desktop/college/year 4/semester 1/ECON0128 Statistical Programming for Social Data Science/first coursework due 18 nov/srs/(1) P_Data_Extract_From_World_Development_Indicators.xlsx")
data2 <- read_excel("/Users/rishitpothu/Desktop/college/year 4/semester 1/ECON0128 Statistical Programming for Social Data Science/first coursework due 18 nov/srs/(4) API_SI.POV.GINI_DS2_en_excel_v2_211.xls", sheet = "Data", skip = 3) #in the data sheet # Remove the first four rows from data2 and set the column names from the fifth row
data3 <- read.csv("/Users/rishitpothu/Desktop/college/year 4/semester 1/ECON0128 Statistical Programming for Social Data Science/first coursework due 18 nov/srs/(5) human-development-index.csv")


#Merge all datasets into one

view(data2)
#delete last 5 rows from data1
data1 <- data1 %>% 
    slice(1:(n() - 5))

view(data1)

# Rename columns in data1 to remove [YRxxxx] from year columns and ensure years are in numeric format
colnames(data1) <- gsub("\\[YR[0-9]{4}\\]", "", colnames(data1))
colnames(data1)[-c(1:4)] <- as.numeric(colnames(data1)[-c(1:4)])

view(data1)

# Extract unique country codes from data1
data1_country_codes <- data1 %>% 
    distinct(`Country Code`) %>% 
    pull(`Country Code`)

# Filter data2 for matching country codes
filtered_data2 <- data2 %>%
    filter(`Country Code` %in% data1_country_codes) %>%
    select(`Country Name`, `Country Code`, `Indicator Name`, `Indicator Code`, starts_with("19"), starts_with("20"))

view(filtered_data2)
# Select relevant columns and filter for years 1974 - 2023
filtered_data2 <- filtered_data2 %>%
    select(`Country Name`, `Country Code`, `Indicator Name`, `Indicator Code`, starts_with("19"), starts_with("20")) %>%
    select(`Country Name`, `Country Code`, `Indicator Name`, `Indicator Code`, `1974`:`2023`)

# Ensure the year columns are in numeric format
colnames(filtered_data2)[5:ncol(filtered_data2)] <- as.numeric(colnames(filtered_data2)[5:ncol(filtered_data2)])

view(filtered_data2)

# Rename columns in filtered_data2
filtered_data2 <- filtered_data2 %>%
        rename(
                `Series Name` = `Indicator Name`,
                `Series Code` = `Indicator Code`
        )
view(filtered_data2)

# Ensure the year columns in data1 are numeric
data1 <- data1 %>%
    mutate(across(`1974`:`2023`, as.numeric))

# Ensure the year columns in filtered_data2 are numeric
filtered_data2 <- filtered_data2 %>%
    mutate(across(`1974`:`2023`, as.numeric))

# Append filtered_data2's data to data1
data1 <- bind_rows(data1, filtered_data2)

# Ensure the data is sorted by Country Name and Year
data1 <- data1 %>%
    arrange(`Country Name`, `Country Code`)

view(data1)

# Check column names of data3
colnames(data3)

# Reshape data3 to wide format with years as columns
data3_wide <- data3 %>%
  pivot_wider(names_from = "Year", values_from = "Human.Development.Index")

# Ensure the year columns in data3_wide are numeric
colnames(data3_wide)[-c(1:2)] <- as.numeric(colnames(data3_wide)[-c(1:2)])

view(data3_wide)

# Rename columns in data3_wide
data3_wide <- data3_wide %>%
    rename(
        `Country Name` = `Entity`,
        `Country Code` = `Code`
    )

# Add Series Name and Series Code columns
data3_wide <- data3_wide %>%
    mutate(
        `Series Name` = "Human Development Index",
        `Series Code` = "Human Development Index"
    )

# Reorder columns to place Series Name and Series Code beside Country Code
data3_wide <- data3_wide %>%
    select(`Country Name`, `Country Code`, `Series Name`, `Series Code`, everything())

view(data3_wide)

# Extract unique country codes from data1
data1_country_codes <- data1 %>% 
    distinct(`Country Code`) %>% 
    pull(`Country Code`)

# Filter data3_wide for matching country codes
filtered_data3 <- data3_wide %>%
    filter(`Country Code` %in% data1_country_codes) %>%
    select(`Country Name`, `Country Code`, `Series Name`, `Series Code`, starts_with("19"), starts_with("20")) %>%
    select(`Country Name`, `Country Code`, `Series Name`, `Series Code`, `1990`:`2022`)


# Ensure the year columns in filtered_data3 are numeric
filtered_data3 <- filtered_data3 %>%
    mutate(across(`1990`:`2022`, as.numeric))

# Append filtered_data3's data to data1
data1 <- bind_rows(data1, filtered_data3)

# Ensure the data is sorted by Country Name and Year
data1 <- data1 %>%
    arrange(`Country Name`, `Country Code`)

view(data1)

write.csv(data1, "/Users/rishitpothu/Desktop/college shit/year 4/semester 1/ECON0128 Statistical Programming for Social Data Science/first coursework due 18 nov/srs/merged_data_final.csv", row.names = FALSE)

#for checkpoint purposes I saved it
final_data <- read.csv("/Users/rishitpothu/Desktop/college shit/year 4/semester 1/ECON0128 Statistical Programming for Social Data Science/first coursework due 18 nov/srs/merged_data_final.csv", check.names = FALSE) #, check.names = FALSE to get rid of X's beside years
view(final_data)

# Remove the Series Code column from final_data
final_data <- final_data %>%
    select(-`Series Code`)

view(final_data)

# Pivot the data to have years in rows and indicators in columns
final_data_long <- final_data %>%
    pivot_longer(cols = `1974`:`2023`, names_to = "Year", values_to = "Value") %>%
    pivot_wider(names_from = `Series Name`, values_from = `Value`)

# Ensure the Year column is numeric
final_data_long <- final_data_long %>%
    mutate(Year = as.numeric(Year))

view(final_data_long)

write.csv(final_data_long, "/Users/rishitpothu/Desktop/college shit/year 4/semester 1/ECON0128 Statistical Programming for Social Data Science/first coursework due 18 nov/srs/merged_data_final_long.csv", row.names = FALSE)

### Merging Finished ###


data <- read.csv("/Users/rishitpothu/Desktop/college shit/year 4/semester 1/ECON0128 Statistical Programming for Social Data Science/first coursework due 18 nov/srs/merged_data_final_long.csv")

view(data)

str(data)

head(data)

# Summary of missing values
colSums(is.na(data))

#seperate CFA franc nations and non-CFA France nations
data <- data %>%
    mutate(
        `CFA Group` = case_when(
            `Country.Code` %in% c("BEN", "BFA", "CIV", "GNB", "MLI", "NER", "SEN", "TGO", "CMR", "CAF", "COG", "GAB", "GNQ", "TCD") ~ "CFA",
            TRUE ~ "Non-CFA"
        )
    )

#summary of all variables
summary_stats <- data %>%
    group_by(`CFA Group`) %>%
    summarise(
        n = n(),
        mean_gdp_growth = mean(`GDP.growth..annual...`, na.rm = TRUE),
        median_gdp_growth = median(`GDP.growth..annual...`, na.rm = TRUE),
        min_gdp_growth = min(`GDP.growth..annual...`, na.rm = TRUE),
        max_gdp_growth = max(`GDP.growth..annual...`, na.rm = TRUE),
        mean_gini = mean(`Gini.index`, na.rm = TRUE),
        median_gini = median(`Gini.index`, na.rm = TRUE),
        min_gini = min(`Gini.index`, na.rm = TRUE),
        max_gini = max(`Gini.index`, na.rm = TRUE),
        mean_inflation = mean(`Inflation..consumer.prices..annual...`, na.rm = TRUE),
        median_inflation = median(`Inflation..consumer.prices..annual...`, na.rm = TRUE),
        min_inflation = min(`Inflation..consumer.prices..annual...`, na.rm = TRUE),
        max_inflation = max(`Inflation..consumer.prices..annual...`, na.rm = TRUE),
        mean_trade_dependence = mean(`Trade.Dependency..annual...`, na.rm = TRUE),
        median_trade_dependence = median(`Trade.Dependency..annual...`, na.rm = TRUE),
        min_trade_dependence = min(`Trade.Dependency..annual...`, na.rm = TRUE),
        max_trade_dependence = max(`Trade.Dependency..annual...`, na.rm = TRUE),
        mean_external_debt = mean(`External.debt.stocks....of.GNI.`, na.rm = TRUE),
        median_external_debt = median(`External.debt.stocks....of.GNI.`, na.rm = TRUE),
        min_external_debt = min(`External.debt.stocks....of.GNI.`, na.rm = TRUE),
        max_external_debt = max(`External.debt.stocks....of.GNI.`, na.rm = TRUE)
    )

summary_stats_shock_years <- data %>%
    filter(Year %in% c(1994, 1999, 2008, 2014, 2020, 2022)) %>%
    group_by(`CFA Group`) %>%
    summarise(
        n = n(),
        mean_gdp_growth = mean(`GDP.growth..annual...`, na.rm = TRUE),
        median_gdp_growth = median(`GDP.growth..annual...`, na.rm = TRUE),
        min_gdp_growth = min(`GDP.growth..annual...`, na.rm = TRUE),
        max_gdp_growth = max(`GDP.growth..annual...`, na.rm = TRUE),
        mean_gini = mean(`Gini.index`, na.rm = TRUE),
        median_gini = median(`Gini.index`, na.rm = TRUE),
        min_gini = min(`Gini.index`, na.rm = TRUE),
        max_gini = max(`Gini.index`, na.rm = TRUE),
        mean_inflation = mean(`Inflation..consumer.prices..annual...`, na.rm = TRUE),
        median_inflation = median(`Inflation..consumer.prices..annual...`, na.rm = TRUE),
        min_inflation = min(`Inflation..consumer.prices..annual...`, na.rm = TRUE),
        max_inflation = max(`Inflation..consumer.prices..annual...`, na.rm = TRUE),
        mean_trade_dependence = mean(`Trade.Dependency..annual...`, na.rm = TRUE),
        median_trade_dependence = median(`Trade.Dependency..annual...`, na.rm = TRUE),
        min_trade_dependence = min(`Trade.Dependency..annual...`, na.rm = TRUE),
        max_trade_dependence = max(`Trade.Dependency..annual...`, na.rm = TRUE),
        mean_external_debt = mean(`External.debt.stocks....of.GNI.`, na.rm = TRUE),
        median_external_debt = median(`External.debt.stocks....of.GNI.`, na.rm = TRUE),
        min_external_debt = min(`External.debt.stocks....of.GNI.`, na.rm = TRUE),
        max_external_debt = max(`External.debt.stocks....of.GNI.`, na.rm = TRUE)
    )

view(summary_stats_shock_years)

view(summary_stats)

# Calculate economic volatility (standard deviation of GDP growth) for each country
volatility_stats <- data %>%
  group_by(`Country.Name`, `Country.Code`, `CFA Group`) %>%
  summarise(
    gdp_growth_sd = sd(`GDP.growth..annual...`, na.rm = TRUE)
  ) %>%
  group_by(`Country.Name`, `Country.Code`, `CFA Group`) %>%
  summarise(
    gdp_growth_sd = mean(gdp_growth_sd, na.rm = TRUE)
  )

# Calculate the standard deviation of GDP growth for CFA and Non-CFA countries
std_dev_gdp_growth <- data %>%
    group_by(`CFA Group`) %>%
    summarise(
        std_dev_gdp_growth = sd(`GDP.growth..annual...`, na.rm = TRUE)
    )

print(std_dev_gdp_growth)


# Calculate the standard deviation of GDP growth for CFA and Non-CFA countries during shock years only
std_dev_gdp_growth_shock_years <- data %>%
    filter(Year %in% c(1994, 1999, 2008, 2014, 2020, 2022)) %>%
    group_by(`CFA Group`) %>%
    summarise(
        std_dev_gdp_growth = sd(`GDP.growth..annual...`, na.rm = TRUE)
    )

print(std_dev_gdp_growth_shock_years)

view(volatility_stats)

        # Load Africa map data
        africa <- ne_countries(continent = "Africa", returnclass = "sf")

        # Merge the volatility statistics with the Africa map data
        africa_volatility <- africa %>%
            left_join(volatility_stats, by = c("iso_a3" = "Country.Code"))

        # Plot economic volatility on the African map with CFA Franc nations labelled
   ggplot(data = africa_volatility) +
       geom_sf(aes(fill = gdp_growth_sd)) +
       geom_sf(data = subset(africa_volatility, iso_a3 %in% c("BEN", "BFA", "CIV", "GNB", "MLI", "NER", "SEN", "TGO", "CMR", "CAF", "COG", "GAB", "GNQ", "TCD")), 
        color = "black", size = 1.5, fill = NA) +
       geom_text(data = subset(africa_volatility, iso_a3 %in% c("BEN", "BFA", "CIV", "GNB", "MLI", "NER", "SEN", "TGO", "CMR", "CAF", "COG", "GAB", "GNQ", "TCD")),
        aes(label = name, geometry = geometry), size = 1.5, color = "black", stat = "sf_coordinates") +
       scale_fill_gradient(
         low = "white", 
         high = "red", 
         na.value = "grey50", 
         name = "GDP Growth Volatility"
       ) +
       theme_minimal() +
       labs(
         title = "Economic Volatility in Africa", 
         subtitle = "Standard Deviation of GDP Growth (Annual)",
         caption = "CFA Franc Nations Labelled"
       ) +
       theme(
         legend.position = "bottom",
         axis.title = element_blank(),
         axis.text = element_blank(),
         axis.ticks = element_blank(),
         panel.grid = element_blank()
       )

    # Save the plot to a PNG file
    ggsave("/Users/rishitpothu/Desktop/africa_volatility.png", width = 10, height = 6)


        # Summary statistics for economic volatility by CFA Group
        volatility_summary <- volatility_stats %>%
            group_by(`CFA Group`) %>%
            summarise(
                mean_volatility = mean(gdp_growth_sd, na.rm = TRUE),
                sd_volatility = sd(gdp_growth_sd, na.rm = TRUE)
            )
        
        # View the summary statistics
        print(volatility_summary)
        # Plot summary statistics for economic volatility by CFA Group
        ggplot(volatility_summary, aes(x = `CFA Group`, y = mean_volatility, fill = `CFA Group`)) +
            geom_bar(stat = "identity", position = "dodge") +
            geom_errorbar(aes(ymin = mean_volatility - sd_volatility, ymax = mean_volatility + sd_volatility), width = 0.2, position = position_dodge(0.9)) +
            theme_minimal() +
            labs(title = "Economic Volatility by CFA Group", x = "", y = "Mean GDP Growth Volatility (SD)") +
            scale_fill_manual(values = c("CFA" = "blue", "Non-CFA" = "green")) +
            theme(
            plot.title = element_text(size = 15, face = "bold"),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 14)
            )
            
#combine africa map of sd of gdp growth WITH cfa group vs non cfa group sd(economic volatility) of gdp growth box plot

### USE Time Series of GDP Growth with Shock Periods and Volatility Model Regression Line #red regression line for cfa, blue line for non cfa
# Define the shock periods
shock_periods <- data.frame(
  Year = c(1994, 1999, 2008, 2014, 2020, 2022),
  Label = c("1994 CFA Franc Devaluation", "1999 Euro-Pegging Adoption", "2008 Financial Crisis", "2014 Commodity Price Crash", "2020 COVID-19", "2022 Ukraine War")
)
        ggplot(data, aes(x = Year, y = `GDP.growth..annual...`, color = `CFA Group`)) +
            geom_line() +
            theme_minimal() +
            labs(title = "GDP Growth Over Time", x = "Year", y = "GDP Growth (%)") +
            geom_vline(data = shock_periods, aes(xintercept = Year), color = "#000000", linetype = "dashed") +
            geom_text(data = shock_periods, aes(x = Year - 1, y = 40, label = Label), angle = 90, hjust = 1, size = 3, color = "red") +
            geom_smooth(method = "lm", se = FALSE, aes(group = `CFA Group`), color = "#0077ff", linetype = "solid") +
            facet_wrap(~ `CFA Group`, scales = "free_y") +
            annotate("text", x = Inf, y = 6, label = "Regression Line", color = "#0077ff", size = 3, hjust = 1.1, vjust = 4) +
            ylim(-10, 40) +  # Cap the bottom and top to keep both graphs on the same level
            theme(
                plot.title = element_text(size = 20, face = "bold"),
                axis.title = element_text(size = 16),
                axis.text = element_text(size = 14),
                legend.title = element_text(size = 16),
                legend.text = element_text(size = 14)
            )
            # Save the plot to a PNG file
            ggsave("/Users/rishitpothu/Desktop/gdp_growth_volatility.png", width = 10, height = 6)


            # Define shock years
            shock_years <- c(1994, 1999, 2008, 2014, 2020, 2022)

            # Create a new column to indicate shock years
            data <- data %>%
                mutate(Shock_Year = ifelse(Year %in% shock_years, "Shock Year", "Non-Shock Year"))

            # Scatterplot GDP volatility and trade dependence during shock years vs non-shock years
                ggplot(data, aes(x = `Trade.Dependency..annual...`, y = `GDP.growth..annual...`, color = Shock_Year)) +
                    geom_point(alpha = 0.6) +
                    geom_smooth(method = "lm", se = FALSE) +
                    theme_minimal() +
                    labs(title = "GDP Volatility and Trade Dependence During Shock Years vs Non-Shock Years", 
                         x = "Trade Dependence (%)", 
                         y = "GDP Growth (%)") +
                    facet_wrap(~ `CFA Group` + Shock_Year, scales = "free_x") +
                    ylim(-10, 40) +  # Set the same y-axis limits for all facets
                    theme(
                        plot.title = element_text(size = 24, face = "bold"),
                        axis.title = element_text(size = 20),
                        axis.text = element_text(size = 18),
                        legend.title = element_text(size = 20),
                        legend.text = element_text(size = 18),
                        strip.text = element_text(size = 20)  # Increase the size of facet labels
                    )

            # Save the plot to a PNG file
            ggsave("/Users/rishitpothu/Desktop/(3) gdp_volatility_trade_dependence_shock_years.png", width = 12, height = 20)


            # Plot external debt level over the years with shock periods
shock_periods <- data.frame(
    Year = c(1994, 1999, 2008, 2014, 2020, 2022),
    Label = c("1994 CFA Franc Devaluation", "1999 Euro-Pegging Adoption", "2008 Financial Crisis", "2014 Commodity Price Crash", "2020 COVID-19", "2022 Ukraine War")
)
        ggplot(data, aes(x = Year, y = `External.debt.stocks....of.GNI.`, color = `CFA Group`)) +
            geom_line(stat = "summary", fun = mean, size = 1) +
            theme_minimal() +
            labs(
                title = "External Debt Trends Over Time",
                x = "Year",
                y = "External Debt (% of GNI)",
                color = "CFA Group"
            ) +
            geom_vline(data = shock_periods, aes(xintercept = Year), color = "#000000", linetype = "dashed") +
            geom_text(data = shock_periods, aes(x = Year - 1, y = 40, label = Label), angle = 90, hjust = -1, size = 3, color = "red") +
            theme(
                plot.title = element_text(size = 24, face = "bold"),
                axis.title = element_text(size = 20),
                axis.text = element_text(size = 18),
                legend.title = element_text(size = 20),
                legend.text = element_text(size = 18)
            )

                # Save the plot to a PNG file
                        ggsave("/Users/rishitpothu/Desktop/5 external_debt_over_years.png", width = 10, height = 10)

# Boxplot for Gini Index
ggplot(data, aes(x = `CFA Group`, y = `Gini.index`, fill = `CFA Group`)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "Income Inequality: CFA vs Non-CFA Countries", x = "CFA Zone", y = "Gini Index") +
    theme(
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14)
    )
     ggsave("/Users/rishitpothu/Desktop/gini_index_boxplot.png", width = 10, height = 6)

# Linear regression for economic volatility
volatility_model <- lm(`GDP.growth..annual...` ~ `CFA Group` + `Trade.Dependency..annual...` + `External.debt.stocks....of.GNI.` + `Inflation..consumer.prices..annual...`, data = data)
summary(volatility_model)
# Linear regression for income inequality
inequality_model <- lm(`Gini.index` ~ `CFA Group` + `Human.Development.Index` + `Trade.Dependency..annual...`, data = data)
summary(inequality_model)

# Residual diagnostics
#volatility model
png("/Users/rishitpothu/Desktop/volatility_model_diagnostics.png", width = 800, height = 800)
par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
plot(volatility_model, which = 1)  # Residuals vs Fitted
plot(volatility_model, which = 2)  # Q-Q Plot
plot(volatility_model, which = 3)  # Scale-Location
plot(volatility_model, which = 4)  # Residuals vs Leverage
par(mfrow = c(1, 1))  # Reset to default layout
dev.off()

#inequality model
png("/Users/rishitpothu/Desktop/inequality_model_residual_diagnostics.png", width = 800, height = 800)
par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
plot(inequality_model, which = 1)  # Residuals vs Fitted
plot(inequality_model, which = 2)  # Q-Q Plot
plot(inequality_model, which = 3)  # Scale-Location
plot(inequality_model, which = 4)  # Residuals vs Leverage
par(mfrow = c(1, 1))  # Reset to default layout
dev.off()

# Variance Inflation Factor (VIF) -> Multicollinearity
vif(volatility_model)


###END OF CODE###