library(tidyverse)

train <- read.csv("train.csv") %>% tibble()

library(dplyr)

print(
  tibble(
    variable = names(train),
    type = sapply(train, \(x) class(x)[1]),
    n_unique = sapply(train, dplyr::n_distinct),
    sample = sapply(train, \(x) paste(head(unique(x), 10), collapse = ", "))
  ) %>% 
    arrange(type, n_unique),
  n = 81
)

## Numerical cols to be converted to char
train$MSSubClass <- as.character(train$MSSubClass)
train$MSSubClass

## Who

## What

### Utilities
Utilities: Type of utilities available
Heating: Type of heating
CentralAir: Central air conditioning
Electrical: Electrical system
Fireplaces: Number of fireplaces
BsmtFullBath: Basement full bathrooms
BsmtHalfBath: Basement half bathrooms
FullBath: Full bathrooms above grade
HalfBath: Half baths above grade
GarageCars: Size of garage in car capacity
MiscFeature: Miscellaneous feature not covered in other categories


### Style
MSSubClass: Identifies the type of dwelling involved in the sale.	
LotShape: General shape of property
BldgType: Type of dwelling
HouseStyle: Style of dwelling
RoofStyle: Type of roof
MasVnrType: Masonry veneer type
GarageType: Garage location
GarageFinish: Interior finish of the garage

### Material
RoofMatl: Roof material
Exterior1st: Exterior covering on house
Exterior2nd: Exterior covering on house (if more than one material)
PavedDrive: Paved driveway
Foundation: Type of foundation

### Measurements
LotArea: Lot size in square feet
MasVnrArea: Masonry veneer area in square feet
BsmtQual: Evaluates the height of the basement
BsmtFinSF1: Type 1 finished square feet
BsmtFinSF2: Type 2 finished square feet
BsmtUnfSF: Unfinished square feet of basement area
TotalBsmtSF: Total square feet of basement area
1stFlrSF: First Floor square feet
2ndFlrSF: Second floor square feet
LowQualFinSF: Low quality finished square feet (all floors)
GrLivArea: Above grade (ground) living area square feet
GarageArea: Size of garage in square feet
WoodDeckSF: Wood deck area in square feet
OpenPorchSF: Open porch area in square feet
EnclosedPorch: Enclosed porch area in square feet
3SsnPorch: Three season porch area in square feet
ScreenPorch: Screen porch area in square feet
PoolArea: Pool area in square feet


### Quality
OverallQual: Rates the overall material and finish of the house
OverallCond: Rates the overall condition of the house
ExterQual: Evaluates the quality of the material on the exterior
ExterCond: Evaluates the present condition of the material on the exterior
BsmtCond: Evaluates the general condition of the basement
BsmtFinType1: Rating of basement finished area
BsmtFinType2: Rating of basement finished area (if multiple types)
HeatingQC: Heating quality and condition
KitchenQual: Kitchen quality
Functional: Home functionality (Assume typical unless deductions are warranted)
FireplaceQu: Fireplace quality
GarageQual: Garage quality
GarageCond: Garage condition
PoolQC: Pool quality
Fence: Fence quality


YearBuilt: Original construction date
GarageYrBlt: Year garage was built
### Modifications
YearRemodAdd: Remodel date (same as construction date if no remodeling or additions)

### Rooms
Bedroom: Bedrooms above grade (does NOT include basement bedrooms)
Kitchen: Kitchens above grade
TotRmsAbvGrd: Total rooms above grade (does not include bathrooms)


### Flow
BsmtExposure: Refers to walkout or garden level walls

## When
MoSold: Month Sold (MM)
YrSold: Year Sold (YYYY)

## Where
MSZoning: Identifies the general zoning classification of the sale.
LotFrontage: Linear feet of street connected to property
Street: Type of road access to property
Alley: Type of alley access to property
LandContour: Flatness of the property
LotConfig: Lot configuration
LandSlope: Slope of property
Neighborhood: Physical locations within Ames city limits
Condition1: Proximity to various conditions
Condition2: Proximity to various conditions (if more than one is present)

## Why
SaleType: Type of sale
SaleCondition: Condition of sale

library(tidyverse)

# Columns to exclude from predictor plotting
exclude_vars <- c("SalePrice", "Id")

# Add log-transformed price
train_plot <- train %>%
  mutate(log_sale_price = (SalePrice))

# Identify numeric and categorical predictors
numeric_vars <- train_plot %>%
  select(-all_of(exclude_vars), -log_sale_price) %>%
  select(where(is.numeric)) %>%
  names()

categorical_vars <- train_plot %>%
  select(-all_of(exclude_vars), -log_sale_price) %>%
  select(where(~ !is.numeric(.x))) %>%
  names()

train_plot %>%
  select(all_of(numeric_vars), log_sale_price) %>%
  pivot_longer(
    cols = all_of(numeric_vars),
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(!is.na(value), !is.na(log_sale_price)) %>%
  ggplot(aes(x = value, y = log_sale_price)) +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_smooth(method = "loess", se = FALSE, colour = "red", linewidth = 0.6) +
  facet_wrap(~ variable, scales = "free_x") +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6)
  ) +
  labs(
    x = NULL,
    y = "log(SalePrice)"
  )

BsmtFinSF1
