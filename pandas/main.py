#
# Description: Following the official Pandas tutorial:
# https://pandas.pydata.org/docs/getting_started/intro_tutorials/index.html
#
import pandas as pd

# Part 1: How to I read and write tabular data?

# pandas supports many different file formats or data sources out of the box
# (csv, excel, sql, json, parquet, …), each of them with the prefix read_*.
titanic = pd.read_csv("data/titanic.csv")

# Printing will always show the first and last 5 rows of the dataframe.
# print(titanic)

# See first 8 rows of the dataframe
# print(titanic.head(8))

# See column data types
# print(titanic.dtypes)

# Create Excel spreadsheet of dataframe
# titanic.to_excel("titanic.xlsx", sheet_name="passengers", index=False)

# Technical summary
# print(titanic.info())

# Part 2: How do I select a subset of a DataFrame?

ages = titanic["Age"]

# Print first 5 entries, name, and dtype of Series (aka column)
# print(ages.head())

# <class 'pandas.core.series.Series'>
# print(type(ages))

# Select multiple Series
age_sex = titanic[["Age", "Sex"]]
# print(age_sex.head())
# print(titanic[["Age", "Sex"]].shape)

# Select subset of passengers older than 35
above_35 = titanic[titanic["Age"] > 35]
# print(above_35)

# Select passengers in cabin class 1 and 2
# https://pandas.pydata.org/docs/reference/api/pandas.Series.isin.html#pandas.Series.isin
class_23 = titanic[titanic["Pclass"].isin([2, 3])]

# The above statement is equivilant to:
class_23 = titanic[(titanic["Pclass"] == 2) | (titanic["Pclass"] == 3)]

# Passengers where age is known
age_no_na = titanic[titanic["Age"].notna()]

# Passengers who are older than 35 and their name is known
adult_names = titanic.loc[titanic["Age"] > 35, "Name"]

# I’m interested in rows 10 till 25 and columns 3 to 5.
titanic.iloc[9:25, 2:5]

# For example, to assign the name anonymous to the first 3 elements of the fourth column
titanic.iloc[0:3, 3] = "anonymous"

