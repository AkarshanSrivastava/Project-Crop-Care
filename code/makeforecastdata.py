import pandas as pd 
from pandas import read_excel
#16-17

forecastdata01= pd.read_excel("F://Akarshan's Document//Akarshan//excelrDATASCIENCE//Project1//Data Sets//Sales1(16-17).xlsx")
forecastdata01["Year_Month"]=pd.to_datetime(forecastdata01['Date']).dt.to_period('M')

data001=forecastdata01.groupby(["Year_Month"])['Value'].sum().reset_index()
data002=forecastdata01.groupby(["Year_Month"])['Quantity'].sum().reset_index()
finaldata=pd.merge(data001,data002)
finaldata


##17-18
forecastdata1= pd.read_csv("F://Akarshan's Document//Akarshan//excelrDATASCIENCE//Project1//Data Sets/Sales(17-18).csv")
forecastdata1["Year_Month"]=pd.to_datetime(forecastdata1['Date']).dt.to_period('M')

data01=forecastdata1.groupby(["Year_Month"])['Value'].sum().reset_index()
data02=forecastdata1.groupby(["Year_Month"])['Quantity'].sum().reset_index()
finaldata1=pd.merge(data01,data02)
finaldata1
forecastfinaldata=finaldata.append(finaldata1, ignore_index=True)

forecastfinaldata.to_csv("F://Akarshan's Document//Akarshan//excelrDATASCIENCE//Project1//Data Sets/forecastfinaldata.csv")

