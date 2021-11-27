from collections import namedtuple
import numpy as np
import matplotlib.pyplot as plt
import Layer
import Utils
from sklearn.datasets import make_circles
import pandas as pd

def celsius_to_kelvin(temp):
  kelvin = temp + 273.15
  return kelvin

def normalize(x):
  return (x- min(x)) /(max(x)-min(x))

n=500
df=pd.read_csv('./Eplus-env-discrete-mixed-v1-res1(RULE_BASED_CONTROLLER)/monitor.csv',sep=',')
#Add resto de ficheros csv
df = df.dropna(how='any',axis=0)
df["time"]=df["month"].astype(str) + "/" + df["day"].astype(str) + " " + df["hour"].astype(str)
df=df[["time","Site Outdoor Air Drybulb Temperature (Environment)",
                      "Zone Thermostat Heating Setpoint Temperature (SPACE1-1)",
                      "Zone Thermostat Cooling Setpoint Temperature (SPACE1-1)", "Zone Air Temperature (SPACE1-1)",
                      "Zone Thermal Comfort Mean Radiant Temperature (SPACE1-1 PEOPLE 1)",
                      "People Air Temperature (SPACE1-1 PEOPLE 1)",
                      "Facility Total HVAC Electric Demand Power (Whole Building)"]]
listCols=["Site Outdoor Air Drybulb Temperature (Environment)",
                      "Zone Thermostat Heating Setpoint Temperature (SPACE1-1)",
                      "Zone Thermostat Cooling Setpoint Temperature (SPACE1-1)", "Zone Air Temperature (SPACE1-1)",
                      "Zone Thermal Comfort Mean Radiant Temperature (SPACE1-1 PEOPLE 1)",
                      "People Air Temperature (SPACE1-1 PEOPLE 1)"]
for i in listCols:
    df[i]=celsius_to_kelvin(df[i])
listCols.append("Facility Total HVAC Electric Demand Power (Whole Building)")
for i in listCols:
    df[i]=normalize(df[i])
df["time"] = pd.to_numeric(df["time"], errors='coerce').fillna(0).astype(np.int64)

indextrain=int(df.shape[0]*0.7)
X=df.head(indextrain).to_numpy()
testindex=df.shape[0]-indextrain
Y=df["Facility Total HVAC Electric Demand Power (Whole Building)"].head(indextrain).to_numpy()
Y=Y[:, np.newaxis]
p=8
topology=[(p,"relu"),(16,"relu"),(8,"relu"),(1,"relu")]

red_neuronal = Utils.createNeuralNetwork(topology)
py=0
epochs=10
for i in range(0, epochs):
    py, err=Utils.train(neural_net=red_neuronal, X=X, Y=Y, lr=0.1, epochs=epochs)
res=Utils.rmse(py,Y)/len(X)
print(err)
