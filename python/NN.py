from collections import namedtuple
import numpy as np
import matplotlib.pyplot as plt
import Layer
import Utils
from sklearn.datasets import make_circles
import pandas as pd

n=500
df=pd.read_csv('./python/multiplica.csv',sep=',')
X=df.to_numpy()
Y=df["c3"].to_numpy()
Y=Y[:, np.newaxis]
p=3
topology=[(p,"relu"),(4,"relu"),(2,"relu"),(1,"relu")]

red_neuronal = Utils.createNeuralNetwork(topology)
py=0
epochs=100
for i in range(0, epochs):
    py, err=Utils.train(neural_net=red_neuronal, X=X, Y=Y, lr=0.1, epochs=epochs)
res=Utils.rmse(py,Y)/len(X)
print(err)
print(df)
