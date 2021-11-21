from random import randint
import random
import operator
from functools import reduce

def createIndividual():
    activationsList=["sigmoid","relu","tanh","selu","softmax"]
    lenUnits=randint(2,4)
    units=[randint(2, 256) for line in range(0,lenUnits)]
    activations=activationsList[randint(0, 4)]
    optimizersList=["Nadam","SGD","RMSprop","Adam","Adadelta","Adagrad","Adamax"]
    optimizer=optimizersList[randint(0, 6)]
    step_per_epoch=randint(20, 200)
    epoch=randint(5, 20)
    batch_size=randint(50, 1024)
    list=[units,activations,optimizer,step_per_epoch,epoch,batch_size]
    return list
def printList(list):
    [print(i) for i in list]
def selectToCross(poblation):
    indexPoblation=[i for i in range(0,len(poblation)-1)]
    numCross=int(len(indexPoblation)/2)
    parent1=[]
    parent2=[]
    for i in range(0, numCross):
        parent1.append(indexPoblation[randint(0,len(indexPoblation)-1)])
        indexPoblation.remove(parent1[len(parent1)-1])
        parent2.append(indexPoblation[randint(0,len(indexPoblation)-1)])
        indexPoblation.remove(parent2[len(parent2)-1])
    return parent1, parent2
def generateChilds(pleft, pright):
    child=pright
    nGens=randint(2,len(pleft[0]))
    gens=random.sample(range(6), nGens)
    for i in gens:
        child[i]=pleft[i]
    return child
def crossOX(poblation, parent1, parent2):
    childs=reduce(operator.add,[[generateChilds(poblation[i],poblation[j]), generateChilds(poblation[j],poblation[i])] for i,j in zip(parent1, parent2)])
    return childs
