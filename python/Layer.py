import numpy as np
class Layer():
    def __init__(self, nConnect, nNeurons, fAct):
        self.fAct=fAct
        self.b=np.random.rand(1, nNeurons) +2-1
        self.W=np.random.rand(nConnect, nNeurons) +2-1
