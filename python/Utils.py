import Layer
import numpy as np
def derivateRelu(x):
    x[x<=0]=0
    x[x>0]=1
    return x

sigmoid=lambda x:1 / (1 + np.exp(-x))
sigmoidDerivate=lambda x:x * (1 - x)
relu=lambda x:np.maximum(0, x)
reluDerivate=lambda x: derivateRelu(x)
f_act={"sigmoid":sigmoid, "sigmoidDerivate": sigmoidDerivate, "relu":relu, "reluDerivate":reluDerivate}

def relu(x):
    return np.maximum(0, x)

rmse=lambda Yp, Yr:np.mean((Yp-Yr)**2)
rmseDerivate=lambda Yp, Yr:(Yp-Yr)

def accuracy_metric(actual, predicted):
	correct = 0
	for i in range(len(actual)):
		if actual[i] == predicted[i]:
			correct += 1
	return correct / float(len(actual)) * 100.0

def plot(res, resD, list):
    fig, axes = plt.subplots(nrows=1, ncols=2, figsize =(15,5))
    axes[0].plot(list, res[:,0])
    axes[1].plot(list, resD[:,0])
    plt.savefig('foo.png')

def createNeuralNetwork(topology):
    red_neuronal=[Layer.Layer(topology[i][0],topology[i+1][0],topology[i][1]) for i in range(0, len(topology[:-1]))]
    return red_neuronal

def neurona(X,W,b):
    return X @ W + b

def forwardPass(out, w, b, fAct):
    z=neurona(out, w, b)
    a=f_act[fAct](z)
    return a

def backward_propagation(output_error, learning_rate, out, layer):
    #return  f_act[layer.fAct+"Derivate"](out) * output_error
    input_error = np.dot(f_act[layer.fAct+"Derivate"](out)*output_error, layer.W.T)
    weights_error = np.dot(out.T, f_act[layer.fAct+"Derivate"](out)*output_error)
    # dBias = output_error

    # update parameters
    layer.W -= learning_rate * weights_error
    layer.b -= learning_rate * output_error
    return input_error

def train(neural_net, X, Y, lr, epochs):

    err=0
    for j in range(len(X)):
        output=X[j]
        for i in neural_net:
            output=forwardPass(output, i.W, i.b, i.fAct)

        err += rmse(output, Y[j])
        print(err)
        error = rmseDerivate(output, Y[j])
        for i in reversed(neural_net):
            error = backward_propagation(error, lr, output, i)
        err /= len(X)
        print('epoch %d/%d   error=%f' % (j+1, epochs, err))
    '''
        for i in reversed(0, range(len(neural_net))):
        z=out[i+1][0]
        a=out[i+1][1]
        if(i==len(neural_net)-1):
            deltas.insert(0, rmseDerivate(a, Y) * f_act[neural_net[i].fAct+"Derivate"](a))
        else:
            deltas.insert(0, deltas[0] @ _W.T * f_act[neural_net[i].fAct+"Derivate"](a))
        print(a)
        print(Y)
        print(deltas[0])
        print(neural_net[i].W.T)
        print(X)
        _W = neural_net[i].W

        neural_net[i].b = neural_net[i].b - np.mean(deltas[0], axis=0, keepdims=True) * lr
        neural_net[i].W = neural_net[i].W - out[i][1].T @ deltas[0] * lr

        '''

    return output, err
