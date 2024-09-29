import pandas as pd
import numpy as np

class linear_regression:
    def __init__(self) -> None:
        self.w = 0
        self.b  = 0

    def predict(self, x):
        y_pred = x.dot(self.w) + self.b
        return y_pred

    def cost_function(self, x, y):
        '''Mean squared error function'''
        row_size = len(y)
        cost_value = (1/(2*row_size)) * np.sum((self.predict(x) - y)**2)
        return cost_value

    def gradient_descent(self, x, y, learning_rate, epochs):
        m = len(y)  # Number of training examples
        for i in range(epochs):
            # Predict using current weights and bias
            y_pred = self.predict(x)
            
            # Calculate gradients
            w_grad = (1 / m) * x.T.dot(y_pred - y)  # Gradient w.r.t. w
            b_grad = (1 / m) * np.sum(y_pred - y)   # Gradient w.r.t. b
            
            # Update weights and bias
            self.w -= learning_rate * w_grad
            self.b -= learning_rate * b_grad
            
            # Compute cost
            cost = self.cost_function(x, y)
            
            # Print cost every 100 iterations
            if i % 100 == 0:
                print(f"Iteration {i}: Cost = {cost}")
        
        # Return the final weights and bias
        return self.w, self.b
x= np.array([[2], [3], [5], [7], [9]])

# Target: Test scores (output data)
y = np.array([50, 60, 80, 90, 95])
model = linear_regression()
learning_rate = 0.01
epochs = 10000

# Train the model using gradient descent
weights, bias = model.gradient_descent(x, y, learning_rate, epochs)

new_hours = np.array([[2], [6]])
predicted_scores = model.predict(new_hours)