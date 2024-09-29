import pandas as pd
import numpy as np

class LogisticRegression:
    def __init__(self, n_features) -> None:
        # Initialize weights and bias
        
        self.w = np.zeros(n_features)
        self.b = 0

    def sigmoid(self, X):
        return 1 / (1 + np.exp(-X))

    def prediction(self, X):
        linear_model = X.dot(self.w) + self.b
        predicted_value = self.sigmoid(linear_model)
        return predicted_value

    def cost_function(self, X, y):
        m = len(y)
        y_pred = self.prediction(X)
        # Clipping values to avoid log(0)
        y_pred = np.clip(y_pred, 1e-10, 1 - 1e-10)
        # Cost function for logistic regression
        loss_value = (-1/m) * np.sum((y * np.log(y_pred)) + (1 - y) * np.log(1 - y_pred))
        return loss_value

    def gradient_descent_logistic(self, X, y, learning_rate, epochs):
        m = len(y)
        for epoch in range(epochs):
            y_pred = self.prediction(X)
            dw = (1/m) * (X.T.dot(y_pred - y))
            db = (1/m) * np.sum(y_pred - y)

            # Update weights and bias
            self.w -= learning_rate * dw
            self.b -= learning_rate * db

            # Calculate and print the cost at each epoch
            loss = self.cost_function(X, y)
            print(f"Epoch {epoch}: Cost = {loss}, Weights = {self.w}, Bias = {self.b}")
if __name__ == '__main__':
    X_train = np.array([[1, 2], [2, 3], [3, 4], [4, 5]])  # Feature matrix (n_samples, n_features)
    y_train = np.array([0, 0, 1, 1])  # Labels (n_samples,)
    n_features = X_train.shape[1]
    model = LogisticRegression(n_features)
    learning_rate = 0.01
    epochs = 1000
    model.gradient_descent_logistic(X_train, y_train, learning_rate, epochs)