import sys
import re
import math
from util import *
from sklearn.linear_model import LogisticRegression

def classify_feature_set(filename):
  # load and partition data
  (feature_data,label_data) = read_data(filename)

  # split
  size = len(feature_data)
  split = int(math.ceil(size * 0.7))
  train_f,train_l = (feature_data[:split],label_data[:split])
  test_f,test_l = (feature_data[split:],label_data[split:])

  # Train model
  classifier = LogisticRegression()
  classifier.fit(test_f,test_l)

  # Test classifier
  predictions = classifier.predict(test_f)

  # Summarize predictions
  num = len(predictions)
  correct = 0
  matrix = [[0 for _ in range(3)] for _ in range(3)]  # hardwired for 3 classes
  for i in range(num):
    predicted = predictions[i]
    actual = test_l[i]
    matrix[predicted-1][actual-1] += 1
    if predicted == actual:
      correct += 1

  print filename
  print "\t1\t2\t3"
  for x in range(len(matrix)):
    print str(x+1) + "\t",
    for y in range(len(matrix[x])):
      print str(matrix[x][y]) + "\t",
    print ""
  print "Correct: " + str(correct) + " out of " + str(num)
  print "Accuracy: " + str(float(correct)/float(num)) + "\n"

def main(argv):
  print "Logistic Regression Classifier:"
  for fs in feature_files:
    classify_feature_set(fs)

if __name__ == "__main__":
  main(sys.argv[1:])
