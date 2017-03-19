import sys
import re
import math
from sklearn import svm

feature_files = ["dataset/feature/timesig.csv",
                 "dataset/feature/keysig.csv",
                 "dataset/feature/primitive.csv",
                 "dataset/feature/all.csv"]

def read_data(filename):
  f = open(filename, 'r')
  p = re.compile(',')
  feature_data = []
  label_data = []
  namehash = {}
  for l in f:
    example = [int(x) for x in p.split(l.strip())]
    x = example[0:-1]
    y = example[-1]
    feature_data.append(x)
    label_data.append(y)
  return (feature_data,label_data)

def write_model(filename):
  f = open(modelfile, "w+")
  f.write('%f\n' % b)
  for i in xrange(len(w)):
    f.write('%s %f\n' % (varnames[i], w[i]))

def classify_feature_set(filename):
  # load and partition data
  (feature_data,label_data) = read_data(filename)
  size = len(feature_data)
  split = int(math.ceil(size * 0.7))

  # Train model
  classifier = svm.LinearSVC()
  classifier.fit(feature_data[:split],label_data[:split])

  # Test classifier
  predictions = classifier.predict(feature_data[split:])

  # Summarize predictions
  num = len(predictions)
  correct = 0
  matrix = [[0 for _ in range(3)] for _ in range(3)]  # hardwired for 3 classes
  for i in range(num):
    predicted = predictions[i]
    actual = ((label_data[split:])[i])
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
  for fs in feature_files:
    classify_feature_set(fs)

if __name__ == "__main__":
  main(sys.argv[1:])
