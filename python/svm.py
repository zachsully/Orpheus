import sys
import re
import math
from sklearn import svm

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


def main(argv):
  if (len(argv) != 2):
    print 'Usage: svm.py <data> <model>'
    sys.exit(2)

  eta = 0.001
  lam = 10

  # load and partition data
  (feature_data,label_data) = read_data(argv[0])
  size = len(feature_data)
  split = int(math.ceil(size * 0.7))

  # Train model
  classifier = svm.LinearSVC()
  classifier.fit(feature_data[:split],label_data[:split])

  # Test classifier
  predictions = classifier.predict(feature_data[split:])
  num = len(predictions)
  correct = 0
  for i in range(num):
    if (predictions[i]) == ((label_data[split:])[i]):
      correct += 1

  print str(correct) + " out of " + str(num)
  print "Accuracy: " + str(float(correct)/float(num))

if __name__ == "__main__":
  main(sys.argv[1:])
