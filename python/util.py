import re
from random import uniform

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

def read_data_2(filename):
  f = open(filename, 'r')
  p = re.compile(',')
  feature_data = []
  label_data = []
  namehash = {}
  for l in f:
    example = [int(x) for x in p.split(l.strip())]
    x = example[0:-1]
    y = example[-1]
    print (x,y)
    if y is 1:
      r = uniform(0,1)
      if r < 0.33:
        feature_data.append(x)
        label_data.append(y)
    elif y is 2:
      for i in range(3):
        feature_data.append(x)
        label_data.append(y)
    # elif y is 3:
    #   for i in range(5):
    #     feature_data.append(x)
    #     label_data.append(y)
    else:
      feature_data.append(x)
      label_data.append(y)
  return (feature_data,label_data)

def write_model(filename):
  f = open(modelfile, "w+")
  f.write('%f\n' % b)
  for i in xrange(len(w)):
    f.write('%s %f\n' % (varnames[i], w[i]))
