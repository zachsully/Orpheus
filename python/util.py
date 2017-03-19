import re

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
