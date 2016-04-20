import numpy

numpy.set_printoptions(formatter={'float_kind':'{:25f}'.format})

from numpy import genfromtxt
from sklearn.cross_decomposition import CCA

psych = numpy.loadtxt('psych_mmreg.csv',
                      delimiter = ',',
                      dtype = {'names': ('locus_of_control', 'self_concept', 'motivation'), 'formats': ('f4', 'f4', 'f4')},
                      skiprows = 1)

acad = numpy.loadtxt('acad_mmreg.csv',
                      delimiter = ',',
                      dtype = {'names': ('read', 'write', 'math', 'science', 'female'), 'formats': ('f4', 'f4', 'f4', 'f4', 'i4')},
                      skiprows = 1)

psych

psych = psych.tolist()
acad = acad.tolist()

#print acad

cca = CCA(n_components = 3)
cca.fit(psych, acad)

#print "X weights:\n", cca.x_weights_
#print "\nY weights:\n", cca.y_weights_
#print "\nX loadings:\n", cca.x_loadings_
#print "\nY loadings:\n", cca.y_loadings_
#print "\nX scores:\n", cca.x_scores_
print "\nY scores:\n", cca.y_scores_


'''
X = [(0., 0., 1.), (1.,0.,0.), (2.,2.,2.), (3.,5.,4.)]
Y = [(0.1, -0.2), (0.9, 1.1), (6.2, 5.9), (11.9, 12.3)]
cca = CCA(n_components=1)
cca.fit(X, Y)
print cca.x_loadings_
'''
