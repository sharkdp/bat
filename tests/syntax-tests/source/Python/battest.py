from os import getcwd
import numpy as np
from matplotlib.pyplot import plot as plt
from time import *


# COMMENT test
h2 = 4  # this is a comment
"""this is also a comment"""

# Import test

# class test


class Hello:
    def __init__(self, x):
        self.name = x
        
    def selfprint(self):
        print("hello my name is ", self.name)

    def testprint(self):
        print(1*2, 2+3, 4 % 5, 8-4, 9/4, 23//4)

# Decorators test
class Decorators:
    @classmethod
    def decoratorsTest(self):
        pass
    
H1 = Hello("john")
H1.selfprint()
H1.testprint()


# list test
a = [1, 2, 3, 4, 5]
a.sort()
print(a[1:3])
print(a[:4])
print(a[2])
print(a[2:])

# dictionary test
# copied from w3schools example

myfamily = {
    "child1": {
        "name": "Emil",
        "year": 2004
    },
    "child2": {
        "name": "Tobias",
        "year": 2007
    },
    "child3": {
        "name": "Linus",
        "year": 2011
    }
}

# tuple test

testTuple = ("one", 2, "3")
print(testTuple)

print(np.random.randint(5, 45))

# string test
a = "hello world"
b = """good morning
hello world
bye"""

formattest = "teststring is ={}".format(5)

# lambda test


def x2(n):
    lambda n: n/7


# if else ladder
if 1 > 2:
    print("yes")
elif 4 > 5:
    print("maybe")
else:
    print("no")

# loops
i = 5
while(i > 0):
    print(i)
    i -= 1

for x in range(1, 20, 2):
    print(x)
