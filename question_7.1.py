#%%
import os

import numpy as np

#%%

a = [
    [[0, 1, 2, 3],
     [4, 5, 6, 7],
     [8, 9, 10, 11]
    ],
    [[12, 13, 14, 15],
     [16, 17, 18, 19],
     [20, 21, 22, 23]
    ]
]

b = [[0, 1, 2, 3],
     [4, 5, 6, 7],
     [8, 9, 10, 11]
]


#%%

def calc_index(ndim, coord):

    idim = coord
    n1 = len(idim) - 1
    index = 0

    while n1 > 0:
        mult = idim[n1]
        n2 = n1-1
        
        while n2 > -1:
            mult *= ndim[n2]
            n2 -= 1
        
        index += mult
        n1 -= 1

    else:
        index += idim[n1]

    return(index)

#%%

ndim = [4, 3]

coord = [2, 1]

#%%
            
found = calc_index(ndim = ndim, coord = coord)


ground_truth = b.copy()

for i in coord[::-1]:
    ground_truth = ground_truth[i]


print(found)

print(ground_truth)

#%%



# def get_coord(ndim, index):

    # for i in ndim[::-1]:
        # i = int(i)

#%%

with open("./Question 7/Question 7.1/input_coordinates_7_1.txt") as f:
    coordinates = f.readlines()


print(len(coordinates))

# ndim = [4, 8, 5, 9, 6, 7]

ndim = [50, 57]

data = coordinates[1].strip().split("\t")

data = [int(x) for x in data]

calc_index(ndim = ndim, coord = data)

#%%

a = [
    [[0, 1, 2, 3],
     [4, 5, 6, 7],
     [8, 9, 10, 11]
    ],
    [[12, 13, 14, 15],
     [16, 17, 18, 19],
     [20, 21, 22, 23]
    ]
]

ndim = [4, 3, 2]

idim = [2,1,1]

index = calc_index(ndim = ndim, coord = idim)

print(index)

calc_coord(ndim, index)

def calc_coord(ndim, index):

    coord = len(ndim)*[""]
    
    n1 = len(ndim) - 1

    while n1 > 0:
        
        mult = 1
        
        n2 = n1-1
        
        while n2 > -1:
            mult *= ndim[n2]
            n2 -= 1
        
        coord[n1]= index // mult
        index = index % mult
        n1 -= 1

    else:
        coord[n1] = index

    return(coord)



