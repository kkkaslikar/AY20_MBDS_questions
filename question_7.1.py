#%%
import os

import numpy

#%%

a = [[[0, 1, 2],
      [3, 4, 5],
      [6, 7, 8]],
     [[9, 10, 11],
      [12, 13, 14],
      [15, 16, 17]]]

#%%

def calc_index(ndim, coord):

    idim = coord
    n1 = len(idim) -1
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

ndim = [3, 3, 2]

coord = [2, 2, 0]

#%%
            
found = calc_index(ndim = ndim, coord = coord)

ground_truth = a.copy()

for i in coord[::-1]:
    ground_truth = ground_truth[i]
    
print(ground_truth)


print(found)

#%%

def get_coord(ndim):
    
    
