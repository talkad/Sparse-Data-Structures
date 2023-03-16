import os
import subprocess
from itertools import product


def execute_script(save_dir, script, num_mats, nz_ratio, is_valgrind=False):
    execute = [script, str(num_mats), str(nz_ratio)]

    if is_valgrind:
        execute = ['valgrind'] + execute + [os.path.join(save_dir,f'--log-file={num_mats}_{is_valgrind}.out')]
        p=subprocess.Popen(execute)   
    else:
        with open(os.path.join(save_dir, f'{num_mats}_{is_valgrind}.txt'), 'a+') as f:
            p=subprocess.Popen(execute, stdout=f)
    print(execute)
    p.communicate()

save_dir = 'results'
local_dir = '/home/talkad/Desktop/data_structures'
base_dir = '/home/talkad/Desktop/data_structures/sparse_data_struct'
implementations = ['dynamic_array'] #['csr3', 'dynamic_array']# ['mat3d', 'csr2']    # ['linked_list', 'mat3d', 'dynamic_array', 

num_materials = [2,4,8] # [2,4,8,16]
nz_ratios = [0.01, 0.1, 0.3, 0.5, 1]

for implementation in implementations:

    os.chdir(local_dir)
    imp_dir = os.path.join(save_dir, implementation)
    os.makedirs(imp_dir, exist_ok = True)
    os.chdir(os.path.join(base_dir,implementation))

    for num_mats, nz_ratio, is_valgrind in product(num_materials, nz_ratios, [False]):
        # print(num_mats, nz_ratio, is_valgrind)
        execute_script(os.path.join(local_dir, imp_dir), './exe',num_mats, nz_ratio, is_valgrind)


