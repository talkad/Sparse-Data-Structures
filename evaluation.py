import os
import subprocess
from itertools import product
from multiprocessing import Process, Manager


save_dir = 'results'
local_dir = '/home/talkad/Desktop/data_structures'
base_dir = '/home/talkad/Desktop/data_structures/sparse_data_struct'


class Execute:
    def __init__(self):
        # data struct parameters
        self.optimazation = 2 
        self.num_materials = [2,4,8]
        self.nz_ratios = [0.01, 0.1, 0.3, 0.5, 1]
        self.structs = ['mat3d', 'linked_list','dynamic_array', 'dynamic_array_exist', 'csr3', 'csr2']


    def execute_params(self, struct):
        os.chdir(local_dir)
        imp_dir = os.path.join(save_dir, struct)
        os.makedirs(imp_dir, exist_ok = True)
        os.chdir(os.path.join(base_dir,struct))

        # compile
        execute = ['./script', self.optimazation]
        p=subprocess.Popen(execute, stdout=f)
        p.communicate()

        # execute
        for num_mats, nz_ratio in product(self.num_materials, self.nz_ratios):
            execute = ['./exe', str(num_mats), str(nz_ratio)]

            with open(os.path.join(save_dir, f'{num_mats}.txt'), 'a+') as f:
                p=subprocess.Popen(execute, stdout=f)
                p.communicate()


    def execute_struct_script(self, struct):
        manager = Manager()
        return_dict = manager.dict()
        t = Process(target=self.execute_params, args=(struct), daemon=True)

        t.start()


<<<<<<< HEAD
    os.chdir(local_dir)
    imp_dir = os.path.join(save_dir, f'./csr_block_{block_size}')
    os.makedirs(imp_dir, exist_ok = True)
    os.chdir(os.path.join(base_dir, './csr_block_inline'))
=======
    def execute(self):
        for struct in self.structs:
            self.execute_struct_script(struct)
>>>>>>> opt

