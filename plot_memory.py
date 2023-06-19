import matplotlib.pyplot as plt


# ratios = [0.01, 0.1, 0.3, 0.5, 0.8, 1]
# num_data = 50
# nx, ny = 10**4, 10**4
# nz = 4 # 16
# num_material = 4

# # 10000x10000x20
# # 4 materials
# mat_ids_mem = nx*ny*nz*8
# naive_mem = lambda _: nx*ny*nz* num_data * 8
# csr_mem = lambda ratio: mat_ids_mem + 2*(nx*ny*ratio*num_material + nx*ny*(1-ratio))*8*num_data
# mat_array_mem = lambda ratio: mat_ids_mem + (nx*ny*8 + 2*nx*ny*num_material*ratio*8 + nx*ny*(1-ratio)*8)*num_data
# linked_mem = lambda ratio: mat_ids_mem + (nx*ny*8 + nx*ny*num_material*ratio*25 + nx*ny*(1-ratio)*25)*num_data


# plt.title('Memory')
# plt.plot(ratios,[naive_mem(ratio) for ratio in ratios], label='Mat3D')
# plt.plot(ratios,[linked_mem(ratio) for ratio in ratios], label='Linked List')
# plt.plot(ratios,[mat_array_mem(ratio) for ratio in ratios], label='Mat of arrays')
# plt.plot(ratios,[csr_mem(ratio) for ratio in ratios], label='CSR')


# # total

# # plt.plot([0.01, 1],[32*10**8*num_data, 32*10**8*num_data], label='naive mat3d')
# # # csr3
# # plt.plot(ratios,[csr_mem(ratio)*num_data+32*10**8 for ratio in ratios], label='2 materials')

# plt.legend()
# plt.ylabel('memory (bytes)')
# plt.xlabel('materials precentage')
# plt.show()






# ratios = [0.01, 0.1, 0.3, 0.5, 1]
# num_materials = 20
# nx, ny, nz = 350, 350, 350



# mat_ids_mem = nx*ny*nz*num_materials*4
# naive_mem = lambda _: nx*ny*nz* num_materials * 8
# csr_mem = lambda ratio: mat_ids_mem + nx*ny*nz*ratio*num_materials*8
# mat_array_mem = lambda ratio: nx*ny*nz*8 + nx*ny*nz*num_materials*ratio*8
# linked_mem = lambda ratio: nx*ny*nz*8 + nx*ny*nz*num_materials*ratio*25


# plt.title('Memory')
# plt.plot(ratios,[naive_mem(ratio) for ratio in ratios], label='Mat3D')
# plt.plot(ratios,[linked_mem(ratio) for ratio in ratios], label='Mat of List')
# plt.plot(ratios,[mat_array_mem(ratio) for ratio in ratios], label='Mat of arrays')
# plt.plot(ratios,[csr_mem(ratio) for ratio in ratios], label='CSR')


# # total

# # plt.plot([0.01, 1],[32*10**8*num_data, 32*10**8*num_data], label='naive mat3d')
# # # csr3
# # plt.plot(ratios,[csr_mem(ratio)*num_data+32*10**8 for ratio in ratios], label='2 materials')

# plt.legend()
# plt.ylabel('memory (bytes)')
# plt.xlabel('materials precentage')
# plt.show()