import matplotlib.pyplot as plt


plt.title('mat of lists')

plt.plot([0.01, 1],[60, 60], label='naive mat3d')

# linked list
plt.plot([0.01, 0.1, 0.3, 0.5, 1],[17.17, 17.28, 17.55, 17.8, 17.71], label='2 materials')
plt.plot([0.01, 0.1, 0.3, 0.5, 1],[17.19, 17.48, 18.11, 18.65, 19.7], label='4 materials')
plt.plot([0.01, 0.1, 0.3, 0.5, 1],[17.34, 17.87, 19.21, 20.44, 26.58], label='8 materials')
plt.plot([0.01, 0.1, 0.3, 0.5, 1],[17.3, 18.65, 21.23, 27.73, 37.1], label='16 materials')

plt.plot([0.01, 0.1, 0.3, 0.5, 1],[35, 36, 38.42, 42.23, 40.54], label='2 materials')
plt.plot([0.01, 0.1, 0.3, 0.5, 1],[35.86, 36.65, 40.14, 43.31, 46.29], label='4 materials')
plt.plot([0.01, 0.1, 0.3, 0.5, 1],[35.36, 37.83, 43.63, 49.27, 55], label='8 materials')
plt.plot([0.01, 0.1, 0.3, 0.5, 1],[35.46, 39.95, 50.22, 59, 76.91], label='16 materials')


plt.legend()
plt.xlabel('NZ ratio')
plt.xlabel('time (sec)')
plt.show()