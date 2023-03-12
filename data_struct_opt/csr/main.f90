program main
    use omp_lib
    use data_abstract_module
    use sparse_matrix_module
    use algorithm_module

    type(coo_matrix_t), pointer :: data_struct
    integer :: nx, ny, num_materials, i,j,m
    real(8), dimension(:,:,:), allocatable :: p
    real*8 :: time
    integer, parameter :: N = 5 
    integer, dimension(:), allocatable :: indx
    integer :: idx , a,b

    ! allocate(indx(0:N))

    ! indx = [1,2,2,2,2, 4, 4, 4,4,4,4,4,4]
    ! print*, indx

    ! idx = idx_exists(indx, 2)

    ! print*, idx


    ! call get_boundries(a, b, indx, idx)
    ! print*,a,b


    nx = 3
    ny = 3
    num_materials = 1


    allocate(p(0:num_materials,0:nx,0:ny))
    call init_3d_deterministic(p, num_materials, nx, ny)
    ! call init_3d(p, 1.0, num_materials, nx, ny)
    data_struct => sparse_constructor(p)
    
    print*, '-------'

    time = omp_get_wtime()
    call intensive_algorithm_neighbors(data_struct, nx, ny, num_materials)
    print*, 'func time', omp_get_wtime() - time
    deallocate(p)

end program


!  Result intensive_algorithm   15185592188266.500