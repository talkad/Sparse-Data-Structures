program main
    use omp_lib
    use data_abstract_module
    use dynamic_array_module
    use mat_array_module
    use algorithm_module

    class(mat_array_t), pointer :: data_struct
    integer :: nx, ny, num_materials, i,j,m
    real(8), dimension(:,:,:), allocatable :: p
    real*8 :: time

    nx = 1000
    ny = 1000
    num_materials = 10

    allocate(p(0:num_materials,0:nx,0:ny))
    call init_3d_deterministic(p, num_materials, nx, ny)
    ! call init_3d(p, 1.0, num_materials, nx, ny)

    print*, '----------'
    data_struct => mat_array_constructor(p)
    print*, '----------'

    time = omp_get_wtime()
    call intensive_algorithm_neighbors(data_struct, nx, ny, num_materials)
    print*, 'func time', omp_get_wtime() - time
    deallocate(p)
end program


! nx * ny * 8 + (nx * ny * num_materials * nz_ratio * (sizeof(link)~25))

! correct
! Result intensive_algorithm   15185592188266.500
! Result intensive_algorithm_neighbors   13763736.250000000