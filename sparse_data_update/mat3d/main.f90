program main
    use omp_lib
    use data_abstract_module
    use mat3d_module
    use algorithm_module


    integer :: num_mats
    real :: nz_ratio
    character(len=32) :: arg

    class(data_abstract_t), pointer :: data_struct
    integer :: nx, ny, num_materials, i,j,m
    real(8), dimension(:,:,:), allocatable :: p
    real*8 :: time

    integer, dimension(:), allocatable :: ms, is, js
    real(8), dimension(:), allocatable :: vals


    call get_command_argument(1,arg)
    read(arg,*)  num_mats

    call get_command_argument(2,arg)
    read(arg,*)  nz_ratio

    nx = 5000
    ny = 5000
    num_materials = 20

    data_struct => mat3d_constructor(num_materials, nx, ny)

    allocate(p(0:num_materials,0:nx,0:ny))
    ! call init_3d_deterministic(p, 2, nx, ny)
    call init_3d(p, nz_ratio, num_mats, nx, ny)

    ! init data structure
    do j=0, ny
        do i=0, nx
            do m =0, num_materials
                call data_struct%add_item(m,i,j, p(m,i,j))
            end do
        end do
    end do

    print*, nz_ratio
    time = omp_get_wtime()
    call intensive_algorithm(data_struct, nx, ny, num_materials)
    print*, 'func time', omp_get_wtime() - time

    ! time = omp_get_wtime()
    ! call intensive_algorithm_mat(data_struct, nx, ny, num_materials)
    ! print*, 'func time', omp_get_wtime() - time

    ! time = omp_get_wtime()
    ! call intensive_algorithm_neighbors(data_struct, nx, ny, num_materials)
    ! print*, 'func time', omp_get_wtime() - time



    call advection(nx, ny, num_materials, ms, is, js, vals)

    time = omp_get_wtime()
    call data_struct%update_struct(ms, is, js, vals)
    print*, 'update time', omp_get_wtime() - time

    time = omp_get_wtime()
    call intensive_algorithm(data_struct, nx, ny, num_materials)
    print*, 'func time', omp_get_wtime() - time

    deallocate(p)

end program
