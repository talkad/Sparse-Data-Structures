program main
    use omp_lib
    ! use data_abstract_module
    use mat_list_module
    use algorithm_module

    type(mat_list_t), pointer :: data_struct
    integer :: nx, ny, num_materials, i,j,m
    real(8), dimension(:,:,:), allocatable :: p
    real*8 :: time
    
    integer :: num_mats
    real :: nz_ratio
    character(len=32) :: arg

    call get_command_argument(1,arg)
    read(arg,*)  num_mats

    call get_command_argument(2,arg)
    read(arg,*)  nz_ratio

    nx = 5000
    ny = 5000
    num_materials = 20

    data_struct => mat_list_constructor(nx, ny)

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

    time = omp_get_wtime()
    call intensive_algorithm_mat(data_struct, nx, ny, num_materials)
    print*, 'func time', omp_get_wtime() - time

    time = omp_get_wtime()
    call intensive_algorithm_neighbors(data_struct, nx, ny, num_materials)
    print*, 'func time', omp_get_wtime() - time

    deallocate(p)

end program



! nx * ny * 8 + (nx * ny * num_materials * nz_ratio * (sizeof(link)~25))
! 136,242,544


! O0
!  Result intensive_algorithm   3015000.7500000000     
!  func time   7.5569695570738986

! Result intensive_algorithm_neighbors   15068996.250000000     
! func time   17.679628342040814

