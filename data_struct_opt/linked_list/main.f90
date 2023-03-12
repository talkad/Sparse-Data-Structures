program main
    use omp_lib
    ! use data_abstract_module
    use mat_list_module
    use algorithm_module

    type(mat_list_t), pointer :: data_struct
    integer :: nx, ny, num_materials, i,j,m
    real(8), dimension(:,:,:), allocatable :: p
    real*8 :: time

    nx = 1000
    ny = 1000
    num_materials = 10

    data_struct => mat_list_constructor(nx, ny)

    allocate(p(0:num_materials,0:nx,0:ny))
    call init_3d_deterministic(p, num_materials, nx, ny)
    ! call init_3d(p, 0.1, num_materials, nx, ny)

    ! init data structure
    do j=0, ny
        do i=0, nx
            do m =0, num_materials
                call data_struct%add_item(m,i,j, p(m,i,j))
            end do
        end do
    end do

    print*, '--------------------------', data_struct%counter
    time = omp_get_wtime()
    call intensive_algorithm(data_struct, nx, ny, num_materials)
    print*, 'func time', omp_get_wtime() - time
    print*, '--------------------------', data_struct%counter
    deallocate(p)
end program

! nx * ny * 8 + (nx * ny * num_materials * nz_ratio * (sizeof(link)~25))



!  Result intensive_algorithm   15185592188266.500
! Result intensive_algorithm_neighbors   13763736.250000000
