module demo_algorithms_module
    use sparse_struct_base_module


    contains


    subroutine intensive_algorithm(pressure_sum, nx, ny, nz, num_materials)
        implicit none
        class(sparse_struct_base_t), pointer, intent(in) :: pressure_sum
        integer, intent(in) :: nx,ny,nz,num_materials
        integer :: i, j, k, m
        real(8) :: temp_sum = 0

        ! order k->j->i->m
        do k = 0, nz
            do j = 0, ny
                do i = 0, nx
                    do m = 1, num_materials
                            temp_sum = temp_sum + 0.25d0 * pressure_sum%get_item(m, i, j, k)
                    end do
                end do
            end do
        end do

        print*, 'Result intensive_algorithm', temp_sum
    end subroutine


    subroutine intensive_algorithm_mat(pressure_sum, nx, ny, nz, num_materials)
        implicit none
        class(sparse_struct_base_t), pointer, intent(in) :: pressure_sum
        integer, intent(in) :: nx,ny,nz,num_materials
        integer :: i, j, k, m
        real(8) :: temp_sum = 0

        ! order m->k->j->i
        do m = 1, num_materials
            do k = 0, nz
                do j = 0, ny
                    do i = 0, nx
                            temp_sum = temp_sum + 0.25d0 * pressure_sum%get_item(m, i, j, k)
                    end do
                end do
            end do
        end do

        print*, 'Result intensive_algorithm', temp_sum
    end subroutine


    subroutine intensive_algorithm_neighbors(pressure_sum, nx, ny, nz, num_materials)
        class(sparse_struct_base_t), pointer, intent(in) :: pressure_sum
        integer, intent(in) :: nx,ny,nz,num_materials

        integer :: i, j, k, m
        real(8) :: temp
        real(8) :: curr, up, down, left, right, bottom, top

        do k=1, nz-1
            do j = 1, ny-1
                do i = 1, nx-1
                    do m = 1, num_materials
                        curr = pressure_sum%get_item(m, i, j, k)
                        up = pressure_sum%get_item(m, i-1, j, k)
                        down = pressure_sum%get_item(m, i+1, j, k)
                        left = pressure_sum%get_item(m, i, j-1, k)
                        right = pressure_sum%get_item(m, i, j+1, k)
                        bottom = pressure_sum%get_item(m, i, j, k-1)
                        top = pressure_sum%get_item(m, i, j, k+1)

                        temp = temp + 0.25d0 * (curr + up + down + left + right + bottom + top)
                    end do
                end do
            end do
        end do

        print*, 'Result intensive_algorithm_neighbors', temp
    end subroutine


    subroutine random_advection(nx, ny, nz, num_materials, ms, is, js, ks, vals, nz_ratio)
        ! update 30% of the data
        integer, intent(in) :: nx,ny,nz,num_materials
        integer, dimension(:), allocatable, intent(inout) :: ms, is, js, ks
        real(8), dimension(:), allocatable, intent(inout) :: vals
        real(8), intent(in) :: nz_ratio
        integer :: new_size
        integer :: i, j, k, m, idx

        new_size = size(ms)
        
        ! init
        idx = 0
        ms = 0
        js = 0
        is = 0
        ks = 0
        vals(:) = 1d0

        do k=0, nz
            do j=0, ny
                do i=0, nx
                    do m=1, num_materials

                        if (idx > new_size-1) return

                        if (rand(0) <= nz_ratio) then
                            ms(idx) = m
                            is(idx) = i
                            js(idx) = j
                            ks(idx) = k
                            ! if(idx==0) then
                            !     write(*,*) 'aaaaaaaaaaa', ms(idx),is(idx),js(idx),ks(idx), idx
                            ! end if

                            idx = idx + 1

                            
                        end if 

                    end do
                end do
            end do
        end do

    end subroutine


    subroutine advection(nx, ny, nz, num_materials, ms, is, js, ks, vals, nz_ratio)
        ! update 30% of the data
        integer, intent(in) :: nx,ny,nz,num_materials
        integer, dimension(:), allocatable, intent(inout) :: ms, is, js, ks
        real(8), dimension(:), allocatable, intent(inout) :: vals
        real(8), intent(in) :: nz_ratio
        integer :: new_size
        integer :: i, j, k, m, idx

        new_size = size(ms)
        
        ! init
        idx = 0
        ms = 0
        js = 0
        is = 0
        ks = 0
        vals(:) = 1d0

        do k=0, nz
            do j=0, ny
                do i=0, nx
                    do m=1, num_materials

                        if (idx > new_size-1) return

                        ms(idx) = m
                        is(idx) = i
                        js(idx) = j
                        ks(idx) = k

                        idx = idx + 1

                    end do
                end do
            end do
        end do

    end subroutine

end module