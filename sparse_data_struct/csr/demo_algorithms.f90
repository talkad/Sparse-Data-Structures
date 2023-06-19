module demo_algorithms_module
    use sparse_struct_base_module


    contains


    subroutine intensive_algorithm(A, B, C, nx, ny, nz, num_materials)
        implicit none
        class(sparse_struct_base_t), pointer, intent(in) :: A, B
        class(sparse_struct_base_t), pointer, intent(out) :: C
        integer, intent(in) :: nx,ny,nz,num_materials
        integer :: i, j, k, m, total_size, idx=0

        real(8) :: temp_sum = 0, res
        integer, dimension(:), allocatable :: ms, is, js, ks
        real(8), dimension(:), allocatable :: vals

        total_size = nx*ny*nz*num_materials
        allocate(ms(0:total_size-1))
        allocate(is(0:total_size-1))
        allocate(js(0:total_size-1))
        allocate(ks(0:total_size-1))
        allocate(vals(0:total_size-1))

        ms = -1
        is = -1
        js = -1
        ks = -1
        vals = 0d0

        ! order k->j->i->m
        do k = 0, nz
            do j = 0, ny
                do i = 0, nx
                    do m = 1, num_materials
                            temp_sum = temp_sum + A%get_item(m, i, j, k)

                            res = A%get_item(m, i, j, k) + B%get_item(m, i, j, k)
                            if (res > 0) then
                                ms(idx) = m
                                is(idx) = i
                                js(idx) = j
                                ks(idx) = k
                                vals(idx) = res

                                idx = idx + 1
                            end if
                    end do
                end do
            end do
        end do
        
        call C%update_struct(ms,is,js,ks,vals)
        deallocate(ms,is,js,ks,vals)
        print*, 'Result intensive_algorithm', temp_sum
    end subroutine


    subroutine intensive_algorithm_mat(A, B, C, nx, ny, nz, num_materials)
        implicit none
        class(sparse_struct_base_t), pointer, intent(in) :: A, B
        class(sparse_struct_base_t), pointer, intent(inout) :: C
        integer, intent(in) :: nx,ny,nz,num_materials
        integer :: i, j, k, m, total_size, idx=0

        real(8) :: temp_sum = 0, res
        integer, dimension(:), allocatable :: ms, is, js, ks
        real(8), dimension(:), allocatable :: vals

        total_size = nx*ny*nz*num_materials
        allocate(ms(0:total_size-1))
        allocate(is(0:total_size-1))
        allocate(js(0:total_size-1))
        allocate(ks(0:total_size-1))
        allocate(vals(0:total_size-1))

        ms = -1
        is = -1
        js = -1
        ks = -1
        vals = 0d0

        ! order m->k->j->i
        do m = 1, num_materials
            do k = 0, nz
                do j = 0, ny
                    do i = 0, nx
                            temp_sum = temp_sum + A%get_item(m, i, j, k)

                            res = A%get_item(m, i, j, k) + B%get_item(m, i, j, k)
                            if (res > 0) then
                                ms(idx) = m
                                is(idx) = i
                                js(idx) = j
                                ks(idx) = k
                                vals(idx) = res

                                idx = idx + 1
                            end if
                    end do
                end do
            end do
        end do

        call C%update_struct(ms,is,js,ks,vals)
        deallocate(ms,is,js,ks,vals)
        print*, 'Result intensive_algorithm_mat', temp_sum
    end subroutine


    subroutine intensive_algorithm_neighbors(A, C, nx, ny, nz, num_materials)
        implicit none
        class(sparse_struct_base_t), pointer, intent(inout) :: A
        class(sparse_struct_base_t), pointer, intent(inout) :: C
        integer, intent(in) :: nx,ny,nz,num_materials
        integer :: i, j, k, m, total_size, idx=0
        real(8) :: curr, up, down, left, right, bottom, top

        real(8) :: temp_sum = 0, res
        integer, dimension(:), allocatable:: ms, is, js, ks
        real(8), dimension(:), allocatable:: vals

        total_size = nx*ny*nz*num_materials
        allocate(ms(0:total_size-1))
        allocate(is(0:total_size-1))
        allocate(js(0:total_size-1))
        allocate(ks(0:total_size-1))
        allocate(vals(0:total_size-1))

        ms = 0d0
        is = 0d0
        js = 0d0
        ks = 0d0
        vals = 0d0

        ! order m->k->j->i
        do k=1, nz-1
            do j = 1, ny-1
                do i = 1, nx-1
                    do m = 1, num_materials
                            curr = A%get_item(m, i, j, k)
                            up = A%get_item(m, i-1, j, k)
                            down = A%get_item(m, i+1, j, k)
                            left = A%get_item(m, i, j-1, k)
                            right = A%get_item(m, i, j+1, k)
                            bottom = A%get_item(m, i, j, k-1)
                            top = A%get_item(m, i, j, k+1)

                            res = curr + up + down + left + right + bottom + top

                            temp_sum = temp_sum + res
                            
                            if (res > 0) then
                                ms(idx) = m
                                is(idx) = i
                                js(idx) = j
                                ks(idx) = k
                                vals(idx) = res

                                idx = idx + 1
                            end if
                    end do
                end do
            end do
        end do

        call  C%update_struct(ms,is,js,ks,vals)
        deallocate(ms,is,js,ks,vals)
        print*, 'Result intensive_algorithm_neighbors', temp_sum
    end subroutine


    subroutine advection(nx, ny, nz, num_materials, ms, is, js, ks, vals, nz_ratio)
        integer, intent(in) :: nx,ny,nz,num_materials
        integer, dimension(:), allocatable, intent(inout) :: ms, is, js, ks
        real(8), dimension(:), allocatable, intent(inout) :: vals
        real(8), intent(in) :: nz_ratio
        integer :: new_size
        integer :: i, j, k, m, idx

        new_size = size(ms)
        
        ! init
        idx = 0
        ms = -1
        is = -1
        js = -1
        ks = -1
        vals(:) = 1d0

        do k=0, int(nz*nz_ratio)
            do j=0, ny
                do i=0, nx
                    do m=1, num_materials

                        if (idx > new_size-1) return

                        ! if (rand(0) <= nz_ratio) then
                        ms(idx) = m
                        is(idx) = i
                        js(idx) = j
                        ks(idx) = k
                        ! if(idx==0) then
                        !     write(*,*) 'aaaaaaaaaaa', ms(idx),is(idx),js(idx),ks(idx), idx
                        ! end if

                        idx = idx + 1

                            
                        ! end if 

                    end do
                end do
            end do
        end do

    end subroutine

end module