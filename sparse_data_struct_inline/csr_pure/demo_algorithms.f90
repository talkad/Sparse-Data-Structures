module demo_algorithms_module
    use csr_module


    contains


    subroutine intensive_algorithm(A, B, C, nx, ny, nz, num_materials)
        implicit none
        type(csr_t), pointer, intent(in) :: A, B
        type(csr_t), pointer, intent(out) :: C
        integer, intent(in) :: nx,ny,nz,num_materials
        integer :: i, j, k, m, total_size, idx
        real(8) :: a_val, b_val

        real(8) :: temp_sum = 0, res

        total_size = nx*ny*nz*num_materials

        ! order k->j->i->m
        do k = 0, nz
            do j = 0, ny
                do i = 0, nx
                    do m = 1, num_materials

                            a_val = A%get_item(m, i, j, k)
                            b_val = B%get_item(m, i, j, k)

                            temp_sum = temp_sum + a_val
                            res = a_val*a_val*1.2 + 0.5*b_val

                            idx = C%idx_map(m, i, j, k)
                            if (idx > -1) C%values(idx) = res  
                            
                    end do
                end do
            end do
        end do

        print*, 'Result intensive_algorithm', temp_sum
    end subroutine


    subroutine intensive_algorithm_mat(A, B, C, nx, ny, nz, num_materials)
        implicit none
        type(csr_t), pointer, intent(in) :: A, B
        type(csr_t), pointer, intent(out) :: C
        integer, intent(in) :: nx,ny,nz,num_materials
        integer :: i, j, k, m, total_size, idx
        real(8) :: a_val, b_val
        real(8) :: temp_sum = 0, res

        ! order m->k->j->i
        do m = 1, num_materials
            do k = 0, nz
                do j = 0, ny
                    do i = 0, nx

                        a_val = A%get_item(m, i, j, k)
                        b_val = B%get_item(m, i, j, k)

                        temp_sum = temp_sum + a_val
                        res = a_val*a_val*1.2 + 0.5*b_val

                        idx = C%idx_map(m, i, j, k)
                        if (idx > -1) C%values(idx) = res  

                    end do
                end do
            end do
        end do

        print*, 'Result intensive_algorithm_mat', temp_sum
    end subroutine


    subroutine intensive_algorithm_neighbors(A, C, nx, ny, nz, num_materials)
        implicit none
        type(csr_t), pointer, intent(in) :: A
        type(csr_t), pointer, intent(out) :: C
        integer, intent(in) :: nx,ny,nz,num_materials
        integer :: i, j, k, m, idx
        real(8) :: curr, up, down, left, right, bottom, top

        real(8) :: temp_sum = 0, res

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
                        temp_sum = temp_sum + curr

                        idx = C%idx_map(m, i, j, k)
                        if (idx > -1) C%values(idx) = res

                    end do
                end do
            end do
        end do

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