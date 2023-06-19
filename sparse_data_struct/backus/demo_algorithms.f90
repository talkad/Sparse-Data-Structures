module demo_algorithms_module


    contains


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