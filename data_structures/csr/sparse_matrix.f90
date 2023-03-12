module sparse_matrix_module
    use data_abstract_module

    type, extends(data_abstract_t) :: coo_matrix_t
    private
        real(8), dimension(:), allocatable :: values
        integer, dimension(:), allocatable :: row
        integer, dimension(:), allocatable :: col
        integer, dimension(:), allocatable :: materials
        integer :: upper_bound

    contains

        procedure :: get_item => get_item
        procedure :: add_item => add_item


        procedure, private, nopass :: append_real
        procedure, private, nopass :: append_int
        procedure, private, nopass :: idx_exists
        procedure, private, nopass :: get_boundries
    end type

    interface coo_matrix_t
        module procedure sparse_constructor
    end interface


    contains

    subroutine add_item(this, material_type, i, j, val)
        class(coo_matrix_t) :: this
        integer, intent(in) :: i, j, material_type
        real(8) :: val

        ! maybe someday

    end subroutine


    function get_item(this, material_type, i, j)
        class(coo_matrix_t) :: this
        integer, intent(in) :: i, j, material_type
        real(8) :: get_item
        integer :: val_idx, range_min, range_max
    
        val_idx = idx_exists(this%col, j, 0, this%upper_bound)  

        if (val_idx == -1) then
            get_item = 0
            return
        end if

        call get_boundries(range_min, range_max, this%col, val_idx)
        ! write(*,*)  'aaaaaaaaa', range_min, '-' , range_max

        val_idx = idx_exists(this%row, i, range_min, range_max)
        if (val_idx == -1) then
            get_item = 0
            return
        end if
        call get_boundries(range_min, range_max, this%row, val_idx)

        val_idx = idx_exists(this%materials, material_type, range_min, range_max)
        if (val_idx == -1) then
            get_item = 0
            return
        end if

        get_item = this%values(val_idx)

    end function get_item


    function sparse_constructor(matrix)
        implicit none
        type(coo_matrix_t), pointer :: sparse_constructor
        real(8), dimension(:,:,:), intent(in) :: matrix
        integer :: i, j, m, idx
        logical :: start_col = .False.
        integer :: mapping_length, nx, ny, materials
        real(8), parameter :: EPSILON = 0.00001
        
        allocate(sparse_constructor)
        idx = 0 
        
        do j = 1, size(matrix, dim=3)
            do i = 1, size(matrix, dim=2)
                do m = 1, size(matrix, dim=1)
                    if (matrix(m, i, j) > EPSILON) then
                        call sparse_constructor%append_real(sparse_constructor%values, matrix(m, i, j), idx)
                        call sparse_constructor%append_int(sparse_constructor%row, i-1, idx)
                        call sparse_constructor%append_int(sparse_constructor%col, j-1, idx)
                        call sparse_constructor%append_int(sparse_constructor%materials, m-1, idx)

                        idx = idx + 1
                    end if
                end do
            end do
        end do

        sparse_constructor%upper_bound = idx - 1

      
        ! print*, 'col', sparse_constructor%col
        ! print*, 'row', sparse_constructor%row
        ! print*, 'materials', sparse_constructor%materials


    end function



    ! helper function - append val to array if it is not full, otherwise enlarge the array and append
    subroutine append_real(array, val, idx)
        implicit none
        real(8), dimension(:), allocatable, intent(inout) :: array
        real(8), intent(in) :: val
        integer, intent(in) :: idx

        integer :: prev_size
        real(8), dimension(:), allocatable :: temp

        if (allocated(array)) then
            prev_size = size(array, dim=1)
        end if

        if (.not. allocated(array)) then
            allocate(array(0:16))
        else if (prev_size <= idx) then
            allocate(temp(0:prev_size*2))            ! enlarge array size by factor of 2
            temp(0:prev_size*2) = 0d0
            temp(0:prev_size) = array(0:prev_size)                ! copy previous values
            call move_alloc(temp, array)             ! temp gets deallocated

            ! allocate(temp(0:prev_size))            ! enlarge array size by factor of 2
            ! temp(0:prev_size) = array
            ! deallocate(array)
            ! allocate(array(0:prev_size*2))
            ! array = temp
            ! deallocate(temp)
        end if

        ! print*, allocated(array), size(array, dim=1), idx
        ! print*, 'a', idx, val
        array(idx) = val
        ! print*, array
    end subroutine


    subroutine append_int(array, val, idx)
        ! temporal solution
        implicit none
        integer, dimension(:), allocatable, intent(inout) :: array
        integer, intent(in) :: val
        integer, intent(in) :: idx

        integer :: prev_size
        integer, dimension(:), allocatable :: temp

        if (allocated(array)) then
            prev_size = size(array, dim=1)
        end if

        if (.not. allocated(array)) then
            allocate(array(0:16))
        else if (prev_size <= idx) then
            allocate(temp(0:prev_size*2))            ! enlarge array size by factor of 2
            temp(0:prev_size*2) = 0d0
            temp(0:prev_size) = array(0:prev_size)   ! copy previous values
            call move_alloc(temp, array)             ! temp gets deallocated
        end if

        array(idx) = val
    end subroutine


    integer function idx_exists(array, i, lower, upper)
        implicit none
        integer, dimension(:), allocatable :: array
        integer, intent(in) :: i
        integer :: min_limit, max_limit, mid_idx, range
        integer :: lower, upper

        min_limit = lower
        max_limit = upper

        ! if (present(lower)) min_limit = lower
        ! if (present(upper)) max_limit = upper

        ! write(*,*) 'tryna find', i
        ! write(*,*) 'find', lower, upper, array(lower), array(upper)

        range = max_limit - min_limit
        mid_idx = (min_limit + max_limit) / 2
        ! write(*,*) 'eeeeeeeeeee', mid_idx


        do while (array(mid_idx) /= i .and. range>0)            
            ! print*, min_limit, max_limit, mid_idx
            ! print*, array(mid_idx), i

            if (array(mid_idx) < i) then
                min_limit = mid_idx + 1
            else
                max_limit = mid_idx - 1
            end if

            range = max_limit - min_limit
            mid_idx = (min_limit + max_limit) / 2
        end do 

        if (array(mid_idx) /= i) then
            idx_exists = -1
        else 
            idx_exists = mid_idx
        end if 
        
    end function


    subroutine get_boundries(lower, upper, array, idx)
        implicit none
        integer, dimension(:), allocatable :: array
        integer, intent(in) :: idx
        integer, intent(inout) :: lower, upper
        integer :: val, i

        val = array(idx)
        ! write(*,*) 'bbbbbbb', idx, val, array(idx)

        do i = idx, 0, -1
            if (array(i) /= val) exit
            lower = i
        end do

        do i = idx, size(array, dim=1)
            if (array(i) /= val) exit
            upper = i
        end do

        ! write(*,*) 'done'


    end subroutine


end module