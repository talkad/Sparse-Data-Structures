module sparse_matrix_module
    use data_abstract_module

    type, extends(data_abstract_t) :: coo_matrix_t
    ! private
        real(8), dimension(:), allocatable :: values
        integer, dimension(:,:,:), pointer :: idx_map
        integer :: padding_size

        integer :: padding_idx
        integer :: block_size

    contains

        procedure :: get_item => get_item
        procedure :: add_item => add_item
        procedure :: get_neighbors => get_neighbors
        procedure :: update_struct => update_struct

        procedure, private, nopass :: append_real
        procedure, private :: count_new_cells
    end type

    interface coo_matrix_t
        module procedure sparse_constructor
    end interface


    contains

    subroutine add_item(this, material_type, i, j, val)
        class(coo_matrix_t) :: this
        integer, intent(in) :: i, j, material_type
        real(8) :: val

        ! irrelevant
    end subroutine

    ! Assume the order of the new values is j i m
    subroutine update_struct(this, ms, is, js, vals)
        class(coo_matrix_t) :: this
        integer, dimension(:), allocatable, intent(in) :: is, js, ms
        real(8), dimension(:), allocatable, intent(in) :: vals
        integer :: m, i, j, materials, nx, ny, insertion_idx, idx, num_vals, jj, ii
        integer :: num_padding, new_cells, num_pads, prev_size 
        real(8), dimension(:), allocatable :: temp
        integer :: current_idx, count

        count = 0
        num_padding = size(this%values) - this%padding_idx
        new_cells = this%count_new_cells(ms,is,js)

        if (new_cells > num_padding) then
            ! rescalse the size of the values array 
            num_pads = (new_cells - num_padding) / this%padding_size + 1 

            prev_size = size(this%values, dim=1)

            new_size = prev_size + num_pads * this%padding_size - 1
            allocate(temp(0:new_size))                         ! enlarge array size by factor of 2
            temp(0:new_size) = 0d0
            temp(0:prev_size-1) = this%values(0:prev_size-1)   ! copy previous values
            call move_alloc(temp, this%values)                 ! temp gets deallocated
        end if

        materials = size(this%idx_map, dim=1)-1
        nx = size(this%idx_map, dim=2)-1
        ny = size(this%idx_map, dim=3)-1
        num_vals = size(is)

        idx = num_vals
        insertion_idx = size(this%values)-1

        do j=ny, 0
            do i=nx, 0
                do m=materials, 0

                    current_idx = this%idx_map(m,i,j)

                    if (idx >= 0 .and. ms(idx)==m .and. is(idx)==i .and. js(idx)==j) then
                        this%values(insertion_idx) = vals(idx)
                        this%idx_map(m,i,j) = insertion_idx
                        insertion_idx = insertion_idx - 1
                        idx = idx - 1
                    else if (current_idx > -1) then
                        this%values(insertion_idx) = this%values(current_idx) 
                        this%idx_map(m,i,j) = insertion_idx
                        insertion_idx = insertion_idx - 1
                    end if 
            
                end do
            end do
        end do
        
        this%padding_idx = insertion_idx

        idx = 0
        ! align values to left
        do j=0, ny, this%block_size
            do i=0, nx, this%block_size

                do jj=j, max(j+this%block_size-1, ny)
                    do ii=i, max(i+this%block_size-1, nx)

                        do m=0, materials

                            current_idx = this%idx_map(m,ii,jj) 

                            if (current_idx > -1) then
                                this%values(idx) = this%values(current_idx) 
                                this%idx_map(m,ii,jj) = idx
                                idx = idx + 1
                            end if

                        end do

                    end do
                end do

            end do
        end do

    end subroutine


    function count_new_cells(this, ms, is, js) result(num)
        implicit none
        class(coo_matrix_t) :: this
        integer, dimension(:), allocatable, intent(in) :: is, js, ms
        integer :: num, idx

        num = 0

        do idx=0, size(is)-1
            if (this%idx_map(ms(idx), is(idx), js(idx)) > -1) num=num+1
        end do

    end function


    function get_item(this, material_type, i, j)
        implicit none
        class(coo_matrix_t) :: this
        integer, intent(in) :: i, j, material_type
        real(8) :: get_item
        integer :: idx
        get_item = 0d0
        idx = this%idx_map(material_type, i, j)
        
        if (idx > -1) get_item = this%values(idx)
    end function get_item


    subroutine get_neighbors(this, material_type, i, j, curr_val, upper_val, lower_val, left_val, right_val)
        implicit none
        class(coo_matrix_t) :: this
        integer, intent(in) :: i, j, material_type
        real(8), intent(out) :: curr_val, upper_val, lower_val, left_val, right_val
        integer :: idx_curr, idx_upper, idx_lower, idx_left, idx_right

        curr_val =  0d0
        upper_val = 0d0
        lower_val = 0d0
        left_val =  0d0
        right_val = 0d0

        idx_curr= this%idx_map(material_type, i, j)
        idx_upper=this%idx_map(material_type, i-1, j)
        idx_lower=this%idx_map(material_type, i+1, j)
        idx_left= this%idx_map(material_type, i, j-1)
        idx_right=this%idx_map(material_type, i, j+1)
        
        if (idx_curr > -1) curr_val = this%values(idx_curr)
        if (idx_upper > -1) upper_val = this%values(idx_upper)
        if (idx_lower > -1) lower_val = this%values(idx_lower)
        if (idx_left > -1) left_val = this%values(idx_left)
        if (idx_right > -1) right_val = this%values(idx_right)

    end subroutine

    function sparse_constructor(matrix, indxs, block_size)
        implicit none
        type(coo_matrix_t), pointer :: sparse_constructor
        real(8), dimension(:,:,:), intent(inout), allocatable :: matrix
        integer, dimension(:,:,:), allocatable, target, intent(inout) :: indxs
        integer, intent(in) :: block_size
        integer :: i, j, m, idx, ii, jj
        logical :: start_col = .False.
        integer :: mapping_length, nx, ny, materials
        real(8), parameter :: EPSILON = 0.00001
        integer :: col_init_val

        materials = size(matrix, dim=1)-1
        nx = size(matrix, dim=2)-1
        ny = size(matrix, dim=3)-1

        allocate(sparse_constructor)
        sparse_constructor%idx_map => indxs
        allocate(sparse_constructor%values(0:2047))
        sparse_constructor%block_size = block_size

        sparse_constructor%idx_map(:,:,:) = -1
        idx = 0 
        sparse_constructor%padding_size = 0.1*nx*ny

        do j = 0, ny, block_size
            do i = 0, nx, block_size

                do jj=j, min(j+block_size-1, ny)
                    do ii=i, min(i+block_size-1, nx)

                        do m = 0, materials
                            if (matrix(m, i, j) > EPSILON) then

                                sparse_constructor%idx_map(m, i, j) = idx
                                call sparse_constructor%append_real(sparse_constructor%values, matrix(m, i, j), idx)

                                idx = idx + 1
                            end if
                        end do

                    end do
                end do
            end do
        end do

        sparse_constructor%padding_idx = idx

    end function


    ! helper function - append val to array if it is not full, otherwise enlarge the array and append
    subroutine append_real(array, val, idx)
        implicit none
        real(8), dimension(:), allocatable, intent(inout) :: array
        real(8), intent(in) :: val
        integer, intent(in) :: idx

        integer :: prev_size, new_size
        real(8), dimension(:), allocatable :: temp

        prev_size = size(array, dim=1)

        if (prev_size <= idx) then
            new_size = prev_size*2 - 1
            ! print*, prev_size, new_size

            allocate(temp(0:new_size))                   ! enlarge array size by factor of 2
            temp(0:new_size) = 0d0
            temp(0:prev_size-1) = array(0:prev_size-1)   ! copy previous values
            call move_alloc(temp, array)                 ! temp gets deallocated
        end if

        ! print*, 'write into', idx, 'val', val
        array(idx) = val
    end subroutine

end module