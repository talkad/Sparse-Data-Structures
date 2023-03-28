module sparse_matrix_module
    use data_abstract_module

    type, extends(data_abstract_t) :: coo_matrix_t
    ! private
        real(8), dimension(:), allocatable :: values
        integer, dimension(:,:,:), pointer :: idx_map

    contains

        procedure :: get_item => get_item
        procedure :: add_item => add_item
        procedure :: get_neighbors => get_neighbors
        procedure :: update_struct => update_struct

        procedure, private, nopass :: append_real
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
        integer :: m, i, j, mater, nx, ny, insertion_idx, idx
        real(8), dimension(:), allocatable :: old_values
        logical :: used_cell

        used_cell = .False.
        materials = size(matrix, dim=1)-1
        nx = size(matrix, dim=2)-1
        ny = size(matrix, dim=3)-1

        idx = 1
        insertion_idx = 0

        allocate(old_values(0:size(this%values)-1))
        old_values = this%values

        do j=0, ny
            do i=0, nx
                do m=0, materials

                    if (ms(idx)=m .and. is(idx)==i .and. js(idx)=j) then
                        call this%append_real(this%values, vals(idx), insertion_idx)
                        this%idx_map(m,i,j) = insertion_idx
                        insertion_idx = insertion_idx + 1
                        idx = idx + 1
                        used_cell = .True.
                    else if (this%idx_map(m,i,j) > -1) then
                        call this%append_real(this%values, old_values(this%idx_map(m,i,j)), insertion_idx)
                        this%idx_map(m,i,j) = insertion_idx
                        insertion_idx = insertion_idx + 1
                    end if 
                    
                end do 
                
                ! add padding only to an updated cell (i,j) 
                if (used_cell) then
                    call this%append_real(this%values, vals(idx), insertion_idx)
                    this%idx_map(m,i,j) = insertion_idx
                    call this%append_real(this%values, vals(idx), insertion_idx+1)
                    this%idx_map(m,i,j) = insertion_idx+1
                    insertion_idx = insertion_idx + 2
                    idx = idx + 2
                    used_cell = .False.
                end if
            end do
        end do
        
        deallocate(old_values)

    end subroutine


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

    function sparse_constructor(matrix, indxs)
        implicit none
        type(coo_matrix_t), pointer :: sparse_constructor
        real(8), dimension(:,:,:), intent(inout), allocatable :: matrix
        integer, dimension(:,:,:), allocatable, target, intent(inout) :: indxs
        integer :: i, j, m, idx
        logical :: start_col = .False.
        integer :: mapping_length, nx, ny, materials
        real(8), parameter :: EPSILON = 0.00001
        integer :: col_init_val

        materials = size(matrix, dim=1)-1
        nx = size(matrix, dim=2)-1
        ny = size(matrix, dim=3)-1

        allocate(sparse_constructor)
        ! allocate(sparse_constructor%idx_map(0:materials,0:nx,0:ny))
        sparse_constructor%idx_map => indxs
        allocate(sparse_constructor%values(0:2047))

        sparse_constructor%idx_map(:,:,:) = -1
        idx = 0 

        do j = 0, ny
            do i = 0, nx
                do m = 0, materials
                    if (matrix(m, i, j) > EPSILON) then
                        ! print*, j,i,m

                        sparse_constructor%idx_map(m, i, j) = idx
                        call sparse_constructor%append_real(sparse_constructor%values, matrix(m, i, j), idx)

                        idx = idx + 1
                    end if

                    ! padding of 2 
                    call sparse_constructor%append_real(sparse_constructor%values, 0, idx)
                    call sparse_constructor%append_real(sparse_constructor%values, 0, idx+1)
                    idx = idx + 2
                end do
            end do
        end do

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

            allocate(temp(0:new_size))            ! enlarge array size by factor of 2
            temp(0:new_size) = 0d0
            temp(0:prev_size-1) = array(0:prev_size-1)   ! copy previous values
            call move_alloc(temp, array)             ! temp gets deallocated
        end if

        ! print*, 'write into', idx, 'val', val
        array(idx) = val
    end subroutine

end module