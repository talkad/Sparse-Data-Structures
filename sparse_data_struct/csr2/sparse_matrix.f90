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


    function get_item(this, material_type, i, j)
        implicit none
        class(coo_matrix_t) :: this
        integer, intent(in) :: i, j, material_type
        real(8) :: get_item
        
        get_item = this%values(this%idx_map(material_type, i, j))
    end function get_item


    subroutine get_neighbors(this, material_type, i, j, curr_val, upper_val, lower_val, left_val, right_val)
        implicit none
        class(coo_matrix_t) :: this
        integer, intent(in) :: i, j, material_type
        real(8), intent(out) :: curr_val, upper_val, lower_val, left_val, right_val
        
        curr_val =  this%values(this%idx_map(material_type, i, j))
        upper_val = this%values(this%idx_map(material_type, i-1, j))
        lower_val = this%values(this%idx_map(material_type, i+1, j))
        left_val =  this%values(this%idx_map(material_type, i, j-1))
        right_val = this%values(this%idx_map(material_type, i, j+1))
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
        allocate(sparse_constructor%idx_map(0:materials,0:nx,0:ny))
        sparse_constructor%idx_map => indxs
        allocate(sparse_constructor%values(-1:14))

        idx = 0 
        sparse_constructor%idx_map(:,:,:) = -1
        sparse_constructor%values(-1) = 0d0

        do j = 0, ny
            do i = 0, nx
                do m = 0, materials
                    if (matrix(m, i, j) > EPSILON) then

                        sparse_constructor%idx_map(m, i, j) = idx
                        call sparse_constructor%append_real(sparse_constructor%values, matrix(m, i, j), idx)

                        idx = idx + 1
                    end if
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
            new_size = prev_size*2 - 2
                        ! print*, prev_size, new_size

            allocate(temp(-1:new_size))            ! enlarge array size by factor of 2
            temp(-1:new_size) = 0d0
            temp(-1:prev_size-2) = array(-1:prev_size-2)   ! copy previous values
            call move_alloc(temp, array)             ! temp gets deallocated
        end if

        ! print*, 'write into', idx, 'val', val
        array(idx) = val
    end subroutine

end module