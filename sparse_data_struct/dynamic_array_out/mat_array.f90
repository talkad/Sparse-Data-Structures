module mat_array_module
    use data_abstract_module
    use dynamic_array_module

    type :: mat_array_t 
        type(dynamic_array_t), dimension(:,:), allocatable :: mat

        contains

            procedure :: get_item => get_item
            procedure :: add_item => add_item
            procedure :: get_neighbors => get_neighbors

    end type

    interface mat_array_t
        module procedure mat_array_constructor
    end interface

    contains

    function mat_array_constructor(matrix, idx_map)
        implicit none
        type(mat_array_t), pointer :: mat_array_constructor
        real(8), dimension(:,:,:), intent(inout), allocatable :: matrix
        integer, dimension(:,:,:), allocatable, target, intent(inout) :: idx_map
        integer :: i, j, m, nx, ny, materials
        integer :: idx
        
        idx = 0

        materials = size(matrix, dim=1)-1
        nx = size(matrix, dim=2)-1
        ny = size(matrix, dim=3)-1

        ! print*, ny, nx, materials

        allocate(mat_array_constructor)
        allocate(mat_array_constructor%mat(0:nx,0:ny))
        idx_map(:,:,:) = -1

        do j = 0, ny
            do i = 0, nx
                mat_array_constructor%mat(i,j) = dynamic_array_constructor()

                do m=0, materials
                    if (matrix(m,i,j) /= 0) then 
                        idx_map(m,i,j) = idx
                        call append_real(mat_array_constructor%mat(i,j)%values, matrix(m,i,j), idx)
                        idx = idx+1
                    end if
                end do
                idx = 0
            end do
        end do

    end function


    subroutine add_item(this, material_type, i, j, val)
        class(mat_array_t) :: this
        integer, intent(in) :: i, j, material_type
        real(8) :: val

        ! irrelevant
    end subroutine


    function get_item(this, material_type, i, j, idx_map)
        implicit none
        class(mat_array_t) :: this
        integer, intent(in) :: i, j, material_type
        integer, dimension(:,:,:), allocatable, intent(inout) :: idx_map
        real(8) :: get_item
        integer:: idx
        
        get_item = this%mat(i,j)%get(idx_map(material_type,i,j))
    end function get_item


    subroutine get_neighbors(this, material_type, i, j, curr_val, upper_val, lower_val, left_val, right_val, idx_map)
        implicit none
        class(mat_array_t) :: this
        integer, intent(in) :: i, j, material_type
        real(8), intent(out) :: curr_val, upper_val, lower_val, left_val, right_val
        integer, dimension(:,:,:), allocatable, intent(inout) :: idx_map

        curr_val =  this%mat(i,j)%get(idx_map(material_type,i,j))
        upper_val = this%mat(i-1,j)%get(idx_map(material_type,i-1,j))
        lower_val = this%mat(i+1,j)%get(idx_map(material_type,i+1,j))
        left_val =  this%mat(i,j-1)%get(idx_map(material_type,i,j-1))
        right_val = this%mat(i,j+1)%get(idx_map(material_type,i,j+1))

    end subroutine

end module