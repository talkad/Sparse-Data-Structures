module sparse_matrix_module

    type :: coo_matrix_t
    private
        real, dimension(:), allocatable :: values
        integer, dimension(:), allocatable :: row
        integer, dimension(:), allocatable :: col

    contains

        procedure, public :: get
        procedure, private :: append
        procedure, private, nopass :: idx_exists
    end type

    interface csr_matrix_t
        module procedure create
    end interface


    contains


    ! type(csr_matrix_t) function create(matrix)
    !     implicit none
    !     real, dimension(:,:), intent(in) :: matrix
    !     integer :: i, j

    !     do j = 1, size(matrix, dim=2)
    !         do i = 1, size(matrix, dim=1)
    !             print*, matrix(i,j)
    !         end do
    !     end do
    ! end function

    type(coo_matrix_t) function create(matrix)
        implicit none
        real, dimension(:,:), intent(in) :: matrix
        integer :: i, j, idx = 1
        logical :: start_col = .False.
        integer :: mapping_length
        
        mapping_length = size(matrix, dim=1)*size(matrix, dim=2)
        allocate(create%row(mapping_length))
        allocate(create%col(mapping_length))

        do j = 1, size(matrix, dim=2)
            do i = 1, size(matrix, dim=1)
                if (matrix(i, j) /= 0) then
                    create%row(idx) = i
                    create%col(idx) = j
                    ! create%values(idx) = matrix(i, j)
                    call create%append(matrix(i, j), idx)
                    idx = idx + 1
                end if
            end do
        end do

        ! print*, 'num of none zeros', idx
        print*, 'data array ', create%values

        print*, 'row', create%row
        print*,  'col', create%col

    end function



    real function get(this, i, j)
        implicit none
        class(coo_matrix_t), intent(inout) :: this
        integer, intent(in) :: i, j
        integer :: val_idx

        val_idx = idx_exists(this%col, i, j)

        ! if (val_idx == -1) then
        !     get = 0
        ! else
        !     get = this%values(val_idx)
        ! end if
    end function



    ! interface operator(())
    !     module procedure my_paren
    ! end interface


    ! helper function - append val to array if it is not full, otherwise enlarge the array and append
    subroutine append(this, val, i)
        implicit none
        class(coo_matrix_t), intent(in out) :: this
        real, intent(in) :: val
        integer, intent(in) :: i
        integer :: prev_size
        real, dimension(:), allocatable :: temp

        prev_size = size(this%values, dim=1) 

        if (.not. allocated(this%values)) then
            allocate(this%values(2))
        else if (prev_size < i) then
            allocate(temp(prev_size * 2))         ! enlarge array size by factor of 2
            temp(:prev_size) = this%values        ! copy previous values
            call move_alloc(temp, this%values)    ! temp gets deallocated
        end if

        this%values(i) = val

    end subroutine


    integer function idx_exists(array, i, j)
        implicit none
        integer, dimension(:), allocatable :: array
        integer, intent(in) :: i, j
        integer :: min_limit, max_limit, curr_idx

        min_limit = 1
        max_limit = size(array, dim=1)

        do while (min_limit /= max_limit)

            curr_idx = (min_limit + max_limit) / 2

            if (array(curr_idx) == j) then
                idx_exists = curr_idx
                ! get backward
                ! do loop for finding the exact index
                return
            else if (array(curr_idx) < j) then
                min_limit = curr_idx
            else
                max_limit = curr_idx
            end if 

        end do 

        idx_exists = -1
    end function


    ! the subroutine is part of the module
    !     integer function idx_exists(array, i, j)
    !     implicit none
    !     integer, dimension(:), allocatable :: array
        
    !     ! class(coo_matrix_t), intent(inout) :: this
    !     integer, intent(in) :: i, j
    !     integer :: min_limit, max_limit, curr_idx

    !     min_limit = 1
    !     max_limit = size(this%col, dim=1)

    !     do while (min_limit /= max_limit)

    !         curr_idx = (min_limit + max_limit) / 2

    !         if (this%col(curr_idx) == j) then
    !             idx_exists = curr_idx
    !             ! get backward
    !             return
    !         else if (this%col(curr_idx) < j) then
    !             min_limit = curr_idx
    !         else
    !             max_limit = curr_idx
    !         end if 

    !     end do 

    !     idx_exists = -1
    ! end function



end module