module csr_module
    use sparse_struct_base_module

    type, extends(sparse_struct_base_t) :: csr_t
    ! private
        real(8), dimension(:), allocatable :: values
        integer, dimension(:,:,:,:), pointer :: idx_map
        integer :: padding_size
        integer :: padding_idx

    contains

        procedure :: get_item => get_item
        procedure :: add_item => add_item
        procedure :: update_struct => update_struct

        procedure, private, nopass :: append_real
        procedure, private :: count_new_cells
        ! procedure, private :: align_value_left
    end type

    interface csr_t
        module procedure sparse_constructor
    end interface


    contains

    subroutine add_item(this, material_type, i, j, k, val)
        implicit none
        class(csr_t), intent(inout) :: this
        integer, intent(in) :: i, j, k, material_type
        real(8) :: val

        ! irrelevant
    end subroutine


    ! Assume the order of the new values is j i m
    subroutine update_struct(this, ms, is, js, ks, vals)
        implicit none
        class(csr_t), intent(inout) :: this
        integer, dimension(:), allocatable, intent(in) :: is, js, ks, ms
        real(8), dimension(:), allocatable, intent(in) :: vals
        integer :: m, i, j, k, materials, nx, ny, nz, insertion_idx, idx, num_vals
        integer :: num_padding, new_cells, num_pads, prev_size, new_size
        real(8), dimension(:), allocatable :: temp
        integer :: current_idx, debug1, debug2, debug3
        logical :: logic_debug
        integer, dimension(0:3) :: idx_ptr = [0, 0, 0, 0], cont_idx = [0, 0, 0, 0]
        real(8) :: val

        idx = 0
        ! write(*,*) 'aaaaaaaaaaa', idx, ms(idx),is(idx),js(idx),ks(idx)

        debug1 = 0
        debug2 = 0
        debug3 = 0
        logic_debug = .False.
        num_padding = size(this%values) - this%padding_idx
        new_cells = this%count_new_cells(ms,is,js,ks)

        ! write(*,*), 'initial stats', new_cells, num_padding, sum(vals)

        if (new_cells > num_padding) then
            ! rescalse the size of the values array 
            num_pads = (new_cells - num_padding) / this%padding_size + 1 

            prev_size = size(this%values, dim=1)

            new_size = prev_size + num_pads * this%padding_size - 1
            ! write(*,*), 'reallocation', prev_size, '->', new_size

            allocate(temp(0:new_size))                          ! enlarge array size by factor of 2
            temp(0:new_size) = 0d0
            temp(0:prev_size-1) = this%values(0:prev_size-1)    ! copy previous values
            call move_alloc(temp, this%values)                  ! temp gets deallocated
        end if

        materials = size(this%idx_map, dim=1)
        nx = size(this%idx_map, dim=2)-1
        ny = size(this%idx_map, dim=3)-1
        nz = size(this%idx_map, dim=4)-1
        num_vals = size(is)-1

        idx = num_vals
        insertion_idx = size(this%values)-1

        ! write(*,*), 'before insertion', sum(this%values)
        debug1 = 0
        debug2 = 0
        debug3 = 0

        do k=nz, 0, -1
            do j=ny, 0, -1
                do i=nx, 0, -1
                    do m=materials, 1, -1

                        current_idx = this%idx_map(m,i,j,k)

                        ! if (this%values(insertion_idx) /= 0) then 
                        !     call this%align_value_left()
                        ! end if

                        if (ms(idx)==m .and. is(idx)==i .and. js(idx)==j .and. ks(idx)==k) then
                            this%values(insertion_idx) = vals(idx)
                            this%idx_map(m,i,j,k) = insertion_idx
                            insertion_idx = insertion_idx - 1
                            idx = idx - 1
                        else if (current_idx > -1) then
                            this%values(insertion_idx) = this%values(current_idx)
                            this%values(current_idx) = 0
                            this%idx_map(m,i,j,k) = insertion_idx
                            insertion_idx = insertion_idx - 1
                        end if 

                    end do
                end do
            end do
        end do 

        ! align values to left
        idx = 0

        do k=0, nz
            do j=0, ny
                do i=0, nx
                    do m=1, materials

                        current_idx = this%idx_map(m,i,j,k) 

                        if (current_idx > -1) then
                            this%values(idx) = this%values(current_idx) 
                            this%values(current_idx) = 0
                            this%idx_map(m,i,j,k) = idx
                            idx = idx + 1
                        end if

                    end do
                end do
            end do
        end do

        this%padding_idx = idx
        ! write(*,*), 'after alignment', sum(this%values)
        ! write(*,*), 'value distruction', logic_debug

    end subroutine


    function count_new_cells(this, ms, is, js, ks) result(num)
        implicit none
        class(csr_t), intent(inout) :: this
        integer, dimension(:), allocatable, intent(in) :: is, js, ms, ks
        integer :: num, idx

        num = 0

        do idx=0, size(is)-1
            if (this%idx_map(ms(idx), is(idx), js(idx), ks(idx)) == -1) num=num+1
        end do
    end function


    function get_item(this, material_type, i, j, k)
        implicit none
        class(csr_t), intent(inout) :: this
        integer, intent(in) :: i, j, k, material_type
        real(8) :: get_item
        integer :: idx
        get_item = 0d0
        idx = this%idx_map(material_type, i, j, k)
        
        if (idx > -1) get_item = this%values(idx)
    end function get_item


    function sparse_constructor(indxs)
        type(csr_t), pointer :: sparse_constructor
        integer, dimension(:,:,:,:), allocatable, target, intent(inout) :: indxs
        integer, parameter :: ratio = 0.1
        real(8), parameter :: mix_ratio = 0.01
        integer :: space_size, initial_size

        space_size = size(indxs, dim=2)*size(indxs, dim=3)*size(indxs, dim=4)
        initial_size = space_size + ratio * space_size

        allocate(sparse_constructor)
        sparse_constructor%idx_map => indxs
        allocate(sparse_constructor%values(0:initial_size))

        sparse_constructor%values(0:initial_size) = 0d0
        ! sparse_constructor%idx_map(:,:,:,:) = -1
        sparse_constructor%padding_size = space_size*mix_ratio
        sparse_constructor%padding_idx = 0
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

            allocate(temp(0:new_size))                   ! enlarge array size by factor of 2
            temp(0:new_size) = 0d0
            temp(0:prev_size-1) = array(0:prev_size-1)   ! copy previous values
            call move_alloc(temp, array)                 ! temp gets deallocated
        end if

        array(idx) = val
    end subroutine

end module