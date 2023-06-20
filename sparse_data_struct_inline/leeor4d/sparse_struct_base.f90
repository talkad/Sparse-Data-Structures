module sparse_struct_base_module

    type, abstract :: sparse_struct_base_t

    contains
        procedure(add), public, deferred :: add_item
        procedure(get), public, deferred :: get_item
        procedure(total_sum), public, deferred :: total_sum
        procedure(update_data), public, deferred :: update_struct
    end type

    abstract interface

        pure function get(this, material_type, i, j, k)
            import sparse_struct_base_t
            class(sparse_struct_base_t), intent(in) :: this
            integer, intent(in) :: i, j, k, material_type
            real(8) :: get
        end function

        subroutine add(this, material_type, i, j, k, val)
            import sparse_struct_base_t
            class(sparse_struct_base_t), intent(inout) :: this
            integer, intent(in) :: i, j, k, material_type
            real(8) :: val
        end subroutine

        subroutine update_data(this, ms, is, js, ks, vals)
            import sparse_struct_base_t
            class(sparse_struct_base_t), intent(inout) :: this
            integer, dimension(:), allocatable, intent(in) :: is, js, ks, ms
            real(8), dimension(:), allocatable, intent(in) :: vals
        end subroutine

        function total_sum(this)
            import sparse_struct_base_t
            class(sparse_struct_base_t), intent(inout) :: this
            real(8) :: total_sum
        end function

    end interface


end module
