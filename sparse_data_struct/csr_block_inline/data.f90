module data_abstract_module

    type, abstract :: data_abstract_t

    contains
        procedure(add), public, deferred :: add_item
        procedure(get), public, deferred :: get_item
        procedure(get_neighborhood), public, deferred :: get_neighbors
    end type

    abstract interface
        function get(this, material_type, i, j)
            import data_abstract_t
            class(data_abstract_t) :: this
            integer, intent(in) :: i, j, material_type
            real(8) :: get
        end function

        subroutine add(this, material_type, i, j, val)
            import data_abstract_t
            class(data_abstract_t) :: this
            integer, intent(in) :: i, j, material_type
            real(8) :: val
        end subroutine

        subroutine get_neighborhood(this, material_type, i, j, curr_val, upper_val, lower_val, left_val, right_val)
            import data_abstract_t
            class(data_abstract_t) :: this
            integer, intent(in) :: i, j, material_type
            real(8), intent(out) :: curr_val, upper_val, lower_val, left_val, right_val
        end subroutine

    end interface


end module