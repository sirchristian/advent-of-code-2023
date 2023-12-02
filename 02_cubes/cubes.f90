module cube_game
    implicit none

contains
    function convert_text_to_int(text) result (int)
        implicit none
        character(*),intent(in) :: text
        integer :: int
        integer :: err

        read(text,*,iostat=err) int
        if (err /= 0) then
            int = 0
        end if
    end function convert_text_to_int

    function check_color(sub_game_text, color, max_color) result (is_possible)
        implicit none

        ! params
        character(*), intent(in) :: sub_game_text
        character(*), intent(in) :: color
        integer, intent(in) :: max_color
        logical :: is_possible

        !local
        integer :: color_pos
        integer :: num_start_pos
        integer :: num_color

        is_possible = .true.

        color_pos = index(sub_game_text, color)
        if (color_pos > 0) then
            num_start_pos = scan(sub_game_text(:color_pos), ':,;', back = .true.)
            num_color = convert_text_to_int(trim(sub_game_text(num_start_pos+1:color_pos-1)))
            if (num_color > max_color) then
                is_possible = .false.
                return
            end if
        end if
    end function

    function sub_game_possible(sub_game_text, num_red, num_green, num_blue) result (is_possible)
        implicit none

        ! params
        character(*), intent(in) :: sub_game_text
        integer, intent(in) :: num_red, num_green, num_blue
        logical :: is_possible

        is_possible = check_color(sub_game_text, 'red', num_red)
        if (.not. is_possible) then
            print*, '  XX not enough red to play'
            return
        end if

        is_possible = check_color(sub_game_text, 'green', num_green)
        if (.not. is_possible) then
            print*, '  XX not enough green to play'
            return
        end if

        is_possible = check_color(sub_game_text, 'blue', num_blue)
        if (.not. is_possible) then
            print*, '  XX not enough blue to play'
            return
        end if

    end function sub_game_possible

    function game_id_if_possible(game_text, num_red, num_green, num_blue) result (id)
        implicit none

        ! params
        character(*), intent(in) :: game_text
        integer, intent(in) :: num_red, num_green, num_blue
        integer :: id

        !local 
        integer :: start_pos
        integer :: end_pos
        character(5) :: game_id_text
        character(1000) :: sub_game_text
        logical :: is_possible

        id = 0
        if (game_text(:4) /= 'Game') then
            print*, 'Invalid Game Input'
            return
        end if 

        print*, 'Playing '//game_text
        start_pos = scan(game_text, ':') 
        game_id_text = game_text(5:start_pos-1)

        end_pos = scan(game_text(start_pos:), ';')
        do while (end_pos > 0)
            sub_game_text = game_text(start_pos:end_pos+start_pos)
            is_possible = sub_game_possible(sub_game_text, num_red, num_green, num_blue)
            if (.not. is_possible) then
                return
            end if 
            start_pos = end_pos+start_pos
            end_pos = scan(game_text(start_pos:), ';')
        end do
        
        ! get the last subgame
        sub_game_text = game_text(start_pos:)
        is_possible = sub_game_possible(sub_game_text, num_red, num_green, num_blue)
        if (.not. is_possible) then
            return
        end if 

        print*, '   ... good game! Game ID '//trim(game_id_text)//' is possible'
        id = convert_text_to_int(game_id_text)
    end function game_id_if_possible

end module cube_game

program aoc_2023_02
    use cube_game
    implicit none

    character(2048) :: game_text
    integer :: io, err
    integer :: x, num_red, num_green, num_blue

    x = 0
    num_red = 12 
    num_green = 13 
    num_blue = 14

    io = 55 ! I think this can be anything? 
    open(newunit=io, file='test_input.txt', status='old', action='read')
    do
        read(io, '(A)', iostat=err) game_text
        if (err /= 0) exit
        x = x + game_id_if_possible(trim(game_text), num_red, num_green, num_blue)
    end do
    close(io)

    print*, ''
    print*, 'Sum of all possible games is....'
    print*, x
end program aoc_2023_02